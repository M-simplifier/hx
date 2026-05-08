module Hx.Command.Add (runAdd) where

import Control.Exception (evaluate)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (find, intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import Hx.Json (jsonArray, jsonBool, jsonObject, jsonString)
import Hx.Project
    ( ComponentInfo (..)
    , ComponentType (..)
    , PackageInfo (..)
    , ProjectSnapshot (..)
    , componentTargetLabel
    , discoverProject
    )
import System.Exit (ExitCode, die, exitWith)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (rawSystem)

data AddInvocation = AddInvocation
    { addPackages :: [String]
    , addTarget :: Maybe String
    , addPlanOnly :: Bool
    , addJson :: Bool
    , addVerify :: Bool
    }

data AddPlan = AddPlan
    { planPackageFile :: FilePath
    , planTarget :: ComponentInfo
    , planAlreadyPresent :: [String]
    , planToAdd :: [String]
    , planVerifyCommand :: [String]
    }

runAdd :: [String] -> IO ()
runAdd args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn addUsage
        else
            case parseAddInvocation args of
                Left message -> die message
                Right invocation -> do
                    snapshot <- discoverProject
                    case makeAddPlan snapshot invocation of
                        Left message -> die message
                        Right plan -> do
                            if addJson invocation
                                then putStrLn (addPlanToJson invocation plan)
                                else putStr (renderAddPlan invocation plan)
                            if addPlanOnly invocation || null (planToAdd plan)
                                then pure ()
                                else do
                                    applyAddPlan snapshot plan
                                    putStrLn "Applied dependency update."
                                    if addVerify invocation
                                        then do
                                            putStrLn ("Evidence: " <> intercalate " " (planVerifyCommand plan))
                                            hFlush stdout
                                            exitCode <- runVerification (planVerifyCommand plan)
                                            exitWith exitCode
                                        else putStrLn "Evidence: verification skipped by --no-verify"

parseAddInvocation :: [String] -> Either String AddInvocation
parseAddInvocation rawArgs =
    let packageArgs = filter (not . isHxFlag) rawArgs
     in if null packageArgs
            then Left ("Missing dependency name.\n\n" <> addUsage)
            else
                Right
                    AddInvocation
                        { addPackages = nub (map normalizeDependencyName packageArgs)
                        , addTarget = parseTarget rawArgs
                        , addPlanOnly = "--plan" `elem` rawArgs
                        , addJson = "--json" `elem` rawArgs
                        , addVerify = not ("--no-verify" `elem` rawArgs)
                        }

parseTarget :: [String] -> Maybe String
parseTarget args =
    case [drop (length "--target=") arg | arg <- args, "--target=" `isPrefixOf` arg] of
        target : _ -> Just target
        [] -> Nothing

isHxFlag :: String -> Bool
isHxFlag arg =
    arg `elem` ["--plan", "--apply", "--json", "--no-verify"]
        || "--target=" `isPrefixOf` arg

makeAddPlan :: ProjectSnapshot -> AddInvocation -> Either String AddPlan
makeAddPlan snapshot invocation = do
    (packageInfo, component) <- selectTarget snapshot (addTarget invocation)
    let existing = componentBuildDepends component
        requested = addPackages invocation
        toAdd = [dependency | dependency <- requested, dependency `notElem` existing]
        already = [dependency | dependency <- requested, dependency `elem` existing]
    pure
        AddPlan
            { planPackageFile = packageFile packageInfo
            , planTarget = component
            , planAlreadyPresent = already
            , planToAdd = toAdd
            , planVerifyCommand = ["cabal", "build", "all"]
            }

selectTarget :: ProjectSnapshot -> Maybe String -> Either String (PackageInfo, ComponentInfo)
selectTarget snapshot maybeTarget =
    case maybeTarget of
        Just targetName ->
            case filter (matchesTarget targetName . snd) allComponents of
                [match] -> Right match
                [] ->
                    Left
                        ( "No component matched --target="
                            <> targetName
                            <> ". Available targets: "
                            <> renderAvailableTargets allComponents
                        )
                _ ->
                    Left
                        ( "Target is ambiguous: "
                            <> targetName
                            <> ". Use a more specific target."
                        )
        Nothing ->
            case projectPackages snapshot of
                [packageInfo] -> chooseDefaultComponent packageInfo
                [] -> Left "No .cabal package file found in this project."
                _ -> Left "Multiple packages found. Pass --target=lib, --target=exe:name, --target=test:name, or --target=bench:name."
  where
    allComponents =
        [ (packageInfo, component)
        | packageInfo <- projectPackages snapshot
        , component <- packageComponents packageInfo
        ]

chooseDefaultComponent :: PackageInfo -> Either String (PackageInfo, ComponentInfo)
chooseDefaultComponent packageInfo =
    case (libraries, executables, components) of
        ([library], _, _) -> Right (packageInfo, library)
        ([], [executable], _) -> Right (packageInfo, executable)
        ([], [], [component]) -> Right (packageInfo, component)
        _ ->
            Left
                ( "Unable to choose a default dependency target. Available targets: "
                    <> intercalate ", " (map componentTargetLabel components)
                    <> ". Pass --target=..."
                )
  where
    components = packageComponents packageInfo
    libraries = filter ((== Library) . componentType) components
    executables = filter ((== Executable) . componentType) components

matchesTarget :: String -> ComponentInfo -> Bool
matchesTarget rawTarget component =
    normalizeTarget rawTarget == normalizeTarget (componentTargetLabel component)
        || (normalizeTarget rawTarget == "library" && componentType component == Library)

normalizeTarget :: String -> String
normalizeTarget =
    map toLower

renderAvailableTargets :: [(PackageInfo, ComponentInfo)] -> String
renderAvailableTargets components =
    case components of
        [] -> "none"
        _ -> intercalate ", " (map (componentTargetLabel . snd) components)

applyAddPlan :: ProjectSnapshot -> AddPlan -> IO ()
applyAddPlan snapshot plan = do
    let fullPath = projectRoot snapshot </> planPackageFile plan
    contents <- readFile fullPath
    _ <- evaluate (length contents)
    writeFile fullPath (unlines (applyDependenciesToLines (planTarget plan) (planToAdd plan) (lines contents)))

applyDependenciesToLines :: ComponentInfo -> [String] -> [String] -> [String]
applyDependenciesToLines component dependencies originalLines =
    case componentBuildDependsLine component of
        Just lineNumber ->
            let fieldLine = originalLines !! (lineNumber - 1)
                fieldIndent = takeWhile isSpace fieldLine
                dependencyLines = map ((fieldIndent <> "  , ") <>) dependencies
             in insertAt (fieldEndIndex (lineNumber - 1) originalLines) dependencyLines originalLines
        Nothing ->
            insertAt stanzaEnd newFieldLines originalLines
          where
            stanzaEnd = stanzaEndIndex (componentHeaderLine component - 1) originalLines
            newFieldLines =
                case dependencies of
                    [] -> []
                    firstDependency : restDependencies ->
                        [ "    build-depends:"
                        , "        " <> firstDependency
                        ]
                            ++ map (("      , " <>) ) restDependencies

fieldEndIndex :: Int -> [String] -> Int
fieldEndIndex fieldIndex allLines =
    fromMaybe stanzaEnd (find isNextField [fieldIndex + 1 .. stanzaEnd - 1])
  where
    fieldIndent = takeWhile isSpace (allLines !! fieldIndex)
    stanzaEnd = stanzaEndIndex fieldIndex allLines
    isNextField index =
        let line = allLines !! index
         in not (null (trim line))
                && takeWhile isSpace line == fieldIndent
                && isFieldLine line

stanzaEndIndex :: Int -> [String] -> Int
stanzaEndIndex startIndex allLines =
    fromMaybe (length allLines) (find isNextStanza [startIndex + 1 .. length allLines - 1])
  where
    isNextStanza index =
        let line = allLines !! index
         in isStanzaHeader line

insertAt :: Int -> [String] -> [String] -> [String]
insertAt index newLines existingLines =
    let (before, after) = splitAt index existingLines
     in before ++ newLines ++ after

isStanzaHeader :: String -> Bool
isStanzaHeader rawLine =
    let line = trim rawLine
     in not (null line)
            && not (isSpace (head rawLine))
            && not ("--" `isPrefixOf` line)
            && not (isFieldLine rawLine)

isFieldLine :: String -> Bool
isFieldLine rawLine =
    case break (== ':') (dropWhile isSpace rawLine) of
        (name, ':' : _) -> not (null name) && all isFieldNameChar name
        _ -> False

isFieldNameChar :: Char -> Bool
isFieldNameChar char =
    isAlphaNum char || char `elem` ['-', '_', '.']

runVerification :: [String] -> IO ExitCode
runVerification command =
    case command of
        commandName : commandArgs -> rawSystem commandName commandArgs
        [] -> rawSystem "true" []

renderAddPlan :: AddInvocation -> AddPlan -> String
renderAddPlan invocation plan =
    unlines
        [ "hx add"
        , "Package file: " <> planPackageFile plan
        , "Target: " <> componentTargetLabel (planTarget plan)
        , "Mode: " <> if addPlanOnly invocation then "plan only" else "apply"
        , "Already present: " <> renderList (planAlreadyPresent plan)
        , "Will add: " <> renderList (planToAdd plan)
        , "Side effects: " <> if null (planToAdd plan) then "none" else "edit " <> planPackageFile plan
        , "Evidence: " <> if addVerify invocation then intercalate " " (planVerifyCommand plan) else "verification skipped by --no-verify"
        ]

addPlanToJson :: AddInvocation -> AddPlan -> String
addPlanToJson invocation plan =
    jsonObject
        [ ("schemaVersion", jsonString "hx.add-plan.v1")
        , ("packageFile", jsonString (planPackageFile plan))
        , ("target", jsonString (componentTargetLabel (planTarget plan)))
        , ("planOnly", jsonBool (addPlanOnly invocation))
        , ("verify", jsonBool (addVerify invocation))
        , ("alreadyPresent", jsonArray (map jsonString (planAlreadyPresent plan)))
        , ("willAdd", jsonArray (map jsonString (planToAdd plan)))
        , ("sideEffects", jsonArray (map jsonString sideEffects))
        , ("evidenceCommand", jsonArray (map jsonString (planVerifyCommand plan)))
        ]
  where
    sideEffects =
        if null (planToAdd plan)
            then []
            else ["edit " <> planPackageFile plan]

renderList :: [String] -> String
renderList values =
    case values of
        [] -> "none"
        _ -> intercalate ", " values

normalizeDependencyName :: String -> String
normalizeDependencyName rawName =
    takeWhile isDependencyNameChar rawName

isDependencyNameChar :: Char -> Bool
isDependencyNameChar char =
    isAlphaNum char || char `elem` ['-', '_']

trim :: String -> String
trim =
    dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse

addUsage :: String
addUsage =
    unlines
        [ "hx add - plan or apply Cabal dependency updates"
        , ""
        , "Usage:"
        , "  hx add <package>... [--target=lib|exe:name|test:name|bench:name] [--plan] [--json]"
        , "  hx add <package>... --apply [--no-verify]"
        , ""
        , "Examples:"
        , "  hx add aeson text --plan"
        , "  hx add hspec --target=test:my-test --apply"
        ]
