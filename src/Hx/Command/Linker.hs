module Hx.Command.Linker (runLinker) where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Hx.Command.CabalSupport
    ( LinkerPlan (AutoSelectFastLinker)
    , decideLinkerPlan
    , fuseLdValue
    , linkerArgs
    , linkerPlanWarnings
    , renderLinkerPlan
    )
import Hx.Command.Doctor
    ( DiagnosticSnapshot (..)
    , availableFastLinkerNames
    , inspectDiagnostics
    , projectHasExplicitLinkerSelection
    )
import Hx.Json (jsonArray, jsonBool, jsonObject, jsonString)
import System.Directory (doesFileExist, removeFile)
import System.Exit (die)

data LinkerInvocation
    = LinkerStatus Bool
    | LinkerUse LinkerChoice Bool Bool
    | LinkerClear Bool Bool

data LinkerChoice
    = LinkerAuto
    | LinkerNamed String

data LocalConfig = LocalConfig
    { localExists :: Bool
    , localContents :: String
    }

data UsePlan = UsePlan
    { useTarget :: String
    , useApply :: Bool
    , useBlock :: [String]
    , useSideEffects :: [String]
    }

data ClearPlan = ClearPlan
    { clearApply :: Bool
    , clearSideEffects :: [String]
    }

runLinker :: [String] -> IO ()
runLinker args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn linkerUsage
        else
            case parseLinkerInvocation args of
                Left message ->
                    die message
                Right invocation ->
                    runLinkerInvocation invocation

parseLinkerInvocation :: [String] -> Either String LinkerInvocation
parseLinkerInvocation args =
    case commandArgs of
        [] -> Right (LinkerStatus wantsJson)
        ["status"] -> Right (LinkerStatus wantsJson)
        ["use", rawChoice] -> do
            choice <- parseLinkerChoice rawChoice
            Right (LinkerUse choice wantsJson wantsApply)
        ["clear"] ->
            Right (LinkerClear wantsJson wantsApply)
        _ ->
            Left ("Unknown linker command.\n\n" <> linkerUsage)
  where
    wantsJson = "--json" `elem` args
    wantsApply = "--apply" `elem` args
    commandArgs = filter (not . isControlFlag) args

isControlFlag :: String -> Bool
isControlFlag arg =
    arg `elem` ["--json", "--plan", "--apply"]

parseLinkerChoice :: String -> Either String LinkerChoice
parseLinkerChoice rawChoice =
    case rawChoice of
        "auto" -> Right LinkerAuto
        "mold" -> Right (LinkerNamed "mold")
        "ld.lld" -> Right (LinkerNamed "ld.lld")
        "lld" -> Right (LinkerNamed "ld.lld")
        _ -> Left ("Unknown linker: " <> rawChoice <> ". Use auto, mold, or ld.lld.")

runLinkerInvocation :: LinkerInvocation -> IO ()
runLinkerInvocation invocation = do
    snapshot <- inspectDiagnostics
    localConfig <- readLocalConfig
    case invocation of
        LinkerStatus wantsJson ->
            if wantsJson
                then putStrLn (linkerStatusToJson snapshot localConfig)
                else putStr (renderLinkerStatus snapshot localConfig)
        LinkerUse choice wantsJson wantsApply -> do
            plan <- makeUsePlan snapshot localConfig choice wantsApply
            if wantsJson
                then putStrLn (usePlanToJson snapshot localConfig plan)
                else putStr (renderUsePlan snapshot localConfig plan)
            when wantsApply (applyUsePlan localConfig plan)
        LinkerClear wantsJson wantsApply -> do
            plan <- makeClearPlan localConfig wantsApply
            if wantsJson
                then putStrLn (clearPlanToJson snapshot localConfig plan)
                else putStr (renderClearPlan snapshot localConfig plan)
            when wantsApply (applyClearPlan localConfig)

readLocalConfig :: IO LocalConfig
readLocalConfig = do
    exists <- doesFileExist localConfigPath
    if exists
        then do
            contents <- readFile localConfigPath
            _ <- evaluate (length contents)
            pure LocalConfig{localExists = True, localContents = contents}
        else pure LocalConfig{localExists = False, localContents = ""}

makeUsePlan :: DiagnosticSnapshot -> LocalConfig -> LinkerChoice -> Bool -> IO UsePlan
makeUsePlan snapshot localConfig choice wantsApply = do
    target <- resolveTargetLinker snapshot choice
    when (projectHasExplicitLinkerSelection (snapshotProjectDiagnostics snapshot) && not (hasManagedBlock localConfig)) $
        die
            ( "This project already contains an explicit linker setting outside the hx-managed local block.\n\n"
                <> "Run `hx doctor` to inspect it, then edit the project settings manually or remove that setting before using `hx linker use`."
            )
    replacement <- either die pure (replaceManagedBlock (localContents localConfig) (managedBlock target))
    pure
        UsePlan
            { useTarget = target
            , useApply = wantsApply
            , useBlock = managedBlock target
            , useSideEffects = useEffects localConfig replacement
            }

resolveTargetLinker :: DiagnosticSnapshot -> LinkerChoice -> IO String
resolveTargetLinker snapshot choice =
    case choice of
        LinkerAuto ->
            case availableFastLinkerNames snapshot of
                linkerName : _ -> pure linkerName
                [] ->
                    die "No fast linker is available on PATH. Install `mold` or `ld.lld`, then rerun `hx linker use auto`."
        LinkerNamed linkerName ->
            if linkerName `elem` availableFastLinkerNames snapshot
                then pure linkerName
                else die ("`" <> linkerName <> "` is not available on PATH. Install it first or use `hx linker status`.")

makeClearPlan :: LocalConfig -> Bool -> IO ClearPlan
makeClearPlan localConfig wantsApply =
    case removeManagedBlock (localContents localConfig) of
        Left message -> die message
        Right Nothing ->
            pure ClearPlan{clearApply = wantsApply, clearSideEffects = []}
        Right (Just newContents) ->
            pure ClearPlan{clearApply = wantsApply, clearSideEffects = clearEffects newContents}

applyUsePlan :: LocalConfig -> UsePlan -> IO ()
applyUsePlan localConfig plan = do
    replacement <- either die pure (replaceManagedBlock (localContents localConfig) (useBlock plan))
    writeFile localConfigPath replacement
    putStrLn ("Applied linker setting to " <> localConfigPath <> ".")

applyClearPlan :: LocalConfig -> IO ()
applyClearPlan localConfig =
    case removeManagedBlock (localContents localConfig) of
        Left message -> die message
        Right Nothing ->
            putStrLn "No hx-managed linker setting found."
        Right (Just newContents) ->
            if hasMeaningfulContent newContents
                then do
                    writeFile localConfigPath newContents
                    putStrLn ("Removed hx-managed linker setting from " <> localConfigPath <> ".")
                else do
                    exists <- doesFileExist localConfigPath
                    when exists (removeFile localConfigPath)
                    putStrLn ("Removed " <> localConfigPath <> ".")

renderLinkerStatus :: DiagnosticSnapshot -> LocalConfig -> String
renderLinkerStatus snapshot localConfig =
    unlines
        [ "hx linker"
        , "Fast linkers: " <> renderList (availableFastLinkerNames snapshot)
        , "Current plan: " <> renderLinkerPlan snapshot plan
        , "Invocation args: " <> renderList (linkerArgs plan)
        , "Warnings: " <> renderList (linkerPlanWarnings plan)
        , "Local config: " <> renderLocalConfig localConfig
        , "Managed commands: hx build, hx run, hx ci, hx test"
        , ""
        , "To persist a local linker setting for raw Cabal too:"
        , "  hx linker use auto --apply"
        ]
  where
    plan = decideLinkerPlan snapshot

linkerStatusToJson :: DiagnosticSnapshot -> LocalConfig -> String
linkerStatusToJson snapshot localConfig =
    jsonObject
        [ ("schemaVersion", jsonString "hx.linker-status.v1")
        , ("availableFastLinkers", jsonArray (map jsonString (availableFastLinkerNames snapshot)))
        , ("plan", jsonString (renderLinkerPlan snapshot plan))
        , ("linkerArgs", jsonArray (map jsonString (linkerArgs plan)))
        , ("warnings", jsonArray (map jsonString (linkerPlanWarnings plan)))
        , ("localConfigPath", jsonString localConfigPath)
        , ("localConfigExists", jsonBool (localExists localConfig))
        , ("localConfigManaged", jsonBool (hasManagedBlock localConfig))
        ]
  where
    plan = decideLinkerPlan snapshot

renderUsePlan :: DiagnosticSnapshot -> LocalConfig -> UsePlan -> String
renderUsePlan snapshot localConfig plan =
    unlines
        [ "hx linker use"
        , "Target linker: " <> useTarget plan
        , "Mode: " <> if useApply plan then "apply" else "plan only"
        , "Current plan: " <> renderLinkerPlan snapshot (decideLinkerPlan snapshot)
        , "Warnings: " <> renderList (linkerPlanWarnings (AutoSelectFastLinker (useTarget plan)))
        , "Local config: " <> renderLocalConfig localConfig
        , "Side effects: " <> renderList (useSideEffects plan)
        , "Managed block:"
        , indent (unlines (useBlock plan))
        , if useApply plan then "Applied by --apply." else "Add --apply to write this local setting."
        ]

usePlanToJson :: DiagnosticSnapshot -> LocalConfig -> UsePlan -> String
usePlanToJson snapshot localConfig plan =
    jsonObject
        [ ("schemaVersion", jsonString "hx.linker-plan.v1")
        , ("action", jsonString "use")
        , ("target", jsonString (useTarget plan))
        , ("planOnly", jsonBool (not (useApply plan)))
        , ("localConfigPath", jsonString localConfigPath)
        , ("localConfigExists", jsonBool (localExists localConfig))
        , ("localConfigManaged", jsonBool (hasManagedBlock localConfig))
        , ("currentPlan", jsonString (renderLinkerPlan snapshot (decideLinkerPlan snapshot)))
        , ("warnings", jsonArray (map jsonString (linkerPlanWarnings (AutoSelectFastLinker (useTarget plan)))))
        , ("sideEffects", jsonArray (map jsonString (useSideEffects plan)))
        , ("managedBlock", jsonString (unlines (useBlock plan)))
        ]

renderClearPlan :: DiagnosticSnapshot -> LocalConfig -> ClearPlan -> String
renderClearPlan snapshot localConfig plan =
    unlines
        [ "hx linker clear"
        , "Mode: " <> if clearApply plan then "apply" else "plan only"
        , "Current plan: " <> renderLinkerPlan snapshot (decideLinkerPlan snapshot)
        , "Local config: " <> renderLocalConfig localConfig
        , "Side effects: " <> renderList (clearSideEffects plan)
        , if clearApply plan then "Applied by --apply." else "Add --apply to remove the hx-managed local setting."
        ]

clearPlanToJson :: DiagnosticSnapshot -> LocalConfig -> ClearPlan -> String
clearPlanToJson snapshot localConfig plan =
    jsonObject
        [ ("schemaVersion", jsonString "hx.linker-plan.v1")
        , ("action", jsonString "clear")
        , ("planOnly", jsonBool (not (clearApply plan)))
        , ("localConfigPath", jsonString localConfigPath)
        , ("localConfigExists", jsonBool (localExists localConfig))
        , ("localConfigManaged", jsonBool (hasManagedBlock localConfig))
        , ("currentPlan", jsonString (renderLinkerPlan snapshot (decideLinkerPlan snapshot)))
        , ("sideEffects", jsonArray (map jsonString (clearSideEffects plan)))
        ]

renderLocalConfig :: LocalConfig -> String
renderLocalConfig localConfig
    | not (localExists localConfig) = localConfigPath <> " absent"
    | hasManagedBlock localConfig = localConfigPath <> " present with hx-managed linker block"
    | otherwise = localConfigPath <> " present without hx-managed linker block"

useEffects :: LocalConfig -> String -> [String]
useEffects localConfig replacement
    | localContents localConfig == replacement = []
    | hasManagedBlock localConfig = ["replace hx-managed linker block in " <> localConfigPath]
    | localExists localConfig = ["append hx-managed linker block to " <> localConfigPath]
    | otherwise = ["create " <> localConfigPath]

clearEffects :: String -> [String]
clearEffects newContents =
    if hasMeaningfulContent newContents
        then ["remove hx-managed linker block from " <> localConfigPath]
        else ["remove " <> localConfigPath]

managedBlock :: String -> [String]
managedBlock linkerName =
    [ beginMarker
    , "-- Managed by hx. Run `hx linker clear --apply` to remove this block."
    , "program-options"
    , "  ghc-options: -optl-fuse-ld=" <> fuseLdValue linkerName
    , endMarker
    ]

replaceManagedBlock :: String -> [String] -> Either String String
replaceManagedBlock contents block =
    case splitManagedBlock (lines contents) of
        Left message -> Left message
        Right Nothing -> Right (appendBlock contents block)
        Right (Just (before, after)) -> Right (unlines (before ++ block ++ after))

appendBlock :: String -> [String] -> String
appendBlock contents block =
    unlines (trimTrailingBlankLines (lines contents) ++ separator ++ block)
  where
    separator =
        if null (trimTrailingBlankLines (lines contents))
            then []
            else [""]

removeManagedBlock :: String -> Either String (Maybe String)
removeManagedBlock contents =
    case splitManagedBlock (lines contents) of
        Left message -> Left message
        Right Nothing -> Right Nothing
        Right (Just (before, after)) -> Right (Just (unlines (trimTrailingBlankLines before ++ dropLeadingBlankLines after)))

splitManagedBlock :: [String] -> Either String (Maybe ([String], [String]))
splitManagedBlock contentLines =
    case break (== beginMarker) contentLines of
        (_, []) -> Right Nothing
        (before, _ : afterBegin) ->
            case break (== endMarker) afterBegin of
                (_, []) -> Left ("Malformed hx-managed linker block in " <> localConfigPath <> ". Missing end marker.")
                (_, _ : afterEnd) -> Right (Just (before, afterEnd))

hasManagedBlock :: LocalConfig -> Bool
hasManagedBlock localConfig =
    beginMarker `isInfixOf` localContents localConfig && endMarker `isInfixOf` localContents localConfig

hasMeaningfulContent :: String -> Bool
hasMeaningfulContent =
    any (not . isIgnorableLine) . lines

isIgnorableLine :: String -> Bool
isIgnorableLine line =
    null (trim line) || "--" `isPrefixOf` trim line

trimTrailingBlankLines :: [String] -> [String]
trimTrailingBlankLines =
    reverse . dropWhile (null . trim) . reverse

dropLeadingBlankLines :: [String] -> [String]
dropLeadingBlankLines =
    dropWhile (null . trim)

trim :: String -> String
trim =
    dropWhileEnd isSpaceAscii . dropWhile isSpaceAscii

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse

isSpaceAscii :: Char -> Bool
isSpaceAscii char =
    char `elem` [' ', '\t', '\n', '\r']

indent :: String -> String
indent =
    unlines . map ("  " <>) . lines

renderList :: [String] -> String
renderList values =
    case values of
        [] -> "none"
        _ -> intercalate ", " values

localConfigPath :: FilePath
localConfigPath =
    "cabal.project.local"

beginMarker :: String
beginMarker =
    "-- hx linker: begin"

endMarker :: String
endMarker =
    "-- hx linker: end"

linkerUsage :: String
linkerUsage =
    unlines
        [ "hx linker - inspect or persist the local linker plan"
        , ""
        , "Usage:"
        , "  hx linker [status] [--json]"
        , "  hx linker use auto|mold|ld.lld [--plan] [--json]"
        , "  hx linker use auto|mold|ld.lld --apply"
        , "  hx linker clear [--plan] [--json]"
        , "  hx linker clear --apply"
        , ""
        , "Notes:"
        , "  hx build, hx run, hx ci, and hx test auto-select a fast linker per invocation."
        , "  hx linker use writes an hx-managed block to cabal.project.local so raw Cabal uses the same linker."
        ]
