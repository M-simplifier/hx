module Hx.Project
    ( ComponentInfo (..)
    , ComponentType (..)
    , PackageInfo (..)
    , ProjectSnapshot (..)
    , componentTargetLabel
    , componentTypeLabel
    , discoverProject
    , packageTargetLabel
    , projectSnapshotToJson
    , renderProjectSnapshot
    )
where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Hx.Json (jsonArray, jsonMaybe, jsonNumber, jsonObject, jsonString)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>), takeExtension)

data ProjectSnapshot = ProjectSnapshot
    { projectRoot :: FilePath
    , projectPackages :: [PackageInfo]
    , projectConfigFiles :: [FilePath]
    }
    deriving (Eq, Show)

data PackageInfo = PackageInfo
    { packageFile :: FilePath
    , packageName :: Maybe String
    , packageVersion :: Maybe String
    , packageComponents :: [ComponentInfo]
    }
    deriving (Eq, Show)

data ComponentInfo = ComponentInfo
    { componentType :: ComponentType
    , componentName :: Maybe String
    , componentHeaderLine :: Int
    , componentBuildDepends :: [String]
    , componentBuildDependsLine :: Maybe Int
    }
    deriving (Eq, Show)

data ComponentType
    = Library
    | Executable
    | TestSuite
    | Benchmark
    deriving (Eq, Ord, Show)

data FieldOccurrence = FieldOccurrence
    { occurrenceName :: String
    , occurrenceValue :: String
    , occurrenceLineNumber :: Int
    , occurrenceScope :: Maybe String
    }

discoverProject :: IO ProjectSnapshot
discoverProject = do
    root <- getCurrentDirectory
    projectFiles <- findProjectFiles root
    let cabalFiles = filter ((== ".cabal") . takeExtension) projectFiles
        configFiles = filter isCabalProjectFile projectFiles
    packages <- mapM (inspectPackageFile root) cabalFiles
    pure
        ProjectSnapshot
            { projectRoot = root
            , projectPackages = packages
            , projectConfigFiles = configFiles
            }

findProjectFiles :: FilePath -> IO [FilePath]
findProjectFiles root =
    go ""
  where
    go relativeDir = do
        let currentDir =
                if null relativeDir
                    then root
                    else root </> relativeDir
        entries <- sort <$> listDirectory currentDir
        fmap concat (mapM (visitEntry relativeDir) entries)

    visitEntry relativeDir entry = do
        let relativePath =
                if null relativeDir
                    then entry
                    else relativeDir </> entry
            fullPath = root </> relativePath
        isDir <- doesDirectoryExist fullPath
        if isDir
            then
                if shouldSkipDirectory entry
                    then pure []
                    else go relativePath
            else
                if isRelevantProjectFile entry
                    then pure [relativePath]
                    else pure []

shouldSkipDirectory :: FilePath -> Bool
shouldSkipDirectory entry =
    entry `elem` [".git", ".direnv", ".stack-work", "dist-newstyle", "node_modules", "target"]
        || "." `isPrefixOf` entry

isRelevantProjectFile :: FilePath -> Bool
isRelevantProjectFile entry =
    takeExtension entry == ".cabal" || isCabalProjectFile entry

isCabalProjectFile :: FilePath -> Bool
isCabalProjectFile entry =
    entry == "cabal.project" || "cabal.project." `isPrefixOf` entry

inspectPackageFile :: FilePath -> FilePath -> IO PackageInfo
inspectPackageFile root relativePath = do
    contents <- readFile (root </> relativePath)
    let occurrences = collectFieldOccurrences contents
        packageFields = filter ((== Nothing) . occurrenceScope) occurrences
        pkgName = trim <$> listToMaybe (fieldValues "name" packageFields)
        version = trim <$> listToMaybe (fieldValues "version" packageFields)
        components = collectComponents occurrences contents
    pure
        PackageInfo
            { packageFile = relativePath
            , packageName = pkgName
            , packageVersion = version
            , packageComponents = components
            }

fieldValues :: String -> [FieldOccurrence] -> [String]
fieldValues fieldName occurrences =
    [ occurrenceValue occurrence
    | occurrence <- occurrences
    , normalized (occurrenceName occurrence) == normalized fieldName
    ]

collectComponents :: [FieldOccurrence] -> String -> [ComponentInfo]
collectComponents occurrences contents =
    mapMaybe toComponent stanzaHeaders
  where
    stanzaHeaders =
        [ (lineNumber, trim line)
        | (lineNumber, line) <- zip [1 ..] (lines contents)
        , isStanzaHeader line
        ]

    toComponent (lineNumber, header) = do
        (kind, name) <- parseComponentHeader header
        let buildDependsOccurrences =
                [ occurrence
                | occurrence <- occurrences
                , occurrenceScope occurrence == Just header
                , normalized (occurrenceName occurrence) == "build-depends"
                ]
            buildDependsOccurrence = listToMaybe buildDependsOccurrences
        pure
            ComponentInfo
                { componentType = kind
                , componentName = name
                , componentHeaderLine = lineNumber
                , componentBuildDepends =
                    maybe [] (parseDependencyNames . occurrenceValue) buildDependsOccurrence
                , componentBuildDependsLine = occurrenceLineNumber <$> buildDependsOccurrence
                }

parseComponentHeader :: String -> Maybe (ComponentType, Maybe String)
parseComponentHeader header =
    firstMatching
        [ ("library", Library, False)
        , ("executable ", Executable, True)
        , ("test-suite ", TestSuite, True)
        , ("benchmark ", Benchmark, True)
        ]
  where
    firstMatching [] = Nothing
    firstMatching ((prefix, kind, needsName) : rest)
        | header == prefix && not needsName = Just (kind, Nothing)
        | prefix `isPrefixOf` header =
            let name = trim (drop (length prefix) header)
             in if null name then Nothing else Just (kind, Just name)
        | otherwise = firstMatching rest

collectFieldOccurrences :: String -> [FieldOccurrence]
collectFieldOccurrences =
    reverse . finalize . go Nothing Nothing [] . zip [1 ..] . lines
  where
    go _currentScope currentField occurrences [] =
        appendCurrent currentField occurrences
    go currentScope currentField occurrences ((lineNumber, line) : rest)
        | isIgnoredLine line =
            go currentScope currentField occurrences rest
        | otherwise =
            case parseFieldLine line of
                Just (name, fieldValue) ->
                    let nextOccurrences = appendCurrent currentField occurrences
                        nextField =
                            Just
                                FieldOccurrence
                                    { occurrenceName = trim name
                                    , occurrenceValue = trim fieldValue
                                    , occurrenceLineNumber = lineNumber
                                    , occurrenceScope = currentScope
                                    }
                     in go currentScope nextField nextOccurrences rest
                Nothing ->
                    if isStanzaHeader line
                        then
                            let nextOccurrences = appendCurrent currentField occurrences
                             in go (Just (trim line)) Nothing nextOccurrences rest
                        else
                            case currentField of
                                Just occurrence ->
                                    let continued = trim line
                                     in if null continued
                                            then go currentScope currentField occurrences rest
                                            else
                                                go
                                                    currentScope
                                                    (Just occurrence{occurrenceValue = trim (occurrenceValue occurrence <> " " <> continued)})
                                                    occurrences
                                                    rest
                                Nothing -> go currentScope Nothing occurrences rest

    appendCurrent currentField occurrences =
        case currentField of
            Just occurrence
                | not (null (occurrenceValue occurrence)) -> occurrence : occurrences
            _ -> occurrences

    finalize =
        filter (not . null . occurrenceValue)

parseFieldLine :: String -> Maybe (String, String)
parseFieldLine rawLine =
    case break (== ':') (dropWhile isSpace rawLine) of
        (name, ':' : value)
            | isValidFieldName name -> Just (name, value)
        _ -> Nothing

isValidFieldName :: String -> Bool
isValidFieldName name =
    not (null name)
        && all (\char -> char == '-' || char == '_' || char == '.' || isAlphaNum char) name

isStanzaHeader :: String -> Bool
isStanzaHeader rawLine =
    let trimmedLine = trim rawLine
     in not (null trimmedLine)
            && not (isSpace (head rawLine))
            && not (isComment trimmedLine)
            && parseFieldLine rawLine == Nothing

isIgnoredLine :: String -> Bool
isIgnoredLine line =
    let trimmedLine = trim line
     in null trimmedLine || isComment trimmedLine

isComment :: String -> Bool
isComment =
    ("--" `isPrefixOf`)

parseDependencyNames :: String -> [String]
parseDependencyNames value =
    mapMaybe parseDependencyName (splitOnComma value)

parseDependencyName :: String -> Maybe String
parseDependencyName rawValue =
    let value = trim rawValue
        name = takeWhile isDependencyNameChar value
     in if null name then Nothing else Just name

isDependencyNameChar :: Char -> Bool
isDependencyNameChar char =
    isAlphaNum char || char `elem` ['-', '_']

splitOnComma :: String -> [String]
splitOnComma value =
    case break (== ',') value of
        (chunk, ',' : rest) -> chunk : splitOnComma rest
        (chunk, _) -> [chunk]

renderProjectSnapshot :: ProjectSnapshot -> String
renderProjectSnapshot snapshot =
    unlines
        ( [ "hx status"
          , ""
          , "Root: " <> projectRoot snapshot
          , "Cabal project files: " <> renderList (projectConfigFiles snapshot)
          , "Packages:"
          ]
            ++ renderPackages (projectPackages snapshot)
        )

renderPackages :: [PackageInfo] -> [String]
renderPackages [] =
    ["  - none"]
renderPackages packages =
    concatMap renderPackage packages

renderPackage :: PackageInfo -> [String]
renderPackage packageInfo =
    [ "  - " <> packageTargetLabel packageInfo
    , "    file: " <> packageFile packageInfo
    , "    components: " <> renderList (map componentTargetLabel (packageComponents packageInfo))
    ]

packageTargetLabel :: PackageInfo -> String
packageTargetLabel packageInfo =
    fromMaybe "(unnamed package)" (packageName packageInfo)
        <> maybe "" ("-" <>) (packageVersion packageInfo)

componentTargetLabel :: ComponentInfo -> String
componentTargetLabel component =
    case componentType component of
        Library -> "lib"
        Executable -> "exe:" <> fromMaybe "(unnamed)" (componentName component)
        TestSuite -> "test:" <> fromMaybe "(unnamed)" (componentName component)
        Benchmark -> "bench:" <> fromMaybe "(unnamed)" (componentName component)

componentTypeLabel :: ComponentType -> String
componentTypeLabel componentTypeValue =
    case componentTypeValue of
        Library -> "library"
        Executable -> "executable"
        TestSuite -> "test-suite"
        Benchmark -> "benchmark"

projectSnapshotToJson :: ProjectSnapshot -> String
projectSnapshotToJson snapshot =
    jsonObject
        [ ("schemaVersion", jsonString "hx.project.v1")
        , ("root", jsonString (projectRoot snapshot))
        , ("configFiles", jsonArray (map jsonString (projectConfigFiles snapshot)))
        , ("packages", jsonArray (map packageToJson (projectPackages snapshot)))
        ]

packageToJson :: PackageInfo -> String
packageToJson packageInfo =
    jsonObject
        [ ("file", jsonString (packageFile packageInfo))
        , ("name", jsonMaybe jsonString (packageName packageInfo))
        , ("version", jsonMaybe jsonString (packageVersion packageInfo))
        , ("components", jsonArray (map componentToJson (packageComponents packageInfo)))
        ]

componentToJson :: ComponentInfo -> String
componentToJson component =
    jsonObject
        [ ("type", jsonString (componentTypeLabel (componentType component)))
        , ("name", jsonMaybe jsonString (componentName component))
        , ("target", jsonString (componentTargetLabel component))
        , ("headerLine", jsonNumber (componentHeaderLine component))
        , ("buildDepends", jsonArray (map jsonString (componentBuildDepends component)))
        , ("buildDependsLine", maybe "null" jsonNumber (componentBuildDependsLine component))
        ]

renderList :: [String] -> String
renderList values =
    case values of
        [] -> "none"
        _ -> intercalate ", " values

normalized :: String -> String
normalized =
    map toLower . trim

trim :: String -> String
trim =
    dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse
