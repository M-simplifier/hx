module Hx.Command.Doctor
    ( DiagnosticSnapshot (..)
    , buildPkgConfigBlocker
    , configuredProjectLinkers
    , buildPkgConfigWarnings
    , configuredProjectFastLinkers
    , defaultRunnableTargetArg
    , firstMissingConfiguredProjectLinker
    , inspectDiagnostics
    , projectHasExplicitLinkerSelection
    , projectHasGenericLinkerFlags
    , renderRunnableTargets
    , renderConfiguredProjectLinkers
    , renderProjectLinkerHints
    , runDoctor
    , runDoctorJson
    , preferredFastLinkerName
    )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Char (isSpace, toLower)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Hx.Json (jsonArray, jsonBool, jsonMaybe, jsonObject, jsonString)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data ToolRole
    = Required
    | Supporting
    | Linker
    deriving (Eq, Show)

data ToolSpec = ToolSpec
    { specName :: String
    , specRole :: ToolRole
    , specVersionArgs :: [String]
    }

data ToolReport = ToolReport
    { toolSpec :: ToolSpec
    , toolPath :: Maybe FilePath
    , toolVersion :: Maybe String
    }

data PackageManager
    = Apt
    | Brew
    | Pacman
    | Dnf
    | Yum
    | Zypper
    | Apk
    | Nix
    deriving (Eq, Show)

data HostInfo = HostInfo
    { hostPrettyName :: Maybe String
    , hostDistroId :: Maybe String
    , hostVersionId :: Maybe String
    , hostPackageManagers :: [PackageManager]
    , hostPrimaryManager :: Maybe PackageManager
    , hostIsWsl :: Bool
    }

data LinkerPosture = LinkerPosture
    { linkerCommand :: Maybe String
    , linkerIsGnuLd :: Maybe Bool
    , linkerAvailable :: [ToolReport]
    , linkerFastCandidates :: [ToolReport]
    }

data ProjectDiagnostics = ProjectDiagnostics
    { projectRoot :: FilePath
    , projectPackageReports :: [PackageReport]
    , projectConfigFiles :: [FilePath]
    , projectLinkerHints :: [ProjectLinkerHint]
    , projectConfiguredLinkers :: [ConfiguredProjectLinker]
    }

data PackageReport = PackageReport
    { packageName :: Maybe String
    , packageFile :: FilePath
    , packagePkgConfigDepends :: [PkgConfigDependencyReport]
    , packageRunnableComponents :: [RunnableComponent]
    }

data PkgConfigDependencyReport = PkgConfigDependencyReport
    { pkgConfigSpec :: String
    , pkgConfigName :: String
    , pkgConfigProbe :: PkgConfigProbe
    }

data PkgConfigProbe
    = PkgConfigToolMissing
    | PkgConfigResolved String
    | PkgConfigUnresolved
    deriving (Eq, Show)

data ProjectLinkerHint = ProjectLinkerHint
    { linkerHintFile :: FilePath
    , linkerHintLineNumber :: Int
    , linkerHintScope :: Maybe String
    , linkerHintField :: String
    , linkerHintValue :: String
    , linkerHintKind :: LinkerHintKind
    , linkerHintFastLinker :: Maybe String
    }

data LinkerHintKind
    = LinkerSelectsFastLinker String
    | LinkerSelectsLinkerCommand String
    | LinkerUsesLinkerFlags
    deriving (Eq, Show)

data ConfiguredProjectLinker = ConfiguredProjectLinker
    { configuredLinkerName :: String
    , configuredLinkerAvailable :: Bool
    , configuredLinkerFast :: Bool
    }

data FieldOccurrence = FieldOccurrence
    { occurrenceName :: String
    , occurrenceValue :: String
    , occurrenceLineNumber :: Int
    , occurrenceScope :: Maybe String
    }

data RunnableComponentType
    = RunnableExecutable
    | RunnableTestSuite
    | RunnableBenchmark
    deriving (Eq, Show)

data RunnableComponent = RunnableComponent
    { runnablePackageName :: Maybe String
    , runnableFile :: FilePath
    , runnableName :: String
    , runnableType :: RunnableComponentType
    }

data DiagnosticSnapshot = DiagnosticSnapshot
    { snapshotHostInfo :: HostInfo
    , snapshotReports :: [ToolReport]
    , snapshotLinkerPosture :: LinkerPosture
    , snapshotProjectDiagnostics :: ProjectDiagnostics
    }

runDoctor :: IO ()
runDoctor = do
    snapshot <- inspectDiagnostics
    let host = snapshotHostInfo snapshot
        reports = snapshotReports snapshot
        linker = snapshotLinkerPosture snapshot
        project = snapshotProjectDiagnostics snapshot
        advice = makeAdvice host reports linker project
    putStrLn "hx doctor"
    putStrLn ""
    putStrLn ("Host: " <> renderHost host)
    putStrLn ("Package manager: " <> renderPackageManagers host)
    putStrLn ""
    putStrLn "Tools:"
    mapM_ (putStrLn . renderToolReport reports) reports
    putStrLn ""
    putStrLn "Linker posture:"
    mapM_ putStrLn (renderLinkerPosture linker)
    putStrLn ""
    putStrLn "Project:"
    mapM_ putStrLn (renderProjectDiagnostics project)
    putStrLn ""
    putStrLn ("Summary: " <> summarizeStatus reports linker project)
    putStrLn ""
    putStrLn "Advice:"
    if null advice
        then putStrLn "  - No immediate issues detected."
        else mapM_ putStrLn (map ("  - " <>) advice)

runDoctorJson :: IO ()
runDoctorJson = do
    snapshot <- inspectDiagnostics
    putStrLn (diagnosticSnapshotToJson snapshot)

diagnosticSnapshotToJson :: DiagnosticSnapshot -> String
diagnosticSnapshotToJson snapshot =
    jsonObject
        [ ("schemaVersion", jsonString "hx.diagnostics.v1")
        , ("host", hostInfoToJson (snapshotHostInfo snapshot))
        , ("tools", jsonArray (map toolReportToJson (snapshotReports snapshot)))
        , ("linker", linkerPostureToJson (snapshotLinkerPosture snapshot))
        , ("project", projectDiagnosticsToJson (snapshotProjectDiagnostics snapshot))
        , ( "summary"
            , jsonString (summarizeStatus (snapshotReports snapshot) (snapshotLinkerPosture snapshot) (snapshotProjectDiagnostics snapshot))
            )
        ]

hostInfoToJson :: HostInfo -> String
hostInfoToJson host =
    jsonObject
        [ ("label", jsonString (renderHost host))
        , ("prettyName", jsonMaybe jsonString (hostPrettyName host))
        , ("distroId", jsonMaybe jsonString (hostDistroId host))
        , ("versionId", jsonMaybe jsonString (hostVersionId host))
        , ("packageManagers", jsonArray (map (jsonString . renderManager) (hostPackageManagers host)))
        , ("primaryPackageManager", jsonMaybe (jsonString . renderManager) (hostPrimaryManager host))
        , ("wsl", jsonBool (hostIsWsl host))
        ]

toolReportToJson :: ToolReport -> String
toolReportToJson report =
    jsonObject
        [ ("name", jsonString (specName (toolSpec report)))
        , ("role", jsonString (renderToolRole (specRole (toolSpec report))))
        , ("path", jsonMaybe jsonString (toolPath report))
        , ("version", jsonMaybe jsonString (toolVersion report))
        , ("available", jsonBool (isJust (toolPath report)))
        ]

linkerPostureToJson :: LinkerPosture -> String
linkerPostureToJson posture =
    jsonObject
        [ ("configuredByGhc", jsonMaybe jsonString (linkerCommand posture))
        , ("gnuLd", jsonMaybe jsonBool (linkerIsGnuLd posture))
        , ("available", jsonArray (map (jsonString . specName . toolSpec) (linkerAvailable posture)))
        , ("fastCandidates", jsonArray (map (jsonString . specName . toolSpec) (linkerFastCandidates posture)))
        , ("assessment", jsonString (linkerAssessment posture))
        ]

projectDiagnosticsToJson :: ProjectDiagnostics -> String
projectDiagnosticsToJson project =
    jsonObject
        [ ("root", jsonString (projectRoot project))
        , ("packageFiles", jsonArray (map (jsonString . packageFile) (projectPackageReports project)))
        , ("configFiles", jsonArray (map jsonString (projectConfigFiles project)))
        , ("pkgConfig", jsonString (renderPkgConfigUsage project))
        , ("configuredLinkers", jsonString (renderConfiguredProjectLinkers project))
        , ("linkerHints", jsonString (renderProjectLinkerHints project))
        , ("runnableTargets", jsonArray (map (jsonString . renderRunnableComponentTarget) (projectRunnableComponents project)))
        ]

renderToolRole :: ToolRole -> String
renderToolRole role =
    case role of
        Required -> "required"
        Supporting -> "supporting"
        Linker -> "linker"

inspectDiagnostics :: IO DiagnosticSnapshot
inspectDiagnostics = do
    host <- detectHostInfo
    reports <- mapM inspectTool tools
    linker <- inspectLinkerPosture reports
    project <- inspectProjectDiagnostics reports
    pure
        DiagnosticSnapshot
            { snapshotHostInfo = host
            , snapshotReports = reports
            , snapshotLinkerPosture = linker
            , snapshotProjectDiagnostics = project
            }

detectHostInfo :: IO HostInfo
detectHostInfo = do
    osRelease <- readOsRelease
    packageManagers <- detectPackageManagers
    wslByEnv <- isJust <$> lookupEnv "WSL_DISTRO_NAME"
    wslByInterop <- isJust <$> lookupEnv "WSL_INTEROP"
    kernelRelease <- readFileIfExists "/proc/sys/kernel/osrelease"
    let kernelLooksWsl =
            maybe False (containsInsensitive "microsoft") kernelRelease
        distroId = mapMaybeLookup "ID" osRelease
        versionId = mapMaybeLookup "VERSION_ID" osRelease
        preferredManagers = preferredPackageManagers distroId
        primaryManager =
            listToMaybe [manager | manager <- preferredManagers, manager `elem` packageManagers]
                <|> listToMaybe packageManagers
    pure
        HostInfo
            { hostPrettyName = mapMaybeLookup "PRETTY_NAME" osRelease
            , hostDistroId = distroId
            , hostVersionId = versionId
            , hostPackageManagers = packageManagers
            , hostPrimaryManager = primaryManager
            , hostIsWsl = wslByEnv || wslByInterop || kernelLooksWsl
            }

readOsRelease :: IO [(String, String)]
readOsRelease = do
    candidates <- mapM readKeyValueFile ["/etc/os-release", "/usr/lib/os-release"]
    pure (fromMaybe [] (find (not . null) candidates))

readKeyValueFile :: FilePath -> IO [(String, String)]
readKeyValueFile path = do
    exists <- doesFileExist path
    if exists
        then do
            contents <- readFile path
            pure (mapMaybe parseKeyValueLine (lines contents))
        else pure []

parseKeyValueLine :: String -> Maybe (String, String)
parseKeyValueLine line =
    case break (== '=') line of
        (key, '=' : rawValue) | not (null key) ->
            Just (key, stripQuotes rawValue)
        _ -> Nothing

stripQuotes :: String -> String
stripQuotes value =
    case value of
        '"' : rest -> reverse (dropWhile (== '"') (reverse rest))
        _ -> value

detectPackageManagers :: IO [PackageManager]
detectPackageManagers = do
    statuses <- mapM detectManager knownPackageManagers
    pure (map fst (filter snd statuses))

detectManager :: PackageManager -> IO (PackageManager, Bool)
detectManager manager = do
    found <- isJust <$> findExecutable (packageManagerCommand manager)
    pure (manager, found)

preferredPackageManagers :: Maybe String -> [PackageManager]
preferredPackageManagers distroId =
    case fmap (map toLower) distroId of
        Just "ubuntu" -> [Apt, Nix]
        Just "debian" -> [Apt, Nix]
        Just "linuxmint" -> [Apt, Nix]
        Just "pop" -> [Apt, Nix]
        Just "elementary" -> [Apt, Nix]
        Just "fedora" -> [Dnf, Yum, Nix]
        Just "rhel" -> [Dnf, Yum, Nix]
        Just "centos" -> [Dnf, Yum, Nix]
        Just "arch" -> [Pacman, Nix]
        Just "manjaro" -> [Pacman, Nix]
        Just "alpine" -> [Apk, Nix]
        Just "opensuse-tumbleweed" -> [Zypper, Nix]
        Just "opensuse-leap" -> [Zypper, Nix]
        Just "nixos" -> [Nix]
        Just "darwin" -> [Brew]
        _ ->
            case os of
                "darwin" -> [Brew]
                "linux" -> [Apt, Dnf, Yum, Pacman, Zypper, Apk, Nix]
                _ -> knownPackageManagers

inspectTool :: ToolSpec -> IO ToolReport
inspectTool spec = do
    path <- findExecutable (specName spec)
    version <-
        case path of
            Just _ -> fmap firstLine <$> runCommand (specName spec) (specVersionArgs spec)
            Nothing -> pure Nothing
    pure
        ToolReport
            { toolSpec = spec
            , toolPath = path
            , toolVersion = version
            }

inspectLinkerPosture :: [ToolReport] -> IO LinkerPosture
inspectLinkerPosture reports = do
    ghcInfo <- runCommand "ghc" ["--info"]
    let infoPairs = ghcInfo >>= readMaybe :: Maybe [(String, String)]
        configuredLinker = lookup "ld command" =<< infoPairs
        isGnuLd = fmap ((== "YES") . toUpperAscii) (lookup "ld is GNU ld" =<< infoPairs)
        linkersOnPath = filter ((== Linker) . specRole . toolSpec) reports
        fastCandidates =
            filter
                (\report -> specName (toolSpec report) `elem` ["mold", "ld.lld"] && isJust (toolPath report))
                linkersOnPath
    pure
        LinkerPosture
            { linkerCommand = configuredLinker
            , linkerIsGnuLd = isGnuLd
            , linkerAvailable = filter (isJust . toolPath) linkersOnPath
            , linkerFastCandidates = fastCandidates
            }

inspectProjectDiagnostics :: [ToolReport] -> IO ProjectDiagnostics
inspectProjectDiagnostics reports = do
    root <- getCurrentDirectory
    projectFiles <- findProjectFiles root
    let cabalFiles = filter ((== ".cabal") . takeExtension) projectFiles
        configFiles = filter isCabalProjectFile projectFiles
        pkgConfigCommand = toolCommandPath "pkg-config" reports
    packageReports <- mapM (inspectPackageFile root pkgConfigCommand) cabalFiles
    linkerHints <- fmap concat (mapM (inspectProjectLinkerHints root) (cabalFiles ++ configFiles))
    configuredLinkers <- inspectConfiguredProjectLinkers reports linkerHints
    pure
        ProjectDiagnostics
            { projectRoot = root
            , projectPackageReports = packageReports
            , projectConfigFiles = configFiles
            , projectLinkerHints = linkerHints
            , projectConfiguredLinkers = configuredLinkers
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
        paths <- mapM (visitEntry relativeDir) entries
        pure (concat paths)

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

inspectPackageFile :: FilePath -> Maybe FilePath -> FilePath -> IO PackageReport
inspectPackageFile root pkgConfigCommand relativePath = do
    let fullPath = root </> relativePath
    contents <- readFile fullPath
    let pkgName = listToMaybe (extractFieldValues "name" contents)
        pkgConfigSpecs =
            concatMap splitCommaSeparated (extractFieldValues "pkgconfig-depends" contents)
        runnableComponents = collectRunnableComponents relativePath (fmap trim pkgName) contents
    pkgConfigDepends <- mapM (inspectPkgConfigDependency pkgConfigCommand) pkgConfigSpecs
    pure
        PackageReport
            { packageName = fmap trim pkgName
            , packageFile = relativePath
            , packagePkgConfigDepends = pkgConfigDepends
            , packageRunnableComponents = runnableComponents
            }

inspectPkgConfigDependency :: Maybe FilePath -> String -> IO PkgConfigDependencyReport
inspectPkgConfigDependency pkgConfigCommand rawSpec = do
    let specText = trim rawSpec
        dependencyName = parsePkgConfigDependencyName specText
    probe <-
        case pkgConfigCommand of
            Nothing ->
                pure PkgConfigToolMissing
            Just commandPath -> do
                version <- fmap firstLine <$> runCommand commandPath ["--modversion", dependencyName]
                pure $
                    case version of
                        Just resolvedVersion -> PkgConfigResolved resolvedVersion
                        Nothing -> PkgConfigUnresolved
    pure
        PkgConfigDependencyReport
            { pkgConfigSpec = specText
            , pkgConfigName = dependencyName
            , pkgConfigProbe = probe
            }

inspectProjectLinkerHints :: FilePath -> FilePath -> IO [ProjectLinkerHint]
inspectProjectLinkerHints root relativePath = do
    let fullPath = root </> relativePath
    contents <- readFile fullPath
    pure
        (mapMaybe (toProjectLinkerHint relativePath) (collectFieldOccurrences contents))

inspectConfiguredProjectLinkers :: [ToolReport] -> [ProjectLinkerHint] -> IO [ConfiguredProjectLinker]
inspectConfiguredProjectLinkers reports hints =
    mapM inspectLinker linkerNames
  where
    linkerNames =
        nub (mapMaybe linkerHintSelectedLinker hints)

    inspectLinker linkerName = do
        available <- linkerCommandAppearsAvailable reports linkerName
        pure
            ConfiguredProjectLinker
                { configuredLinkerName = linkerName
                , configuredLinkerAvailable = available
                , configuredLinkerFast = isFastLinkerName linkerName
                }

linkerHintSelectedLinker :: ProjectLinkerHint -> Maybe String
linkerHintSelectedLinker hint =
    case linkerHintKind hint of
        LinkerSelectsFastLinker linkerName ->
            Just linkerName
        LinkerSelectsLinkerCommand linkerName ->
            Just linkerName
        LinkerUsesLinkerFlags ->
            Nothing

linkerCommandAppearsAvailable :: [ToolReport] -> String -> IO Bool
linkerCommandAppearsAvailable reports linkerName =
    case find (\report -> specName (toolSpec report) == linkerName) reports of
        Just report ->
            pure (isJust (toolPath report))
        Nothing ->
            if looksLikeCommandPath linkerName
                then doesFileExist linkerName
                else isJust <$> findExecutable linkerName

looksLikeCommandPath :: String -> Bool
looksLikeCommandPath linkerName =
    any (`elem` linkerName) ['/', '\\'] || "." `isPrefixOf` linkerName

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

extractFieldValues :: String -> String -> [String]
extractFieldValues fieldName contents =
    [ occurrenceValue occurrence
    | occurrence <- collectFieldOccurrences contents
    , map toLower (occurrenceName occurrence) == map toLower fieldName
    ]

collectFieldOccurrences :: String -> [FieldOccurrence]
collectFieldOccurrences =
    reverse . finalize . go Nothing Nothing [] . zip [1 ..] . lines
  where
    go _currentScope currentField occurrences [] =
        appendCurrent currentField occurrences
    go currentScope currentField occurrences ((lineNumber, line) : rest)
        | isIgnoredProjectLine line =
            go currentScope currentField occurrences rest
        | otherwise =
            case parseFieldLine line of
                Just (name, fieldValue) ->
                    let nextOccurrences = appendCurrent currentField occurrences
                        nextField =
                            Just
                                ( FieldOccurrence
                                    { occurrenceName = trim name
                                    , occurrenceValue = trim fieldValue
                                    , occurrenceLineNumber = lineNumber
                                    , occurrenceScope = currentScope
                                    }
                                )
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
                | not (null (occurrenceValue occurrence)) ->
                    occurrence : occurrences
            _ -> occurrences

    finalize =
        filter (not . null . occurrenceValue)

parseFieldLine :: String -> Maybe (String, String)
parseFieldLine rawLine =
    case break (== ':') (dropWhile isSpace rawLine) of
        (name, ':' : value)
            | isValidFieldName name ->
                Just (name, value)
        _ -> Nothing

isValidFieldName :: String -> Bool
isValidFieldName name =
    not (null name)
        && all (\char -> char == '-' || char == '_' || char == '.' || isAlphaNumAscii char) name

isStanzaHeader :: String -> Bool
isStanzaHeader rawLine =
    let trimmedLine = trim rawLine
     in not (null trimmedLine)
            && not (isSpace (head rawLine))
            && isNothing (parseFieldLine rawLine)

isIgnoredProjectLine :: String -> Bool
isIgnoredProjectLine line =
    let trimmedLine = trim line
     in null trimmedLine || "--" `isPrefixOf` trimmedLine

toProjectLinkerHint :: FilePath -> FieldOccurrence -> Maybe ProjectLinkerHint
toProjectLinkerHint relativePath occurrence = do
    guardLinkerField occurrence
    kind <- classifyLinkerOccurrence occurrence
    pure
        ProjectLinkerHint
            { linkerHintFile = relativePath
            , linkerHintLineNumber = occurrenceLineNumber occurrence
            , linkerHintScope = occurrenceScope occurrence
            , linkerHintField = occurrenceName occurrence
            , linkerHintValue = occurrenceValue occurrence
            , linkerHintKind = kind
            , linkerHintFastLinker = linkerHintFastLinkerFromKind kind
            }

guardLinkerField :: FieldOccurrence -> Maybe ()
guardLinkerField occurrence =
    if normalizedFieldName occurrence `elem` linkerFieldNames
        then Just ()
        else Nothing

normalizedFieldName :: FieldOccurrence -> String
normalizedFieldName =
    map toLower . occurrenceName

linkerFieldNames :: [String]
linkerFieldNames =
    [ "ghc-options"
    , "ghc-shared-options"
    , "ghc-prof-options"
    , "ld-options"
    ]

classifyLinkerOccurrence :: FieldOccurrence -> Maybe LinkerHintKind
classifyLinkerOccurrence occurrence =
    classifyLinkerTokens (normalizedFieldName occurrence) (tokenizeOptionValue (occurrenceValue occurrence))

classifyLinkerTokens :: String -> [String] -> Maybe LinkerHintKind
classifyLinkerTokens fieldName tokens =
    case explicitLinkerSelection tokens of
        Just linkerName
            | Just fastLinkerName <- fastLinkerNameOf linkerName ->
                Just (LinkerSelectsFastLinker fastLinkerName)
            | otherwise ->
                Just (LinkerSelectsLinkerCommand linkerName)
        Nothing
            | fieldName == "ld-options" && not (null tokens) ->
                Just LinkerUsesLinkerFlags
            | any isLinkerFlagToken tokens ->
                Just LinkerUsesLinkerFlags
            | otherwise ->
                Nothing

explicitLinkerSelection :: [String] -> Maybe String
explicitLinkerSelection tokens =
    listToMaybe
        ( mapMaybe extractPgmlTarget (tokenPairs tokens)
            ++ mapMaybe extractOptlTarget (tokenPairs tokens)
            ++ mapMaybe extractFuseLdTarget tokens
        )

tokenizeOptionValue :: String -> [String]
tokenizeOptionValue =
    words

tokenPairs :: [a] -> [(a, Maybe a)]
tokenPairs [] = []
tokenPairs [x] = [(x, Nothing)]
tokenPairs (x : y : rest) = (x, Just y) : tokenPairs (y : rest)

extractFuseLdTarget :: String -> Maybe String
extractFuseLdTarget token
    | Just payload <- stripPrefixToken "-optl=" token =
        extractFuseLdTarget payload
    | Just payload <- stripAttachedToken "-optl" token =
        extractFuseLdTarget payload
    | Just payload <- stripPrefixToken "-Wl," token =
        listToMaybe (mapMaybe extractFuseLdTarget (splitOnComma payload))
    | Just payload <- stripPrefixToken "-fuse-ld=" token =
        normalizeFuseLdTarget payload
    | Just payload <- stripPrefixToken "--fuse-ld=" token =
        normalizeFuseLdTarget payload
    | otherwise =
        Nothing

extractOptlTarget :: (String, Maybe String) -> Maybe String
extractOptlTarget (token, nextToken)
    | token == "-optl" =
        nextToken >>= extractFuseLdTarget
    | otherwise =
        Nothing

extractPgmlTarget :: (String, Maybe String) -> Maybe String
extractPgmlTarget (token, nextToken)
    | token == "-pgml" =
        nextToken >>= normalizeLinkerCommandTarget
    | Just payload <- stripPrefixToken "-pgml=" token =
        normalizeLinkerCommandTarget payload
    | Just payload <- stripAttachedToken "-pgml" token =
        normalizeLinkerCommandTarget payload
    | otherwise =
        Nothing

linkerHintFastLinkerFromKind :: LinkerHintKind -> Maybe String
linkerHintFastLinkerFromKind kind =
    case kind of
        LinkerSelectsFastLinker linkerName -> Just linkerName
        _ -> Nothing

isFastLinkerName :: String -> Bool
isFastLinkerName =
    isJust . fastLinkerNameOf

fastLinkerNameOf :: String -> Maybe String
fastLinkerNameOf linkerName =
    case normalizeExecutableName linkerName of
        "mold" -> Just "mold"
        "ld.lld" -> Just "ld.lld"
        "lld" -> Just "ld.lld"
        _ -> Nothing

isLinkerFlagToken :: String -> Bool
isLinkerFlagToken token =
    token == "-optl"
        || token == "-pgml"
        || hasPrefixToken "-Wl," token
        || hasPrefixToken "-fuse-ld=" token
        || hasPrefixToken "--fuse-ld=" token
        || hasPrefixToken "-optl=" token
        || hasPrefixToken "-pgml=" token
        || hasAttachedToken "-optl" token
        || hasAttachedToken "-pgml" token

normalizeFuseLdTarget :: String -> Maybe String
normalizeFuseLdTarget rawName =
    case normalizeExecutableName rawName of
        "" -> Nothing
        "lld" -> Just "ld.lld"
        "gold" -> Just "ld.gold"
        "bfd" -> Just "ld"
        "ld.bfd" -> Just "ld"
        executableName -> Just executableName

normalizeLinkerCommandTarget :: String -> Maybe String
normalizeLinkerCommandTarget rawName =
    case trimToken rawName of
        "" -> Nothing
        commandName -> Just commandName

normalizeExecutableName :: String -> String
normalizeExecutableName =
    dropExeSuffix . map toLower . takeFileName . trimToken

dropExeSuffix :: String -> String
dropExeSuffix token =
    if ".exe" `isSuffixOf` token
        then take (length token - length ".exe") token
        else token

hasPrefixToken :: String -> String -> Bool
hasPrefixToken prefix =
    isPrefixOf prefix . trimToken

hasAttachedToken :: String -> String -> Bool
hasAttachedToken prefix token =
    case stripAttachedToken prefix token of
        Just _ -> True
        Nothing -> False

stripPrefixToken :: String -> String -> Maybe String
stripPrefixToken prefix token =
    let trimmed = trimToken token
     in if prefix `isPrefixOf` trimmed
            then Just (drop (length prefix) trimmed)
            else Nothing

stripAttachedToken :: String -> String -> Maybe String
stripAttachedToken prefix token =
    let trimmed = trimToken token
     in if prefix `isPrefixOf` trimmed && length trimmed > length prefix
            then Just (drop (length prefix) trimmed)
            else Nothing

trimToken :: String -> String
trimToken =
    dropWhileEnd isTrimChar . dropWhile isTrimChar
  where
    isTrimChar char =
        isSpace char || char `elem` ['"', '\'', ',', '(', ')']

splitCommaSeparated :: String -> [String]
splitCommaSeparated value =
    filter (not . null) (map trim (splitOnComma value))

splitOnComma :: String -> [String]
splitOnComma value =
    case break (== ',') value of
        (chunk, ',' : rest) -> chunk : splitOnComma rest
        (chunk, _) -> [chunk]

parsePkgConfigDependencyName :: String -> String
parsePkgConfigDependencyName spec =
    let trimmedSpec = trim spec
        name =
            takeWhile
                (\char -> not (isSpace char) && char `notElem` ['<', '>', '='])
                trimmedSpec
     in if null name then trimmedSpec else name

collectRunnableComponents :: FilePath -> Maybe String -> String -> [RunnableComponent]
collectRunnableComponents relativePath pkgName contents =
    mapMaybe (toRunnableComponent relativePath pkgName . trim) stanzaHeaders
  where
    stanzaHeaders =
        [ line
        | line <- lines contents
        , isStanzaHeader line
        ]

toRunnableComponent :: FilePath -> Maybe String -> String -> Maybe RunnableComponent
toRunnableComponent relativePath pkgName header =
    case parseRunnableHeader header of
        Just (componentType, componentName) ->
            Just
                RunnableComponent
                    { runnablePackageName = pkgName
                    , runnableFile = relativePath
                    , runnableName = componentName
                    , runnableType = componentType
                    }
        Nothing ->
            Nothing

parseRunnableHeader :: String -> Maybe (RunnableComponentType, String)
parseRunnableHeader header =
    firstMatching
        [ ("executable ", RunnableExecutable)
        , ("test-suite ", RunnableTestSuite)
        , ("benchmark ", RunnableBenchmark)
        ]
  where
    firstMatching [] = Nothing
    firstMatching ((prefix, componentType) : rest)
        | prefix `isPrefixOf` header =
            let componentName = trim (drop (length prefix) header)
             in if null componentName
                    then Nothing
                    else Just (componentType, componentName)
        | otherwise =
            firstMatching rest

isAlphaNumAscii :: Char -> Bool
isAlphaNumAscii char =
    ('a' <= char && char <= 'z')
        || ('A' <= char && char <= 'Z')
        || ('0' <= char && char <= '9')

renderHost :: HostInfo -> String
renderHost host =
    intercalate " "
        (catMaybes [label, Just ("on " <> os <> " " <> arch), wslSuffix])
  where
    label =
        case hostPrettyName host of
            Just prettyName -> Just prettyName
            Nothing ->
                hostDistroId host <|> Just os
    wslSuffix =
        if hostIsWsl host
            then Just "(WSL)"
            else Nothing

renderPackageManagers :: HostInfo -> String
renderPackageManagers host =
    case hostPackageManagers host of
        [] -> "none detected"
        managers ->
            case hostPrimaryManager host of
                Just primary ->
                    renderManager primary <> renderAlso managers primary
                Nothing ->
                    intercalate ", " (map renderManager managers)
  where
    renderAlso managers primary =
        let also = filter (/= primary) managers
         in if null also
                then ""
                else " (also detected: " <> intercalate ", " (map renderManager also) <> ")"

renderToolReport :: [ToolReport] -> ToolReport -> String
renderToolReport reports report =
    case toolPath report of
        Just path ->
            "[ok]      "
                <> pad nameWidth name
                <> " "
                <> pad versionWidth versionText
                <> " "
                <> path
        Nothing ->
            "[missing] "
                <> pad nameWidth name
                <> " "
                <> pad versionWidth "-"
                <> " not found on PATH"
  where
    name = specName (toolSpec report)
    versionText = fromMaybe "-" (toolVersion report)
    nameWidth = maximum (map (length . specName . toolSpec) reports)
    versionWidth = max 12 (maximum (map versionDisplayWidth reports))

versionDisplayWidth :: ToolReport -> Int
versionDisplayWidth report =
    length (fromMaybe "-" (toolVersion report))

renderLinkerPosture :: LinkerPosture -> [String]
renderLinkerPosture posture =
    [ "  configured by GHC: " <> fromMaybe "unknown" (linkerCommand posture)
    , "  available on PATH: " <> renderAvailableLinkers (linkerAvailable posture)
    , "  fast candidates: " <> renderAvailableLinkers (linkerFastCandidates posture)
    , "  assessment: " <> linkerAssessment posture
    ]

renderProjectDiagnostics :: ProjectDiagnostics -> [String]
renderProjectDiagnostics project =
    [ "  root: " <> projectRoot project
    , "  package files: " <> renderPackageFiles project
    , "  cabal.project files: " <> renderProjectFiles project
    , "  pkg-config declarations: " <> renderPkgConfigUsage project
    , "  explicit linker selections: " <> renderConfiguredProjectLinkers project
    , "  linker wiring: " <> renderProjectLinkerHints project
    ]

renderPackageFiles :: ProjectDiagnostics -> String
renderPackageFiles project =
    case projectPackageReports project of
        [] -> "none"
        reports -> intercalate ", " (map packageFile reports)

renderProjectFiles :: ProjectDiagnostics -> String
renderProjectFiles project =
    case projectConfigFiles project of
        [] -> "none"
        files -> intercalate ", " files

renderPkgConfigUsage :: ProjectDiagnostics -> String
renderPkgConfigUsage project =
    case pkgConfigPackages project of
        [] -> "none found"
        packages ->
            intercalate "; " (map renderPackagePkgConfig packages)

renderPackagePkgConfig :: PackageReport -> String
renderPackagePkgConfig packageReport =
    renderPackageLabel packageReport
        <> " -> "
        <> intercalate ", " (map renderPkgConfigDependency (packagePkgConfigDepends packageReport))

renderPkgConfigDependency :: PkgConfigDependencyReport -> String
renderPkgConfigDependency dependency =
    pkgConfigSpec dependency
        <> " ["
        <> renderPkgConfigProbe (pkgConfigProbe dependency)
        <> "]"

renderPkgConfigProbe :: PkgConfigProbe -> String
renderPkgConfigProbe probe =
    case probe of
        PkgConfigToolMissing -> "tool missing"
        PkgConfigResolved version -> "ok " <> version
        PkgConfigUnresolved -> "not resolved"

renderProjectLinkerHints :: ProjectDiagnostics -> String
renderProjectLinkerHints project =
    case projectLinkerHints project of
        [] -> "no structured linker setting found"
        hints ->
            intercalate "; " (map renderProjectLinkerHint hints)

renderConfiguredProjectLinkers :: ProjectDiagnostics -> String
renderConfiguredProjectLinkers project =
    case projectConfiguredLinkers project of
        [] -> "none found"
        linkers ->
            intercalate ", " (map renderConfiguredProjectLinker linkers)

renderConfiguredProjectLinker :: ConfiguredProjectLinker -> String
renderConfiguredProjectLinker configuredLinker =
    configuredLinkerName configuredLinker
        <> " ["
        <> intercalate ", " (catMaybes [fastTag, availabilityTag])
        <> "]"
  where
    fastTag =
        if configuredLinkerFast configuredLinker
            then Just "fast"
            else Nothing
    availabilityTag =
        Just
            ( if configuredLinkerAvailable configuredLinker
                then "ok"
                else "missing"
            )

renderProjectLinkerHint :: ProjectLinkerHint -> String
renderProjectLinkerHint hint =
    linkerHintFile hint
        <> ":"
        <> show (linkerHintLineNumber hint)
        <> renderLinkerScope hint
        <> " "
        <> linkerHintField hint
        <> " "
        <> renderLinkerHintKind hint

renderLinkerScope :: ProjectLinkerHint -> String
renderLinkerScope hint =
    case linkerHintScope hint of
        Just scope -> " [" <> scope <> "]"
        Nothing -> ""

renderLinkerHintKind :: ProjectLinkerHint -> String
renderLinkerHintKind hint =
    case linkerHintKind hint of
        LinkerSelectsFastLinker linkerName ->
            "selects fast linker `"
                <> linkerName
                <> "` via `"
                <> linkerHintValue hint
                <> "`"
        LinkerSelectsLinkerCommand linkerName ->
            "selects linker `"
                <> linkerName
                <> "` via `"
                <> linkerHintValue hint
                <> "`"
        LinkerUsesLinkerFlags ->
            "passes linker flags via `"
                <> linkerHintValue hint
                <> "`"

renderAvailableLinkers :: [ToolReport] -> String
renderAvailableLinkers reports =
    case reports of
        [] -> "none"
        xs -> intercalate ", " (map (specName . toolSpec) xs)

linkerAssessment :: LinkerPosture -> String
linkerAssessment posture
    | not (null (linkerFastCandidates posture)) =
        "fast linker available; project-level linker wiring is the next step"
    | null (linkerAvailable posture) =
        "no linker detected on PATH; builds are likely to fail"
    | linkerIsGnuLd posture == Just True =
        "GNU ld only; builds can succeed, but link steps will usually be slower"
    | otherwise =
        "usable linker present, but no fast linker candidate detected"

summarizeStatus :: [ToolReport] -> LinkerPosture -> ProjectDiagnostics -> String
summarizeStatus reports posture project
    | not (null blockers) =
        "blocked by missing required tools: " <> intercalate ", " blockers
    | projectNeedsMissingConfiguredLinker project =
        "current project selects a linker command that is not available on this host"
    | pkgConfigMissing && projectUsesPkgConfig project && null (linkerFastCandidates posture) =
        "current project declares pkg-config dependencies, and fast-linker wiring is still missing"
    | pkgConfigMissing && projectUsesPkgConfig project =
        "current project declares pkg-config dependencies, but pkg-config is missing"
    | projectHasUnresolvedPkgConfig project && null (linkerFastCandidates posture) =
        "current project still has unresolved pkg-config libraries, and fast-linker wiring is still missing"
    | projectHasUnresolvedPkgConfig project =
        "current project has unresolved pkg-config libraries on this host"
    | projectHasExplicitNonFastLinkerSelection project =
        "toolchain is usable; current project explicitly selects its own linker command"
    | projectHasExplicitLinkerSelection project =
        "toolchain and project-level linker wiring look aligned"
    | projectHasGenericLinkerFlags project && not (projectHasExplicitLinkerSelection project) && null (linkerFastCandidates posture) =
        "current project already passes linker flags, but no fast-linker selection is configured"
    | projectHasGenericLinkerFlags project && not (projectHasExplicitLinkerSelection project) =
        "current project already passes linker flags, but none select a fast linker"
    | pkgConfigMissing && null (linkerFastCandidates posture) =
        "toolchain is usable for this project, but optional native-library support and fast linking need attention"
    | pkgConfigMissing =
        "toolchain is usable for this project; pkg-config is absent, but no pkg-config dependencies are declared here"
    | projectUsesPkgConfig project && null (linkerFastCandidates posture) && not (projectHasExplicitLinkerSelection project) =
        "current project's pkg-config libraries resolve; project-level fast-linker wiring is the main obvious improvement"
    | projectUsesPkgConfig project =
        "current project's pkg-config libraries resolve on this host"
    | not (projectHasExplicitLinkerSelection project) && null (linkerFastCandidates posture) =
        "toolchain is usable; project-level fast-linker wiring is the main obvious improvement"
    | null (linkerFastCandidates posture) =
        "toolchain is usable; faster linking is the main obvious improvement"
    | otherwise =
        "toolchain looks healthy for normal cabal work"
  where
    blockers =
        [ specName (toolSpec report)
        | report <- reports
        , specRole (toolSpec report) == Required
        , isNothing (toolPath report)
        ]
    pkgConfigMissing = missingTool "pkg-config" reports

makeAdvice :: HostInfo -> [ToolReport] -> LinkerPosture -> ProjectDiagnostics -> [String]
makeAdvice host reports posture project =
    concat
        [ coreToolAdvice host reports
        , pkgConfigAdvice host reports project
        , linkerAdvice host reports posture project
        ]

coreToolAdvice :: HostInfo -> [ToolReport] -> [String]
coreToolAdvice host reports =
    catMaybes
        [ if missingTool "ghc" reports
            then Just (installViaGhcupOrPackageManager host "ghc" "Install GHC before trying to build Haskell code.")
            else Nothing
        , if missingTool "cabal" reports
            then Just (installViaGhcupOrPackageManager host "cabal" "Install cabal-install so hx can drive builds and runs.")
            else Nothing
        , if missingTool "ghcup" reports
            then Just "ghcup is missing. It is the simplest way to manage matching GHC and cabal versions on a workstation."
            else Nothing
        ]

pkgConfigAdvice :: HostInfo -> [ToolReport] -> ProjectDiagnostics -> [String]
pkgConfigAdvice host reports project =
    if missingTool "pkg-config" reports
        then
            if projectUsesPkgConfig project
                then
                    [ "pkg-config is missing, and the current project declares "
                        <> renderPkgConfigTargets project
                        <> ". Install `pkg-config` first, then the matching system development packages."
                        <> packageInstallSuffix host "pkg-config"
                    ]
                        ++ map (renderPkgConfigInstallAdvice host) (projectPkgConfigDependencies project)
                else
                    [ "pkg-config is missing. The current project does not declare `pkgconfig-depends`, but packages with C libraries often need it."
                        <> packageInstallSuffix host "pkg-config"
                    ]
        else
            map (renderPkgConfigResolutionAdvice host) (unresolvedPkgConfigDependencies project)

linkerAdvice :: HostInfo -> [ToolReport] -> LinkerPosture -> ProjectDiagnostics -> [String]
linkerAdvice host reports posture project
    | null (linkerAvailable posture) =
        ["No linker was detected on PATH. Install binutils or an LLVM linker before attempting builds." <> packageInstallSuffix host "ld"]
    | projectNeedsMissingConfiguredLinker project =
        case firstMissingConfiguredProjectLinker project of
            Just linkerName ->
                [ "The current project already mentions `"
                    <> linkerName
                    <> "`, but that command is not available on this host. Install it or remove the override from the project files."
                    <> explicitLinkerInstallSuffix host linkerName
                ]
            Nothing -> []
    | projectHasExplicitNonFastLinkerSelection project && not (null (linkerFastCandidates posture)) =
        [ "A fast linker is installed, but the current project explicitly selects "
            <> renderQuotedNames (configuredProjectNonFastLinkers project)
            <> ". `hx build` and `hx run` will respect that selection instead of injecting `mold` or `ld.lld`."
        ]
    | projectHasExplicitNonFastLinkerSelection project =
        [ "The current project explicitly selects "
            <> renderQuotedNames (configuredProjectNonFastLinkers project)
            <> ". `hx build` and `hx run` will respect that linker command."
        ]
    | projectHasGenericLinkerFlags project && not (projectHasExplicitLinkerSelection project) && not (null (linkerFastCandidates posture)) =
        [ "The current project already passes linker flags, but none of the detected settings select `mold` or `ld.lld`. Extend the existing Cabal linker options instead of adding a separate ad hoc override." ]
    | not (null (linkerFastCandidates posture)) =
        case (linkerCommand posture, configuredProjectFastLinkers project, configuredProjectLinkers project) of
            (Just "ld", [], []) ->
                ["A fast linker is installed, but neither GHC nor the current project selects it yet. Wire `mold` or `ld.lld` through Cabal project settings to use it."]
            _ -> []
    | otherwise =
        [ "No fast linker candidate was found. Installing `mold` or `ld.lld` usually shortens Haskell link times."
            <> fastLinkerInstallSuffix host reports
        ]

projectUsesPkgConfig :: ProjectDiagnostics -> Bool
projectUsesPkgConfig =
    not . null . pkgConfigPackages

pkgConfigPackages :: ProjectDiagnostics -> [PackageReport]
pkgConfigPackages project =
    filter (not . null . packagePkgConfigDepends) (projectPackageReports project)

renderPkgConfigTargets :: ProjectDiagnostics -> String
renderPkgConfigTargets project =
    intercalate "; " (map renderPackagePkgConfig (pkgConfigPackages project))

renderPackageLabel :: PackageReport -> String
renderPackageLabel packageReport =
    case packageName packageReport of
        Just name -> name <> " (" <> packageFile packageReport <> ")"
        Nothing -> packageFile packageReport

configuredProjectFastLinkers :: ProjectDiagnostics -> [String]
configuredProjectFastLinkers project =
    [ configuredLinkerName configuredLinker
    | configuredLinker <- projectConfiguredLinkers project
    , configuredLinkerFast configuredLinker
    ]

configuredProjectLinkers :: ProjectDiagnostics -> [String]
configuredProjectLinkers project =
    map configuredLinkerName (projectConfiguredLinkers project)

preferredFastLinkerName :: DiagnosticSnapshot -> Maybe String
preferredFastLinkerName snapshot =
    listToMaybe
        [ linkerName
        | linkerName <- ["mold", "ld.lld"]
        , linkerName `elem` availableFastLinkers
        ]
  where
    availableFastLinkers =
        map (specName . toolSpec) (linkerFastCandidates (snapshotLinkerPosture snapshot))

defaultRunnableTargetArg :: DiagnosticSnapshot -> Maybe String
defaultRunnableTargetArg snapshot =
    case executableComponents of
        [component] -> Just (runnableName component)
        [] ->
            case runnableComponents of
                [component] -> Just (runnableName component)
                _ -> Nothing
        _ -> Nothing
  where
    runnableComponents =
        projectRunnableComponents (snapshotProjectDiagnostics snapshot)
    executableComponents =
        filter ((== RunnableExecutable) . runnableType) runnableComponents

renderRunnableTargets :: DiagnosticSnapshot -> String
renderRunnableTargets snapshot =
    case projectRunnableComponents (snapshotProjectDiagnostics snapshot) of
        [] -> "none"
        components -> intercalate ", " (map renderRunnableComponentTarget components)

buildPkgConfigBlocker :: DiagnosticSnapshot -> Maybe String
buildPkgConfigBlocker snapshot
    | missingTool "pkg-config" (snapshotReports snapshot) && projectUsesPkgConfig project =
        Just
            ( "The current project declares pkg-config dependencies, but `pkg-config` is not on PATH.\n\n"
                <> "Project declarations: "
                <> renderPkgConfigTargets project
                <> "\n\n"
                <> intercalate "\n" (map (renderPkgConfigInstallAdvice host) (projectPkgConfigDependencies project))
                <> "\n\nRun `hx doctor` for more detail."
            )
    | projectHasUnresolvedPkgConfig project =
        Just
            ( "The current project has unresolved pkg-config libraries.\n\n"
                <> intercalate "\n" (map (renderPkgConfigResolutionAdvice host) (unresolvedPkgConfigDependencies project))
                <> "\n\nRun `hx doctor` for more detail."
            )
    | otherwise =
        Nothing
  where
    host = snapshotHostInfo snapshot
    project = snapshotProjectDiagnostics snapshot

buildPkgConfigWarnings :: DiagnosticSnapshot -> [String]
buildPkgConfigWarnings snapshot
    | missingTool "pkg-config" (snapshotReports snapshot) && not (projectUsesPkgConfig project) =
        ["pkg-config is missing on PATH, but the current project does not declare `pkgconfig-depends`."]
    | otherwise =
        []
  where
    project = snapshotProjectDiagnostics snapshot

projectRunnableComponents :: ProjectDiagnostics -> [RunnableComponent]
projectRunnableComponents project =
    concatMap packageRunnableComponents (projectPackageReports project)

renderRunnableComponentTarget :: RunnableComponent -> String
renderRunnableComponentTarget component =
    case runnablePackageName component of
        Just packageName ->
            packageName
                <> ":"
                <> runnableName component
                <> " ("
                <> renderRunnableType (runnableType component)
                <> " in "
                <> runnableFile component
                <> ")"
        Nothing ->
            runnableName component
                <> " ("
                <> renderRunnableType (runnableType component)
                <> " in "
                <> runnableFile component
                <> ")"

renderRunnableType :: RunnableComponentType -> String
renderRunnableType componentType =
    case componentType of
        RunnableExecutable -> "executable"
        RunnableTestSuite -> "test-suite"
        RunnableBenchmark -> "benchmark"

projectHasGenericLinkerFlags :: ProjectDiagnostics -> Bool
projectHasGenericLinkerFlags project =
    any isGenericLinkerHint (projectLinkerHints project)

projectHasExplicitLinkerSelection :: ProjectDiagnostics -> Bool
projectHasExplicitLinkerSelection =
    not . null . configuredProjectLinkers

isGenericLinkerHint :: ProjectLinkerHint -> Bool
isGenericLinkerHint hint =
    linkerHintKind hint == LinkerUsesLinkerFlags

projectPkgConfigDependencies :: ProjectDiagnostics -> [PkgConfigDependencyReport]
projectPkgConfigDependencies project =
    uniquePkgConfigDependencies $
        concatMap packagePkgConfigDepends (pkgConfigPackages project)

projectHasUnresolvedPkgConfig :: ProjectDiagnostics -> Bool
projectHasUnresolvedPkgConfig =
    not . null . unresolvedPkgConfigDependencies

unresolvedPkgConfigDependencies :: ProjectDiagnostics -> [PkgConfigDependencyReport]
unresolvedPkgConfigDependencies project =
    filter ((== PkgConfigUnresolved) . pkgConfigProbe) (projectPkgConfigDependencies project)

projectNeedsMissingConfiguredLinker :: ProjectDiagnostics -> Bool
projectNeedsMissingConfiguredLinker =
    any (not . configuredLinkerAvailable) . projectConfiguredLinkers

projectHasExplicitNonFastLinkerSelection :: ProjectDiagnostics -> Bool
projectHasExplicitNonFastLinkerSelection =
    not . null . configuredProjectNonFastLinkers

configuredProjectNonFastLinkers :: ProjectDiagnostics -> [String]
configuredProjectNonFastLinkers project =
    [ configuredLinkerName configuredLinker
    | configuredLinker <- projectConfiguredLinkers project
    , not (configuredLinkerFast configuredLinker)
    ]

firstMissingConfiguredProjectLinker :: ProjectDiagnostics -> Maybe String
firstMissingConfiguredProjectLinker project =
    configuredLinkerName <$> find (not . configuredLinkerAvailable) (projectConfiguredLinkers project)

renderQuotedNames :: [String] -> String
renderQuotedNames names =
    intercalate ", " (map (\name -> "`" <> name <> "`") names)

toolCommandPath :: String -> [ToolReport] -> Maybe FilePath
toolCommandPath name reports =
    toolPath =<< find (\report -> specName (toolSpec report) == name) reports

uniquePkgConfigDependencies :: [PkgConfigDependencyReport] -> [PkgConfigDependencyReport]
uniquePkgConfigDependencies dependencies =
    map pickFirst uniqueNames
  where
    uniqueNames = nub (map pkgConfigName dependencies)
    pickFirst dependencyName =
        fromMaybe
            (PkgConfigDependencyReport dependencyName dependencyName PkgConfigUnresolved)
            (find (\dependency -> pkgConfigName dependency == dependencyName) dependencies)

renderPkgConfigInstallAdvice :: HostInfo -> PkgConfigDependencyReport -> String
renderPkgConfigInstallAdvice host dependency =
    "For `"
        <> pkgConfigName dependency
        <> "`, likely system packages are "
        <> renderLikelySystemPackages host dependency
        <> "."

renderPkgConfigResolutionAdvice :: HostInfo -> PkgConfigDependencyReport -> String
renderPkgConfigResolutionAdvice host dependency =
    "pkg-config cannot resolve `"
        <> pkgConfigName dependency
        <> "` in the current project."
        <> renderLikelyInstallCommand host dependency

renderLikelySystemPackages :: HostInfo -> PkgConfigDependencyReport -> String
renderLikelySystemPackages host dependency =
    case hostPrimaryManager host of
        Just manager ->
            let packages = likelySystemPackages manager (pkgConfigName dependency)
             in if null packages
                    then "`" <> pkgConfigName dependency <> "`"
                    else intercalate ", " (map (\packageName -> "`" <> packageName <> "`") packages)
        Nothing -> "`" <> pkgConfigName dependency <> "`"

renderLikelyInstallCommand :: HostInfo -> PkgConfigDependencyReport -> String
renderLikelyInstallCommand host dependency =
    case hostPrimaryManager host of
        Just manager ->
            let packages = likelySystemPackages manager (pkgConfigName dependency)
             in case packages of
                    [] -> ""
                    xs ->
                        " Try "
                            <> intercalate
                                " or "
                                (map (\packageName -> "`" <> installCommand manager packageName <> "`") xs)
                            <> "."
        Nothing -> ""

likelySystemPackages :: PackageManager -> String -> [String]
likelySystemPackages manager dependencyName =
    case pkgConfigPackageKey dependencyName of
        Just packageKey ->
            systemPackagesFor manager packageKey
        Nothing ->
            []

pkgConfigPackageKey :: String -> Maybe String
pkgConfigPackageKey dependencyName =
    case normalizePkgConfigName dependencyName of
        "libpq" -> Just "postgresql"
        "openssl" -> Just "openssl"
        "libssl" -> Just "openssl"
        "libcrypto" -> Just "openssl"
        "zlib" -> Just "zlib"
        "sqlite" -> Just "sqlite"
        "sqlite3" -> Just "sqlite"
        "libffi" -> Just "libffi"
        "gmp" -> Just "gmp"
        "curl" -> Just "curl"
        "libcurl" -> Just "curl"
        "icu-i18n" -> Just "icu"
        "icu-uc" -> Just "icu"
        "icu-io" -> Just "icu"
        "ncurses" -> Just "ncurses"
        "tinfo" -> Just "ncurses"
        "pcre2-8" -> Just "pcre2"
        "libsodium" -> Just "libsodium"
        "liblzma" -> Just "xz"
        "xz" -> Just "xz"
        "glib-2.0" -> Just "glib"
        "gobject-2.0" -> Just "glib"
        "gio-2.0" -> Just "glib"
        "gio-unix-2.0" -> Just "glib"
        "gmodule-2.0" -> Just "glib"
        "gthread-2.0" -> Just "glib"
        "gtk+-3.0" -> Just "gtk3"
        "gdk-3.0" -> Just "gtk3"
        "gtk4" -> Just "gtk4"
        "gdk-4.0" -> Just "gtk4"
        "gdk-pixbuf-2.0" -> Just "gdk-pixbuf"
        "atk" -> Just "atk"
        "pango" -> Just "pango"
        "pangocairo" -> Just "pango"
        "pangoft2" -> Just "pango"
        "cairo" -> Just "cairo"
        "cairo-gobject" -> Just "cairo"
        "freetype2" -> Just "freetype"
        "fontconfig" -> Just "fontconfig"
        "libxml-2.0" -> Just "libxml2"
        "libxslt" -> Just "libxslt"
        "yaml-0.1" -> Just "libyaml"
        "libyaml" -> Just "libyaml"
        "libuv" -> Just "libuv"
        "libevent" -> Just "libevent"
        "libzmq" -> Just "zeromq"
        "zeromq" -> Just "zeromq"
        "hiredis" -> Just "hiredis"
        "x11" -> Just "x11"
        "xext" -> Just "xext"
        "xrender" -> Just "xrender"
        "xrandr" -> Just "xrandr"
        "xinerama" -> Just "xinerama"
        "xcursor" -> Just "xcursor"
        "xi" -> Just "xi"
        "xfixes" -> Just "xfixes"
        normalizedName
            | "libevent" `isPrefixOf` normalizedName ->
                Just "libevent"
            | otherwise ->
                Nothing

systemPackagesFor :: PackageManager -> String -> [String]
systemPackagesFor manager packageKey =
    case (manager, packageKey) of
        (Apt, "postgresql") -> ["libpq-dev"]
        (Apt, "openssl") -> ["libssl-dev"]
        (Apt, "zlib") -> ["zlib1g-dev"]
        (Apt, "sqlite") -> ["libsqlite3-dev"]
        (Apt, "libffi") -> ["libffi-dev"]
        (Apt, "gmp") -> ["libgmp-dev"]
        (Apt, "curl") -> ["libcurl4-openssl-dev"]
        (Apt, "icu") -> ["libicu-dev"]
        (Apt, "ncurses") -> ["libncurses-dev"]
        (Apt, "pcre2") -> ["libpcre2-dev"]
        (Apt, "libsodium") -> ["libsodium-dev"]
        (Apt, "xz") -> ["liblzma-dev"]
        (Apt, "glib") -> ["libglib2.0-dev"]
        (Apt, "gtk3") -> ["libgtk-3-dev"]
        (Apt, "gtk4") -> ["libgtk-4-dev"]
        (Apt, "gdk-pixbuf") -> ["libgdk-pixbuf-2.0-dev"]
        (Apt, "atk") -> ["libatk1.0-dev"]
        (Apt, "pango") -> ["libpango1.0-dev"]
        (Apt, "cairo") -> ["libcairo2-dev"]
        (Apt, "freetype") -> ["libfreetype-dev"]
        (Apt, "fontconfig") -> ["libfontconfig-dev"]
        (Apt, "libxml2") -> ["libxml2-dev"]
        (Apt, "libxslt") -> ["libxslt1-dev"]
        (Apt, "libyaml") -> ["libyaml-dev"]
        (Apt, "libuv") -> ["libuv1-dev"]
        (Apt, "libevent") -> ["libevent-dev"]
        (Apt, "zeromq") -> ["libzmq3-dev"]
        (Apt, "hiredis") -> ["libhiredis-dev"]
        (Apt, "x11") -> ["libx11-dev"]
        (Apt, "xext") -> ["libxext-dev"]
        (Apt, "xrender") -> ["libxrender-dev"]
        (Apt, "xrandr") -> ["libxrandr-dev"]
        (Apt, "xinerama") -> ["libxinerama-dev"]
        (Apt, "xcursor") -> ["libxcursor-dev"]
        (Apt, "xi") -> ["libxi-dev"]
        (Apt, "xfixes") -> ["libxfixes-dev"]
        (Brew, "postgresql") -> ["libpq"]
        (Brew, "openssl") -> ["openssl@3"]
        (Brew, "zlib") -> ["zlib"]
        (Brew, "sqlite") -> ["sqlite"]
        (Brew, "libffi") -> ["libffi"]
        (Brew, "gmp") -> ["gmp"]
        (Brew, "curl") -> ["curl"]
        (Brew, "icu") -> ["icu4c"]
        (Brew, "ncurses") -> ["ncurses"]
        (Brew, "pcre2") -> ["pcre2"]
        (Brew, "libsodium") -> ["libsodium"]
        (Brew, "xz") -> ["xz"]
        (Brew, "glib") -> ["glib"]
        (Brew, "gtk3") -> ["gtk+3"]
        (Brew, "gtk4") -> ["gtk4"]
        (Brew, "gdk-pixbuf") -> ["gdk-pixbuf"]
        (Brew, "atk") -> ["atk"]
        (Brew, "pango") -> ["pango"]
        (Brew, "cairo") -> ["cairo"]
        (Brew, "freetype") -> ["freetype"]
        (Brew, "fontconfig") -> ["fontconfig"]
        (Brew, "libxml2") -> ["libxml2"]
        (Brew, "libxslt") -> ["libxslt"]
        (Brew, "libyaml") -> ["libyaml"]
        (Brew, "libuv") -> ["libuv"]
        (Brew, "libevent") -> ["libevent"]
        (Brew, "zeromq") -> ["zeromq"]
        (Brew, "hiredis") -> ["hiredis"]
        (Brew, "x11") -> ["libx11"]
        (Brew, "xext") -> ["libxext"]
        (Brew, "xrender") -> ["libxrender"]
        (Brew, "xrandr") -> ["libxrandr"]
        (Brew, "xinerama") -> ["libxinerama"]
        (Brew, "xcursor") -> ["libxcursor"]
        (Brew, "xi") -> ["libxi"]
        (Brew, "xfixes") -> ["libxfixes"]
        (Pacman, "postgresql") -> ["postgresql-libs"]
        (Pacman, "openssl") -> ["openssl"]
        (Pacman, "zlib") -> ["zlib"]
        (Pacman, "sqlite") -> ["sqlite"]
        (Pacman, "libffi") -> ["libffi"]
        (Pacman, "gmp") -> ["gmp"]
        (Pacman, "curl") -> ["curl"]
        (Pacman, "icu") -> ["icu"]
        (Pacman, "ncurses") -> ["ncurses"]
        (Pacman, "pcre2") -> ["pcre2"]
        (Pacman, "libsodium") -> ["libsodium"]
        (Pacman, "xz") -> ["xz"]
        (Pacman, "glib") -> ["glib2"]
        (Pacman, "gtk3") -> ["gtk3"]
        (Pacman, "gtk4") -> ["gtk4"]
        (Pacman, "gdk-pixbuf") -> ["gdk-pixbuf2"]
        (Pacman, "atk") -> ["atk"]
        (Pacman, "pango") -> ["pango"]
        (Pacman, "cairo") -> ["cairo"]
        (Pacman, "freetype") -> ["freetype2"]
        (Pacman, "fontconfig") -> ["fontconfig"]
        (Pacman, "libxml2") -> ["libxml2"]
        (Pacman, "libxslt") -> ["libxslt"]
        (Pacman, "libyaml") -> ["libyaml"]
        (Pacman, "libuv") -> ["libuv"]
        (Pacman, "libevent") -> ["libevent"]
        (Pacman, "zeromq") -> ["zeromq"]
        (Pacman, "hiredis") -> ["hiredis"]
        (Pacman, "x11") -> ["libx11"]
        (Pacman, "xext") -> ["libxext"]
        (Pacman, "xrender") -> ["libxrender"]
        (Pacman, "xrandr") -> ["libxrandr"]
        (Pacman, "xinerama") -> ["libxinerama"]
        (Pacman, "xcursor") -> ["libxcursor"]
        (Pacman, "xi") -> ["libxi"]
        (Pacman, "xfixes") -> ["libxfixes"]
        (Dnf, "postgresql") -> ["postgresql-devel"]
        (Dnf, "openssl") -> ["openssl-devel"]
        (Dnf, "zlib") -> ["zlib-devel"]
        (Dnf, "sqlite") -> ["sqlite-devel"]
        (Dnf, "libffi") -> ["libffi-devel"]
        (Dnf, "gmp") -> ["gmp-devel"]
        (Dnf, "curl") -> ["libcurl-devel"]
        (Dnf, "icu") -> ["libicu-devel"]
        (Dnf, "ncurses") -> ["ncurses-devel"]
        (Dnf, "pcre2") -> ["pcre2-devel"]
        (Dnf, "libsodium") -> ["libsodium-devel"]
        (Dnf, "xz") -> ["xz-devel"]
        (Dnf, "glib") -> ["glib2-devel"]
        (Dnf, "gtk3") -> ["gtk3-devel"]
        (Dnf, "gtk4") -> ["gtk4-devel"]
        (Dnf, "gdk-pixbuf") -> ["gdk-pixbuf2-devel"]
        (Dnf, "atk") -> ["atk-devel"]
        (Dnf, "pango") -> ["pango-devel"]
        (Dnf, "cairo") -> ["cairo-devel"]
        (Dnf, "freetype") -> ["freetype-devel"]
        (Dnf, "fontconfig") -> ["fontconfig-devel"]
        (Dnf, "libxml2") -> ["libxml2-devel"]
        (Dnf, "libxslt") -> ["libxslt-devel"]
        (Dnf, "libyaml") -> ["libyaml-devel"]
        (Dnf, "libuv") -> ["libuv-devel"]
        (Dnf, "libevent") -> ["libevent-devel"]
        (Dnf, "zeromq") -> ["zeromq-devel"]
        (Dnf, "hiredis") -> ["hiredis-devel"]
        (Dnf, "x11") -> ["libX11-devel"]
        (Dnf, "xext") -> ["libXext-devel"]
        (Dnf, "xrender") -> ["libXrender-devel"]
        (Dnf, "xrandr") -> ["libXrandr-devel"]
        (Dnf, "xinerama") -> ["libXinerama-devel"]
        (Dnf, "xcursor") -> ["libXcursor-devel"]
        (Dnf, "xi") -> ["libXi-devel"]
        (Dnf, "xfixes") -> ["libXfixes-devel"]
        (Yum, key) -> systemPackagesFor Dnf key
        (Zypper, "postgresql") -> ["postgresql-devel"]
        (Zypper, "openssl") -> ["libopenssl-devel"]
        (Zypper, "zlib") -> ["zlib-devel"]
        (Zypper, "sqlite") -> ["sqlite3-devel"]
        (Zypper, "libffi") -> ["libffi-devel"]
        (Zypper, "gmp") -> ["gmp-devel"]
        (Zypper, "curl") -> ["libcurl-devel"]
        (Zypper, "icu") -> ["libicu-devel"]
        (Zypper, "ncurses") -> ["ncurses-devel"]
        (Zypper, "pcre2") -> ["pcre2-devel"]
        (Zypper, "libsodium") -> ["libsodium-devel"]
        (Zypper, "xz") -> ["xz-devel"]
        (Zypper, "glib") -> ["glib2-devel"]
        (Zypper, "gtk3") -> ["gtk3-devel"]
        (Zypper, "gtk4") -> ["gtk4-devel"]
        (Zypper, "gdk-pixbuf") -> ["gdk-pixbuf-devel"]
        (Zypper, "atk") -> ["atk-devel"]
        (Zypper, "pango") -> ["pango-devel"]
        (Zypper, "cairo") -> ["cairo-devel"]
        (Zypper, "freetype") -> ["freetype2-devel"]
        (Zypper, "fontconfig") -> ["fontconfig-devel"]
        (Zypper, "libxml2") -> ["libxml2-devel"]
        (Zypper, "libxslt") -> ["libxslt-devel"]
        (Zypper, "libyaml") -> ["libyaml-devel"]
        (Zypper, "libuv") -> ["libuv-devel"]
        (Zypper, "libevent") -> ["libevent-devel"]
        (Zypper, "zeromq") -> ["zeromq-devel"]
        (Zypper, "hiredis") -> ["hiredis-devel"]
        (Zypper, "x11") -> ["libX11-devel"]
        (Zypper, "xext") -> ["libXext-devel"]
        (Zypper, "xrender") -> ["libXrender-devel"]
        (Zypper, "xrandr") -> ["libXrandr-devel"]
        (Zypper, "xinerama") -> ["libXinerama-devel"]
        (Zypper, "xcursor") -> ["libXcursor-devel"]
        (Zypper, "xi") -> ["libXi-devel"]
        (Zypper, "xfixes") -> ["libXfixes-devel"]
        (Apk, "postgresql") -> ["postgresql-dev"]
        (Apk, "openssl") -> ["openssl-dev"]
        (Apk, "zlib") -> ["zlib-dev"]
        (Apk, "sqlite") -> ["sqlite-dev"]
        (Apk, "libffi") -> ["libffi-dev"]
        (Apk, "gmp") -> ["gmp-dev"]
        (Apk, "curl") -> ["curl-dev"]
        (Apk, "icu") -> ["icu-dev"]
        (Apk, "ncurses") -> ["ncurses-dev"]
        (Apk, "pcre2") -> ["pcre2-dev"]
        (Apk, "libsodium") -> ["libsodium-dev"]
        (Apk, "xz") -> ["xz-dev"]
        (Apk, "glib") -> ["glib-dev"]
        (Apk, "gtk3") -> ["gtk+3.0-dev"]
        (Apk, "gtk4") -> ["gtk4.0-dev"]
        (Apk, "gdk-pixbuf") -> ["gdk-pixbuf-dev"]
        (Apk, "atk") -> ["atk-dev"]
        (Apk, "pango") -> ["pango-dev"]
        (Apk, "cairo") -> ["cairo-dev"]
        (Apk, "freetype") -> ["freetype-dev"]
        (Apk, "fontconfig") -> ["fontconfig-dev"]
        (Apk, "libxml2") -> ["libxml2-dev"]
        (Apk, "libxslt") -> ["libxslt-dev"]
        (Apk, "libyaml") -> ["yaml-dev"]
        (Apk, "libuv") -> ["libuv-dev"]
        (Apk, "libevent") -> ["libevent-dev"]
        (Apk, "zeromq") -> ["zeromq-dev"]
        (Apk, "hiredis") -> ["hiredis-dev"]
        (Apk, "x11") -> ["libx11-dev"]
        (Apk, "xext") -> ["libxext-dev"]
        (Apk, "xrender") -> ["libxrender-dev"]
        (Apk, "xrandr") -> ["libxrandr-dev"]
        (Apk, "xinerama") -> ["libxinerama-dev"]
        (Apk, "xcursor") -> ["libxcursor-dev"]
        (Apk, "xi") -> ["libxi-dev"]
        (Apk, "xfixes") -> ["libxfixes-dev"]
        (Nix, "postgresql") -> ["postgresql"]
        (Nix, "openssl") -> ["openssl"]
        (Nix, "zlib") -> ["zlib"]
        (Nix, "sqlite") -> ["sqlite"]
        (Nix, "libffi") -> ["libffi"]
        (Nix, "gmp") -> ["gmp"]
        (Nix, "curl") -> ["curl"]
        (Nix, "icu") -> ["icu"]
        (Nix, "ncurses") -> ["ncurses"]
        (Nix, "pcre2") -> ["pcre2"]
        (Nix, "libsodium") -> ["libsodium"]
        (Nix, "xz") -> ["xz"]
        (Nix, "glib") -> ["glib"]
        (Nix, "gtk3") -> ["gtk3"]
        (Nix, "gtk4") -> ["gtk4"]
        (Nix, "gdk-pixbuf") -> ["gdk-pixbuf"]
        (Nix, "atk") -> ["atk"]
        (Nix, "pango") -> ["pango"]
        (Nix, "cairo") -> ["cairo"]
        (Nix, "freetype") -> ["freetype"]
        (Nix, "fontconfig") -> ["fontconfig"]
        (Nix, "libxml2") -> ["libxml2"]
        (Nix, "libxslt") -> ["libxslt"]
        (Nix, "libyaml") -> ["libyaml"]
        (Nix, "libuv") -> ["libuv"]
        (Nix, "libevent") -> ["libevent"]
        (Nix, "zeromq") -> ["zeromq"]
        (Nix, "hiredis") -> ["hiredis"]
        (Nix, "x11") -> ["xorg.libX11"]
        (Nix, "xext") -> ["xorg.libXext"]
        (Nix, "xrender") -> ["xorg.libXrender"]
        (Nix, "xrandr") -> ["xorg.libXrandr"]
        (Nix, "xinerama") -> ["xorg.libXinerama"]
        (Nix, "xcursor") -> ["xorg.libXcursor"]
        (Nix, "xi") -> ["xorg.libXi"]
        (Nix, "xfixes") -> ["xorg.libXfixes"]
        _ -> []

normalizePkgConfigName :: String -> String
normalizePkgConfigName =
    map toLower . trim

missingTool :: String -> [ToolReport] -> Bool
missingTool name reports =
    any
        (\report -> specName (toolSpec report) == name && isNothing (toolPath report))
        reports

installViaGhcupOrPackageManager :: HostInfo -> String -> String -> String
installViaGhcupOrPackageManager host toolName prefix =
    prefix
        <> " "
        <> case toolName of
            "ghc" ->
                "If ghcup is available, use `ghcup install ghc recommended`."
                    <> fallbackInstall host toolName
            "cabal" ->
                "If ghcup is available, use `ghcup install cabal recommended`."
                    <> fallbackInstall host toolName
            _ -> "Install it with your package manager."

fallbackInstall :: HostInfo -> String -> String
fallbackInstall host toolName =
    case hostPrimaryManager host of
        Just manager ->
            " Otherwise install it with `" <> installCommand manager toolName <> "`."
        Nothing -> ""

packageInstallSuffix :: HostInfo -> String -> String
packageInstallSuffix host toolName =
    case hostPrimaryManager host of
        Just manager ->
            " Try `" <> installCommand manager toolName <> "`."
        Nothing -> ""

fastLinkerInstallSuffix :: HostInfo -> [ToolReport] -> String
fastLinkerInstallSuffix host _reports =
    case hostPrimaryManager host of
        Just manager ->
            let moldPackage = fromMaybe "mold" (packageNameFor manager "mold")
                lldPackage = fromMaybe "ld.lld" (packageNameFor manager "ld.lld")
             in " Try `" <> installCommand manager moldPackage <> "` or `" <> installCommand manager lldPackage <> "`."
        Nothing -> ""

explicitLinkerInstallSuffix :: HostInfo -> String -> String
explicitLinkerInstallSuffix host linkerName =
    if looksLikeCommandPath linkerName
        then ""
        else packageInstallSuffix host linkerName

installCommand :: PackageManager -> String -> String
installCommand manager toolName =
    case manager of
        Apt -> "sudo apt install " <> packageName
        Brew -> "brew install " <> packageName
        Pacman -> "sudo pacman -S " <> packageName
        Dnf -> "sudo dnf install " <> packageName
        Yum -> "sudo yum install " <> packageName
        Zypper -> "sudo zypper install " <> packageName
        Apk -> "sudo apk add " <> packageName
        Nix -> "nix profile install nixpkgs#" <> packageName
  where
    packageName = fromMaybe toolName (packageNameFor manager toolName)

packageNameFor :: PackageManager -> String -> Maybe String
packageNameFor manager toolName =
    case (manager, toolName) of
        (Apt, "cabal") -> Just "cabal-install"
        (Apt, "ld.gold") -> Just "binutils"
        (Apt, "ld.lld") -> Just "lld"
        (Apt, "ld") -> Just "binutils"
        (Pacman, "pkg-config") -> Just "pkgconf"
        (Pacman, "cabal") -> Just "cabal-install"
        (Pacman, "ld.gold") -> Just "binutils"
        (Pacman, "ld.lld") -> Just "lld"
        (Pacman, "ld") -> Just "binutils"
        (Brew, "pkg-config") -> Just "pkgconf"
        (Brew, "cabal") -> Just "cabal-install"
        (Brew, "ld.gold") -> Just "binutils"
        (Brew, "ld.lld") -> Just "llvm"
        (Dnf, "pkg-config") -> Just "pkgconf-pkg-config"
        (Dnf, "cabal") -> Just "cabal-install"
        (Dnf, "ld.gold") -> Just "binutils"
        (Dnf, "ld.lld") -> Just "lld"
        (Dnf, "ld") -> Just "binutils"
        (Yum, "pkg-config") -> Just "pkgconf-pkg-config"
        (Yum, "cabal") -> Just "cabal-install"
        (Yum, "ld.gold") -> Just "binutils"
        (Yum, "ld.lld") -> Just "lld"
        (Yum, "ld") -> Just "binutils"
        (Zypper, "cabal") -> Just "cabal-install"
        (Zypper, "ld.gold") -> Just "binutils"
        (Zypper, "ld.lld") -> Just "lld"
        (Zypper, "ld") -> Just "binutils"
        (Apk, "pkg-config") -> Just "pkgconf"
        (Apk, "cabal") -> Just "cabal-install"
        (Apk, "ld.gold") -> Just "binutils"
        (Apk, "ld.lld") -> Just "lld"
        (Apk, "ld") -> Just "binutils"
        (Nix, "pkg-config") -> Just "pkg-config"
        (Nix, "cabal") -> Just "cabal-install"
        (Nix, "ld.gold") -> Just "binutils"
        (Nix, "ld.lld") -> Just "llvmPackages.lld"
        (Nix, "ld") -> Just "binutils"
        _ -> Just toolName

runCommand :: FilePath -> [String] -> IO (Maybe String)
runCommand command args = do
    result <- try (readProcessWithExitCode command args "") :: IO (Either IOException (ExitCode, String, String))
    pure $
        case result of
            Right (ExitSuccess, stdoutText, stderrText) ->
                let output = trim (stdoutText <> stderrText)
                 in if null output then Nothing else Just output
            _ -> Nothing

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path = do
    exists <- doesFileExist path
    if exists
        then do
            contents <- try (readFile path) :: IO (Either IOException String)
            pure (either (const Nothing) Just contents)
        else pure Nothing

firstLine :: String -> String
firstLine =
    trim . takeWhile (/= '\n')

trim :: String -> String
trim =
    dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse

pad :: Int -> String -> String
pad width value =
    value <> replicate (max 0 (width - length value)) ' '

containsInsensitive :: String -> String -> Bool
containsInsensitive needle haystack =
    map toLower needle `isInfixOf` map toLower haystack

mapMaybeLookup :: Eq a => a -> [(a, b)] -> Maybe b
mapMaybeLookup key =
    lookup key

packageManagerCommand :: PackageManager -> String
packageManagerCommand manager =
    case manager of
        Apt -> "apt"
        Brew -> "brew"
        Pacman -> "pacman"
        Dnf -> "dnf"
        Yum -> "yum"
        Zypper -> "zypper"
        Apk -> "apk"
        Nix -> "nix"

renderManager :: PackageManager -> String
renderManager manager =
    case manager of
        Apt -> "apt"
        Brew -> "brew"
        Pacman -> "pacman"
        Dnf -> "dnf"
        Yum -> "yum"
        Zypper -> "zypper"
        Apk -> "apk"
        Nix -> "nix"

toUpperAscii :: String -> String
toUpperAscii =
    map
        (\char ->
            if 'a' <= char && char <= 'z'
                then toEnum (fromEnum char - 32)
                else char
        )

knownPackageManagers :: [PackageManager]
knownPackageManagers =
    [ Apt
    , Brew
    , Pacman
    , Dnf
    , Yum
    , Zypper
    , Apk
    , Nix
    ]

tools :: [ToolSpec]
tools =
    [ ToolSpec "ghc" Required ["--numeric-version"]
    , ToolSpec "cabal" Required ["--numeric-version"]
    , ToolSpec "ghcup" Supporting ["--numeric-version"]
    , ToolSpec "pkg-config" Supporting ["--version"]
    , ToolSpec "mold" Linker ["--version"]
    , ToolSpec "ld.lld" Linker ["--version"]
    , ToolSpec "ld" Linker ["--version"]
    ]
