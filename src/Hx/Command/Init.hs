module Hx.Command.Init (runInit) where

import Data.Char (isAlphaNum, toLower)
import Hx.Json (jsonArray, jsonBool, jsonObject, jsonString)
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.Exit (die)
import System.FilePath ((</>))

data ProjectKind
    = Cli
    | Library
    | Service
    deriving (Eq, Show)

data InitInvocation = InitInvocation
    { initName :: String
    , initKind :: ProjectKind
    , initPlanOnly :: Bool
    , initJson :: Bool
    }

data PlannedFile = PlannedFile
    { plannedPath :: FilePath
    , plannedContents :: String
    }

runInit :: [String] -> IO ()
runInit args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn initUsage
        else
            case parseInitInvocation args of
                Left message -> die message
                Right invocation -> do
                    let files = plannedFiles invocation
                        root = initName invocation
                    exists <- doesPathExist root
                    if exists && not (initPlanOnly invocation)
                        then die ("Refusing to overwrite existing path: " <> root)
                        else do
                            if initJson invocation
                                then putStrLn (initPlanToJson invocation files)
                                else putStrLn (renderInitPlan invocation files)
                            if initPlanOnly invocation
                                then pure ()
                                else do
                                    mapM_ (writePlannedFile root) files
                                    putStrLn ("Created " <> root)
                                    putStrLn "Next steps:"
                                    putStrLn ("  cd " <> root)
                                    putStrLn "  hx status"
                                    putStrLn "  hx ci"

parseInitInvocation :: [String] -> Either String InitInvocation
parseInitInvocation rawArgs =
    case positionalArgs of
        [name] ->
            if null (sanitizePackageName name)
                then Left ("Invalid project name: " <> name)
                else
                    Right
                        InitInvocation
                            { initName = sanitizePackageName name
                            , initKind = parsedKind
                            , initPlanOnly = "--plan" `elem` rawArgs
                            , initJson = "--json" `elem` rawArgs
                            }
        [] -> Left ("Missing project name.\n\n" <> initUsage)
        _ -> Left ("Too many project names.\n\n" <> initUsage)
  where
    positionalArgs = filter (not . isHxFlag) rawArgs
    parsedKind =
        case [drop (length "--kind=") arg | arg <- rawArgs, "--kind=" `prefixOf` arg] of
            [] -> Cli
            kindName : _ -> parseKindOrCli kindName

parseKindOrCli :: String -> ProjectKind
parseKindOrCli rawKind =
    case rawKind of
        "cli" -> Cli
        "library" -> Library
        "lib" -> Library
        "service" -> Service
        _ -> Cli

isHxFlag :: String -> Bool
isHxFlag arg =
    arg `elem` ["--plan", "--apply", "--json"]
        || "--kind=" `prefixOf` arg

plannedFiles :: InitInvocation -> [PlannedFile]
plannedFiles invocation =
    [ PlannedFile ".gitignore" gitignoreContents
    , PlannedFile "cabal.project" "packages: .\nwrite-ghc-environment-files: never\n"
    , PlannedFile (projectName <> ".cabal") (cabalFileContents invocation)
    , PlannedFile "README.md" (readmeContents invocation)
    , PlannedFile "test/Main.hs" testMainContents
    , PlannedFile ".github/workflows/ci.yml" ciWorkflowContents
    ]
        ++ sourceFiles invocation
  where
    projectName = initName invocation

sourceFiles :: InitInvocation -> [PlannedFile]
sourceFiles invocation =
    case initKind invocation of
        Library ->
            [PlannedFile "src/Lib.hs" libraryContents]
        Cli ->
            [PlannedFile "app/Main.hs" cliMainContents]
        Service ->
            [ PlannedFile "app/Main.hs" serviceMainContents
            , PlannedFile "src/Lib.hs" libraryContents
            ]

cabalFileContents :: InitInvocation -> String
cabalFileContents invocation =
    unlines
        ( [ "cabal-version:      3.8"
          , "name:               " <> projectName
          , "version:            0.1.0.0"
          , "synopsis:           " <> projectName
          , "license:            MIT"
          , "build-type:         Simple"
          , ""
          , "common shared-options"
          , "    ghc-options: -Wall"
          , "    default-language: GHC2021"
          , ""
          ]
            ++ packageStanzas
        )
  where
    projectName = initName invocation
    packageStanzas =
        case initKind invocation of
            Library ->
                libraryStanza
                    ++ testStanza projectName ["      , " <> projectName]
            Cli ->
                executableStanza projectName []
                    ++ testStanza projectName []
            Service ->
                libraryStanza
                    ++ executableStanza projectName ["      , " <> projectName]
                    ++ testStanza projectName ["      , " <> projectName]

libraryStanza :: [String]
libraryStanza =
    [ "library"
    , "    import: shared-options"
    , "    hs-source-dirs: src"
    , "    exposed-modules: Lib"
    , "    build-depends:"
    , "        base >=4.18 && <4.22"
    , ""
    ]

executableStanza :: String -> [String] -> [String]
executableStanza projectName extraDepends =
    [ "executable " <> projectName
    , "    import: shared-options"
    , "    main-is: Main.hs"
    , "    hs-source-dirs: app"
    , "    build-depends:"
    , "        base >=4.18 && <4.22"
    ]
        ++ extraDepends
        ++ [""]

testStanza :: String -> [String] -> [String]
testStanza projectName extraDepends =
    [ "test-suite " <> projectName <> "-test"
    , "    import: shared-options"
    , "    type: exitcode-stdio-1.0"
    , "    main-is: Main.hs"
    , "    hs-source-dirs: test"
    , "    build-depends:"
    , "        base >=4.18 && <4.22"
    ]
        ++ extraDepends
        ++ [""]

readmeContents :: InitInvocation -> String
readmeContents invocation =
    unlines
        [ "# " <> initName invocation
        , ""
        , "Created with `hx init --kind=" <> renderKind (initKind invocation) <> "`."
        , ""
        , "## Commands"
        , ""
        , "```bash"
        , "hx status"
        , "hx add text --plan"
        , "hx ci"
        , "```"
        ]

gitignoreContents :: String
gitignoreContents =
    unlines
        [ "dist-newstyle/"
        , "tmp/"
        , "*.hi"
        , "*.o"
        , ".ghc.environment.*"
        , "cabal.project.local"
        ]

cliMainContents :: String
cliMainContents =
    unlines
        [ "module Main where"
        , ""
        , "main :: IO ()"
        , "main = putStrLn \"hello from hx\""
        ]

serviceMainContents :: String
serviceMainContents =
    unlines
        [ "module Main where"
        , ""
        , "import Lib (message)"
        , ""
        , "main :: IO ()"
        , "main = putStrLn message"
        ]

libraryContents :: String
libraryContents =
    unlines
        [ "module Lib (message) where"
        , ""
        , "message :: String"
        , "message = \"hello from hx\""
        ]

testMainContents :: String
testMainContents =
    unlines
        [ "module Main where"
        , ""
        , "main :: IO ()"
        , "main = putStrLn \"ok\""
        ]

ciWorkflowContents :: String
ciWorkflowContents =
    unlines
        [ "name: ci"
        , ""
        , "on:"
        , "  push:"
        , "    branches:"
        , "      - main"
        , "  pull_request:"
        , ""
        , "permissions:"
        , "  contents: read"
        , ""
        , "jobs:"
        , "  ci:"
        , "    runs-on: ubuntu-latest"
        , "    steps:"
        , "      - uses: actions/checkout@v6"
        , "      - uses: haskell-actions/setup@v2"
        , "        with:"
        , "          ghc-version: \"9.6.7\""
        , "          cabal-version: \"3.12.1.0\""
        , "      - run: cabal build all"
        , "      - run: cabal test all"
        ]

writePlannedFile :: FilePath -> PlannedFile -> IO ()
writePlannedFile root file = do
    createDirectoryIfMissing True (parentDirectory (root </> plannedPath file))
    writeFile (root </> plannedPath file) (plannedContents file)

parentDirectory :: FilePath -> FilePath
parentDirectory path =
    reverse (dropWhile (/= '/') (reverse path))

renderInitPlan :: InitInvocation -> [PlannedFile] -> String
renderInitPlan invocation files =
    unlines
        [ "hx init"
        , "Project: " <> initName invocation
        , "Kind: " <> renderKind (initKind invocation)
        , "Mode: " <> if initPlanOnly invocation then "plan only" else "apply"
        , "Files:"
        , unlines (map (("  - " <>) . plannedPath) files)
        ]

initPlanToJson :: InitInvocation -> [PlannedFile] -> String
initPlanToJson invocation files =
    jsonObject
        [ ("schemaVersion", jsonString "hx.init-plan.v1")
        , ("project", jsonString (initName invocation))
        , ("kind", jsonString (renderKind (initKind invocation)))
        , ("planOnly", jsonBool (initPlanOnly invocation))
        , ("files", jsonArray (map (jsonString . plannedPath) files))
        ]

renderKind :: ProjectKind -> String
renderKind kind =
    case kind of
        Cli -> "cli"
        Library -> "library"
        Service -> "service"

sanitizePackageName :: String -> String
sanitizePackageName =
    collapseDashes . map sanitizeChar
  where
    sanitizeChar char
        | isAlphaNum char = toLower char
        | char `elem` ['-', '_'] = '-'
        | otherwise = '-'

    collapseDashes value =
        case value of
            '-' : rest -> collapseDashes rest
            _ -> reverse (dropWhile (== '-') (reverse (go False value)))

    go _ [] = []
    go previousDash (char : rest)
        | char == '-' && previousDash = go True rest
        | char == '-' = '-' : go True rest
        | otherwise = char : go False rest

initUsage :: String
initUsage =
    unlines
        [ "hx init - create a Cabal-first Haskell project"
        , ""
        , "Usage:"
        , "  hx init <name> [--kind=cli|library|service] [--plan] [--json]"
        , ""
        , "Examples:"
        , "  hx init my-app --kind=cli"
        , "  hx init my-lib --kind=library --plan"
        ]

prefixOf :: String -> String -> Bool
prefixOf prefix value =
    take (length prefix) value == prefix
