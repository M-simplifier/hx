module Main where

import Control.Exception (SomeException, bracket, evaluate, finally, try)
import Control.Monad (forM, unless, when)
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Version (showVersion)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hx.CLI (run)
import Hx.Command.Build (runBuild)
import Hx.Command.Run (runRun)
import Paths_hx (version)
import System.Directory
    ( Permissions (..)
    , createDirectory
    , createDirectoryIfMissing
    , createFileLink
    , doesFileExist
    , findExecutable
    , getPermissions
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    , setPermissions
    , withCurrentDirectory
    )
import System.Environment (lookupEnv, setEnv, unsetEnv, withArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (Handle, hClose, hFlush, openTempFile, stderr, stdout)
import System.FilePath ((</>), takeDirectory)

data Captured = Captured
    { capturedExit :: ExitCode
    , capturedStdout :: String
    , capturedStderr :: String
    }

data TestCase = TestCase
    { testName :: String
    , testAction :: IO ()
    }

main :: IO ()
main = do
    results <-
        forM tests $ \testCase -> do
            outcome <- try (testAction testCase) :: IO (Either SomeException ())
            case outcome of
                Right () -> do
                    putStrLn ("ok - " <> testName testCase)
                    pure True
                Left err -> do
                    putStrLn ("not ok - " <> testName testCase)
                    putStrLn ("  " <> show err)
                    pure False
    unless (and results) exitFailure

tests :: [TestCase]
tests =
    [ TestCase "top-level help with no args" testTopLevelHelp
    , TestCase "version command" testVersion
    , TestCase "unknown command fails" testUnknownCommand
    , TestCase "build help" testBuildHelp
    , TestCase "run help" testRunHelp
    , TestCase "build dry-run release profile" testBuildDryRunRelease
    , TestCase "run dry-run server profile" testRunDryRunServer
    , TestCase "run auto-selects sole executable over test suite" testRunAutoSelectsSoleExecutable
    , TestCase "run dry-run server profile with extra RTS args" testRunDryRunServerWithAdditionalRts
    , TestCase "run rejects mixed RTS input styles" testRunRejectsMixedRtsInputs
    , TestCase "init plan JSON" testInitPlanJson
    , TestCase "status JSON reports project components" testStatusJson
    , TestCase "doctor JSON ignores generated target export" testDoctorJsonIgnoresTarget
    , TestCase "ci JSON uses detected fast linker" testCiJsonUsesDetectedFastLinker
    , TestCase "linker use writes and clears local config" testLinkerUseWritesAndClearsLocalConfig
    , TestCase "add plan reports dependency side effect" testAddPlan
    , TestCase "add apply updates cabal file" testAddApply
    , TestCase "build blocks when pkg-config tool is missing" testBuildBlocksWhenPkgConfigToolMissing
    , TestCase "build blocks when pkg-config library is unresolved" testBuildBlocksWhenPkgConfigLibraryIsUnresolved
    , TestCase "build blocks when explicit linker wrapper is missing" testBuildBlocksWhenExplicitLinkerIsMissing
    ]

testTopLevelHelp :: IO ()
testTopLevelHelp = do
    captured <- captureAction (withArgs [] run)
    assertExit "top-level help exit" ExitSuccess (capturedExit captured)
    assertContains "top-level help stdout" "hx - Haskell toolchain helper" (capturedStdout captured)
    assertEqual "top-level help stderr" "" (capturedStderr captured)

testVersion :: IO ()
testVersion = do
    captured <- captureAction (withArgs ["version"] run)
    assertExit "version exit" ExitSuccess (capturedExit captured)
    assertEqual "version stdout" ("hx " <> showVersion version <> "\n") (capturedStdout captured)
    assertEqual "version stderr" "" (capturedStderr captured)

testUnknownCommand :: IO ()
testUnknownCommand = do
    captured <- captureAction (withArgs ["wat"] run)
    assertExit "unknown command exit" (ExitFailure 1) (capturedExit captured)
    assertContains "unknown command stderr" "Unknown command: wat" (capturedStderr captured)
    assertContains "unknown command stderr usage" "hx - Haskell toolchain helper" (capturedStderr captured)

testBuildHelp :: IO ()
testBuildHelp = do
    captured <- captureAction (withArgs ["build", "--help"] run)
    assertExit "build help exit" ExitSuccess (capturedExit captured)
    assertContains "build help stdout" "hx build - profile-aware cabal build wrapper" (capturedStdout captured)
    assertContains "build help profile text" "server   Release profile plus embedded `-N` runtime defaults" (capturedStdout captured)

testRunHelp :: IO ()
testRunHelp = do
    captured <- captureAction (withArgs ["run", "--help"] run)
    assertExit "run help exit" ExitSuccess (capturedExit captured)
    assertContains "run help stdout" "hx run - profile-aware cabal run wrapper" (capturedStdout captured)
    assertContains "run help runtime flag" "--rts=<arg>" (capturedStdout captured)

testBuildDryRunRelease :: IO ()
testBuildDryRunRelease =
    withSampleProject $ \projectDir -> do
        captured <- withCurrentDirectory projectDir (captureAction (runBuild ["release", "--dry-run"]))
        assertExit "build dry-run exit" ExitSuccess (capturedExit captured)
        assertContains "build dry-run profile" "Profile: release" (capturedStdout captured)
        assertContains
            "build dry-run command"
            "Command: cabal build --enable-optimization=2 --enable-split-sections --disable-profiling"
            (capturedStdout captured)

testRunDryRunServer :: IO ()
testRunDryRunServer =
    withSampleProject $ \projectDir -> do
        captured <- withCurrentDirectory projectDir (captureAction (runRun ["--profile=server", "run-sample", "--dry-run"]))
        assertExit "run dry-run exit" ExitSuccess (capturedExit captured)
        assertContains "run dry-run runtime summary" "Runtime: profile defaults via `-with-rtsopts`: -N" (capturedStdout captured)
        assertContains "run dry-run command" "--ghc-option=-with-rtsopts=-N" (capturedStdout captured)

testRunAutoSelectsSoleExecutable :: IO ()
testRunAutoSelectsSoleExecutable =
    withProjectFiles sampleProjectWithTestFiles $ \projectDir -> do
        captured <- withCurrentDirectory projectDir (captureAction (runRun ["--dry-run"]))
        assertExit "run auto-select exit" ExitSuccess (capturedExit captured)
        assertContains "run auto-select target" "Target: auto-selecting `run-sample`" (capturedStdout captured)
        assertContains "run auto-select command" " run-sample --dry-run" (capturedStdout captured)

testRunDryRunServerWithAdditionalRts :: IO ()
testRunDryRunServerWithAdditionalRts =
    withSampleProject $ \projectDir -> do
        captured <-
            withCurrentDirectory
                projectDir
                (captureAction (runRun ["--profile=server", "run-sample", "--rts=-A64m", "--dry-run"]))
        assertExit "run dry-run with RTS exit" ExitSuccess (capturedExit captured)
        assertContains
            "run dry-run with RTS runtime summary"
            "Runtime: profile defaults via `-with-rtsopts`: -N; additional RTS args via `+RTS ... -RTS`: -A64m"
            (capturedStdout captured)
        assertSubstringCount "run dry-run with RTS rtsopts count" 1 "--ghc-option=-rtsopts" (capturedStdout captured)
        assertSubstringCount "run dry-run with RTS threaded count" 1 "--ghc-option=-threaded" (capturedStdout captured)

testRunRejectsMixedRtsInputs :: IO ()
testRunRejectsMixedRtsInputs =
    withSampleProject $ \projectDir -> do
        captured <-
            withCurrentDirectory
                projectDir
                (captureAction (runRun ["run-sample", "--rts=-N", "--", "+RTS", "-s", "-RTS"]))
        assertExit "run mixed RTS exit" (ExitFailure 1) (capturedExit captured)
        assertContains "run mixed RTS stderr" "Conflicting runtime arguments." (capturedStderr captured)

testInitPlanJson :: IO ()
testInitPlanJson = do
    captured <- captureAction (withArgs ["init", "demo-app", "--kind=cli", "--plan", "--json"] run)
    assertExit "init plan JSON exit" ExitSuccess (capturedExit captured)
    assertContains "init plan schema" "\"schemaVersion\":\"hx.init-plan.v1\"" (capturedStdout captured)
    assertContains "init plan project" "\"project\":\"demo-app\"" (capturedStdout captured)
    assertContains "init plan cabal file" "demo-app.cabal" (capturedStdout captured)

testStatusJson :: IO ()
testStatusJson =
    withSampleProject $ \projectDir -> do
        captured <- withCurrentDirectory projectDir (captureAction (withArgs ["status", "--json"] run))
        assertExit "status JSON exit" ExitSuccess (capturedExit captured)
        assertContains "status JSON schema" "\"schemaVersion\":\"hx.project.v1\"" (capturedStdout captured)
        assertContains "status JSON executable target" "\"target\":\"exe:run-sample\"" (capturedStdout captured)

testDoctorJsonIgnoresTarget :: IO ()
testDoctorJsonIgnoresTarget =
    withSampleProject $ \projectDir -> do
        writeProjectFile projectDir ("target/public-export/hx/copied.cabal", samplePackageContents [])
        captured <- withCurrentDirectory projectDir (captureAction (withArgs ["doctor", "--json"] run))
        assertExit "doctor JSON exit" ExitSuccess (capturedExit captured)
        assertContains "doctor JSON schema" "\"schemaVersion\":\"hx.diagnostics.v1\"" (capturedStdout captured)
        assertContains "doctor JSON root cabal file" "\"packageFiles\":[\"run-sample.cabal\"]" (capturedStdout captured)
        assertNotContains "doctor JSON target export cabal file" "target/public-export/hx/copied.cabal" (capturedStdout captured)

testCiJsonUsesDetectedFastLinker :: IO ()
testCiJsonUsesDetectedFastLinker =
    withSampleProject $ \projectDir ->
        withMockToolPath ["ghc"] [("cabal", fakeSuccessfulCabalScript), ("mold", fakeMoldScript)] $
            withCurrentDirectory projectDir $ do
                captured <- captureAction (withArgs ["ci", "--json"] run)
                assertExit "ci JSON fast linker exit" ExitSuccess (capturedExit captured)
                assertContains "ci JSON linker args" "\"linkerArgs\":[\"--ghc-option=-optl-fuse-ld=mold\"]" (capturedStdout captured)
                assertContains
                    "ci JSON build command"
                    "\"command\":[\"cabal\",\"build\",\"--ghc-option=-optl-fuse-ld=mold\",\"all\"]"
                    (capturedStdout captured)
                assertContains
                    "ci JSON mold warning"
                    "Warning: GHC may print `Warning: Couldn't figure out linker information!` when using `mold`"
                    (capturedStderr captured)
                assertContains "ci JSON stderr linker summary" "Linker: auto-selecting `mold` for this invocation" (capturedStderr captured)

testLinkerUseWritesAndClearsLocalConfig :: IO ()
testLinkerUseWritesAndClearsLocalConfig =
    withSampleProject $ \projectDir ->
        withMockToolPath ["ghc", "cabal"] [("mold", fakeMoldScript)] $
            withCurrentDirectory projectDir $ do
                initialStatusCaptured <- captureAction (withArgs ["linker", "status", "--json"] run)
                assertExit "linker initial status exit" ExitSuccess (capturedExit initialStatusCaptured)
                assertContains
                    "linker initial status warning"
                    "\"warnings\":[\"GHC may print"
                    (capturedStdout initialStatusCaptured)

                useCaptured <- captureAction (withArgs ["linker", "use", "mold", "--apply"] run)
                assertExit "linker use exit" ExitSuccess (capturedExit useCaptured)
                assertContains "linker use output" "Applied linker setting to cabal.project.local." (capturedStdout useCaptured)
                contents <- readStrictFile (projectDir </> "cabal.project.local")
                assertContains "linker use marker" "-- hx linker: begin" contents
                assertContains "linker use flag" "ghc-options: -optl-fuse-ld=mold" contents

                statusCaptured <- captureAction (withArgs ["linker", "status", "--json"] run)
                assertExit "linker status exit" ExitSuccess (capturedExit statusCaptured)
                assertContains "linker status managed" "\"localConfigManaged\":true" (capturedStdout statusCaptured)

                clearCaptured <- captureAction (withArgs ["linker", "clear", "--apply"] run)
                assertExit "linker clear exit" ExitSuccess (capturedExit clearCaptured)
                exists <- doesFileExist (projectDir </> "cabal.project.local")
                assertEqual "linker clear removes local-only file" "False" (show exists)

testAddPlan :: IO ()
testAddPlan =
    withSampleProject $ \projectDir -> do
        captured <- withCurrentDirectory projectDir (captureAction (withArgs ["add", "text", "--plan"] run))
        assertExit "add plan exit" ExitSuccess (capturedExit captured)
        assertContains "add plan target" "Target: exe:run-sample" (capturedStdout captured)
        assertContains "add plan dependency" "Will add: text" (capturedStdout captured)
        assertContains "add plan side effect" "Side effects: edit run-sample.cabal" (capturedStdout captured)

testAddApply :: IO ()
testAddApply =
    withSampleProject $ \projectDir -> do
        captured <- withCurrentDirectory projectDir (captureAction (withArgs ["add", "text", "--apply", "--no-verify"] run))
        assertExit "add apply exit" ExitSuccess (capturedExit captured)
        assertContains "add apply output" "Applied dependency update." (capturedStdout captured)
        contents <- readStrictFile (projectDir </> "run-sample.cabal")
        assertContains "add apply cabal dependency" ", text" contents

testBuildBlocksWhenPkgConfigToolMissing :: IO ()
testBuildBlocksWhenPkgConfigToolMissing =
    withPkgConfigProject $ \projectDir ->
        withMockToolPath ["ghc", "cabal"] [] $
            withCurrentDirectory projectDir $ do
                captured <- captureAction (runBuild ["--dry-run"])
                assertExit "missing pkg-config exit" (ExitFailure 1) (capturedExit captured)
                assertContains
                    "missing pkg-config stderr"
                    "The current project declares pkg-config dependencies, but `pkg-config` is not on PATH."
                    (capturedStderr captured)
                assertContains "missing pkg-config declarations" "Project declarations:" (capturedStderr captured)
                assertContains "missing pkg-config library name" "`openssl`" (capturedStderr captured)

testBuildBlocksWhenPkgConfigLibraryIsUnresolved :: IO ()
testBuildBlocksWhenPkgConfigLibraryIsUnresolved =
    withPkgConfigProject $ \projectDir ->
        withMockToolPath ["ghc", "cabal"] [("pkg-config", fakeFailingPkgConfigScript)] $
            withCurrentDirectory projectDir $ do
                captured <- captureAction (runBuild ["--dry-run"])
                assertExit "unresolved pkg-config exit" (ExitFailure 1) (capturedExit captured)
                assertContains
                    "unresolved pkg-config stderr"
                    "The current project has unresolved pkg-config libraries."
                    (capturedStderr captured)
                assertContains
                    "unresolved pkg-config library"
                    "pkg-config cannot resolve `openssl` in the current project."
                    (capturedStderr captured)

testBuildBlocksWhenExplicitLinkerIsMissing :: IO ()
testBuildBlocksWhenExplicitLinkerIsMissing =
    withMissingLinkerProject $ \projectDir ->
        withMockToolPath ["ghc", "cabal"] [] $
            withCurrentDirectory projectDir $ do
                captured <- captureAction (runBuild ["--dry-run"])
                assertExit "missing linker exit" (ExitFailure 1) (capturedExit captured)
                assertContains
                    "missing linker stderr"
                    "The current project already selects `./tooling/missing-linker`, but that command is not available on this host."
                    (capturedStderr captured)
                assertContains "missing linker hints" "Project linker wiring:" (capturedStderr captured)

captureAction :: IO () -> IO Captured
captureAction action = do
    tempDir <- getTemporaryDirectory
    bracket (captureHandles tempDir) cleanupHandles $ \handles -> do
        result <- try (redirectToCapturedHandles handles action) :: IO (Either ExitCode ())
        stdoutContents <- readStrictFile (capturedStdoutPath handles)
        stderrContents <- readStrictFile (capturedStderrPath handles)
        pure
            Captured
                { capturedExit = either id (const ExitSuccess) result
                , capturedStdout = stdoutContents
                , capturedStderr = stderrContents
                }

data CaptureHandles = CaptureHandles
    { savedStdoutHandle :: Handle
    , savedStderrHandle :: Handle
    , tempStdoutHandle :: Handle
    , tempStderrHandle :: Handle
    , capturedStdoutPath :: FilePath
    , capturedStderrPath :: FilePath
    }

captureHandles :: FilePath -> IO CaptureHandles
captureHandles tempDir = do
    savedOut <- hDuplicate stdout
    savedErr <- hDuplicate stderr
    (stdoutPath, stdoutHandle) <- openTempFile tempDir "hx-test-stdout"
    (stderrPath, stderrHandle) <- openTempFile tempDir "hx-test-stderr"
    pure
        CaptureHandles
            { savedStdoutHandle = savedOut
            , savedStderrHandle = savedErr
            , tempStdoutHandle = stdoutHandle
            , tempStderrHandle = stderrHandle
            , capturedStdoutPath = stdoutPath
            , capturedStderrPath = stderrPath
            }

cleanupHandles :: CaptureHandles -> IO ()
cleanupHandles handles = do
    mapM_ safeClose [tempStdoutHandle handles, tempStderrHandle handles, savedStdoutHandle handles, savedStderrHandle handles]
    mapM_ safeRemoveFile [capturedStdoutPath handles, capturedStderrPath handles]

redirectToCapturedHandles :: CaptureHandles -> IO () -> IO ()
redirectToCapturedHandles handles action = do
    hDuplicateTo (tempStdoutHandle handles) stdout
    hDuplicateTo (tempStderrHandle handles) stderr
    action
        `finally` do
            hFlush stdout
            hFlush stderr
            hDuplicateTo (savedStdoutHandle handles) stdout
            hDuplicateTo (savedStderrHandle handles) stderr
            safeClose (tempStdoutHandle handles)
            safeClose (tempStderrHandle handles)

readStrictFile :: FilePath -> IO String
readStrictFile path = do
    contents <- readFile path
    _ <- evaluate (length contents)
    pure contents

withSampleProject :: (FilePath -> IO a) -> IO a
withSampleProject =
    withProjectFiles sampleProjectFiles

withPkgConfigProject :: (FilePath -> IO a) -> IO a
withPkgConfigProject =
    withProjectFiles pkgConfigProjectFiles

withMissingLinkerProject :: (FilePath -> IO a) -> IO a
withMissingLinkerProject =
    withProjectFiles missingLinkerProjectFiles

withProjectFiles :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withProjectFiles files action =
    withTempDirectory "hx-sample-project" $ \projectDir -> do
        mapM_ (writeProjectFile projectDir) files
        action projectDir

writeProjectFile :: FilePath -> (FilePath, String) -> IO ()
writeProjectFile projectDir (relativePath, contents) = do
    let destination = projectDir </> relativePath
        parent = takeDirectory destination
    createDirectoryIfMissing True parent
    writeFile destination contents

sampleProjectFiles :: [(FilePath, String)]
sampleProjectFiles =
    [ ("cabal.project", "packages: .\n")
    , ("run-sample.cabal", samplePackageContents [])
    , ("app/Main.hs", sampleMainContents)
    ]

sampleProjectWithTestFiles :: [(FilePath, String)]
sampleProjectWithTestFiles =
    [ ("cabal.project", "packages: .\n")
    , ("run-sample.cabal", samplePackageWithTestContents)
    , ("app/Main.hs", sampleMainContents)
    , ("test/Main.hs", "main :: IO ()\nmain = putStrLn \"ok\"\n")
    ]

pkgConfigProjectFiles :: [(FilePath, String)]
pkgConfigProjectFiles =
    [ ("cabal.project", "packages: .\n")
    , ("run-sample.cabal", samplePackageContents ["  pkgconfig-depends: openssl"])
    , ("app/Main.hs", sampleMainContents)
    ]

missingLinkerProjectFiles :: [(FilePath, String)]
missingLinkerProjectFiles =
    [ ("cabal.project", "packages: .\n")
    , ("run-sample.cabal", samplePackageContents ["  ghc-options: -pgml=./tooling/missing-linker"])
    , ("app/Main.hs", sampleMainContents)
    ]

samplePackageContents :: [String] -> String
samplePackageContents extraLines =
    unlines
        ( [ "cabal-version: 3.8"
          , "name: run-sample"
          , "version: 0.1.0.0"
          , "build-type: Simple"
          , ""
          , "executable run-sample"
          , "  main-is: Main.hs"
          , "  hs-source-dirs: app"
          , "  build-depends: base >=4.18 && <4.22"
          ]
            ++ extraLines
            ++ ["  default-language: GHC2021"]
        )

sampleMainContents :: String
sampleMainContents =
    unlines
        [ "module Main where"
        , ""
        , "main :: IO ()"
        , "main = putStrLn \"run-sample\""
        ]

samplePackageWithTestContents :: String
samplePackageWithTestContents =
    unlines
        [ "cabal-version: 3.8"
        , "name: run-sample"
        , "version: 0.1.0.0"
        , "build-type: Simple"
        , ""
        , "executable run-sample"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: app"
        , "  build-depends: base >=4.18 && <4.22"
        , "  default-language: GHC2021"
        , ""
        , "test-suite run-sample-test"
        , "  type: exitcode-stdio-1.0"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: test"
        , "  build-depends: base >=4.18 && <4.22"
        , "  default-language: GHC2021"
        ]

withMockToolPath :: [String] -> [(String, String)] -> IO a -> IO a
withMockToolPath linkedTools scriptedTools action =
    withTempDirectory "hx-tool-path" $ \binDir -> do
        mapM_ (linkRequiredTool binDir) linkedTools
        mapM_ (writeMockTool binDir) scriptedTools
        withEnvironmentValue "PATH" (Just binDir) action

linkRequiredTool :: FilePath -> String -> IO ()
linkRequiredTool binDir toolName = do
    toolPath <- findRequiredExecutable toolName
    createFileLink toolPath (binDir </> toolName)

findRequiredExecutable :: String -> IO FilePath
findRequiredExecutable toolName = do
    maybePath <- findExecutable toolName
    case maybePath of
        Just toolPath ->
            pure toolPath
        Nothing ->
            fail ("Required test tool is missing on PATH: " <> toolName)

writeMockTool :: FilePath -> (String, String) -> IO ()
writeMockTool binDir (toolName, scriptContents) =
    writeExecutableFile (binDir </> toolName) scriptContents

writeExecutableFile :: FilePath -> String -> IO ()
writeExecutableFile path contents = do
    writeFile path contents
    permissions <- getPermissions path
    setPermissions path permissions{executable = True}

withEnvironmentValue :: String -> Maybe String -> IO a -> IO a
withEnvironmentValue name newValue action = do
    originalValue <- lookupEnv name
    let restore =
            case originalValue of
                Just value -> setEnv name value
                Nothing -> unsetEnv name
    bracket
        (setCurrentValue newValue)
        (\_ -> restore)
        (\_ -> action)
  where
    setCurrentValue value =
        case value of
            Just nextValue -> setEnv name nextValue
            Nothing -> unsetEnv name

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
    tempDir <- getTemporaryDirectory
    (markerPath, markerHandle) <- openTempFile tempDir prefix
    hClose markerHandle
    safeRemoveFile markerPath
    let directoryPath = markerPath <> "-dir"
    createDirectory directoryPath
    action directoryPath `finally` removePathForcibly directoryPath

fakeFailingPkgConfigScript :: String
fakeFailingPkgConfigScript =
    unlines
        [ "#!/bin/sh"
        , "if [ \"$1\" = \"--modversion\" ] && [ \"$2\" = \"openssl\" ]; then"
        , "  exit 1"
        , "fi"
        , "exit 1"
        ]

fakeSuccessfulCabalScript :: String
fakeSuccessfulCabalScript =
    unlines
        [ "#!/bin/sh"
        , "if [ \"$1\" = \"--numeric-version\" ]; then"
        , "  echo 3.12.1.0"
        , "  exit 0"
        , "fi"
        , "exit 0"
        ]

fakeMoldScript :: String
fakeMoldScript =
    unlines
        [ "#!/bin/sh"
        , "echo mold 2.0.0"
        ]

safeClose :: Handle -> IO ()
safeClose handle = do
    _ <- try (hClose handle) :: IO (Either SomeException ())
    pure ()

safeRemoveFile :: FilePath -> IO ()
safeRemoveFile path = do
    _ <- try (removeFile path) :: IO (Either SomeException ())
    pure ()

assertExit :: String -> ExitCode -> ExitCode -> IO ()
assertExit label expected actual =
    unless (expected == actual) $
        fail (label <> ": expected " <> show expected <> ", got " <> show actual)

assertEqual :: String -> String -> String -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        fail
            ( label
                <> ": expected "
                <> show expected
                <> ", got "
                <> show actual
            )

assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack =
    unless (needle `isInfixOf` haystack) $
        fail
            ( label
                <> ": expected to find "
                <> show needle
                <> " in "
                <> show haystack
            )

assertNotContains :: String -> String -> String -> IO ()
assertNotContains label needle haystack =
    when (needle `isInfixOf` haystack) $
        fail
            ( label
                <> ": expected not to find "
                <> show needle
                <> " in "
                <> show haystack
            )

assertSubstringCount :: String -> Int -> String -> String -> IO ()
assertSubstringCount label expectedCount needle haystack =
    let actualCount = substringCount needle haystack
     in unless (expectedCount == actualCount) $
            fail
                ( label
                    <> ": expected "
                    <> show expectedCount
                    <> " occurrences of "
                    <> show needle
                    <> ", got "
                    <> show actualCount
                    <> " in "
                    <> show haystack
                )

substringCount :: String -> String -> Int
substringCount needle haystack
    | null needle = 0
    | otherwise =
        length [suffix | suffix <- tails haystack, needle `isPrefixOf` suffix]
