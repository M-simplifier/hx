module Hx.Command.Ci (runCi, runTest) where

import Data.List (intercalate)
import Hx.Command.CabalSupport
    ( linkerArgs
    , preflightLinkerPlan
    , preflightWarnings
    , renderLinkerPlan
    , runCabalPreflight
    )
import Hx.Command.Doctor (inspectDiagnostics)
import Hx.Json (jsonArray, jsonNumber, jsonObject, jsonString)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode, rawSystem)

data CommandResult = CommandResult
    { resultCommand :: [String]
    , resultExitCode :: ExitCode
    }

runCi :: [String] -> IO ()
runCi args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn ciUsage
        else runCommandSet "hx.ci.v1" "hx ci" ciCommands args

runTest :: [String] -> IO ()
runTest args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn testUsage
        else runCommandSet "hx.test.v1" "hx test" testCommands args

runCommandSet :: String -> String -> ([String] -> [[String]]) -> [String] -> IO ()
runCommandSet schema label commandFactory args =
    case filter (/= "--json") args of
        [] -> do
            snapshot <- inspectDiagnostics
            case runCabalPreflight snapshot of
                Left message ->
                    die message
                Right preflight -> do
                    let selectedLinkerArgs = linkerArgs (preflightLinkerPlan preflight)
                        commands = commandFactory selectedLinkerArgs
                        linkerSummary = renderLinkerPlan snapshot (preflightLinkerPlan preflight)
                    if "--json" `elem` args
                        then do
                            mapM_ (hPutStrLn stderr . ("Warning: " <>)) (preflightWarnings preflight)
                            hPutStrLn stderr ("Linker: " <> linkerSummary)
                            results <- runCommandsForJson commands
                            putStrLn (commandResultsToJson schema linkerSummary selectedLinkerArgs results)
                            exitWith (combinedExitCode results)
                        else do
                            putStrLn label
                            mapM_ (putStrLn . ("Warning: " <>)) (preflightWarnings preflight)
                            putStrLn ("Linker: " <> linkerSummary)
                            results <- runCommands commands
                            putStrLn (renderCommandResults results)
                            exitWith (combinedExitCode results)
        unknown : _ -> die ("Unknown option: " <> unknown)

runCommandsForJson :: [[String]] -> IO [CommandResult]
runCommandsForJson =
    go []
  where
    go results [] = pure (reverse results)
    go results (command : rest) = do
        hPutStrLn stderr ("==> " <> intercalate " " command)
        hFlush stderr
        (exitCode, stdoutText, stderrText) <- runCommandCaptured command
        hPutStr stderr stdoutText
        hPutStr stderr stderrText
        let result = CommandResult command exitCode
        case exitCode of
            ExitSuccess -> go (result : results) rest
            ExitFailure _ -> pure (reverse (result : results))

runCommands :: [[String]] -> IO [CommandResult]
runCommands =
    go []
  where
    go results [] = pure (reverse results)
    go results (command : rest) = do
        putStrLn ("==> " <> intercalate " " command)
        hFlush stdout
        exitCode <- runCommand command
        let result = CommandResult command exitCode
        case exitCode of
            ExitSuccess -> go (result : results) rest
            ExitFailure _ -> pure (reverse (result : results))

runCommand :: [String] -> IO ExitCode
runCommand command =
    case command of
        commandName : commandArgs -> rawSystem commandName commandArgs
        [] -> rawSystem "true" []

runCommandCaptured :: [String] -> IO (ExitCode, String, String)
runCommandCaptured command =
    case command of
        commandName : commandArgs -> readProcessWithExitCode commandName commandArgs ""
        [] -> readProcessWithExitCode "true" [] ""

combinedExitCode :: [CommandResult] -> ExitCode
combinedExitCode results =
    case [exitCode | CommandResult _ exitCode@(ExitFailure _) <- results] of
        exitCode : _ -> exitCode
        [] -> ExitSuccess

renderCommandResults :: [CommandResult] -> String
renderCommandResults results =
    unlines
        ( "Evidence:"
            : map renderCommandResult results
        )

renderCommandResult :: CommandResult -> String
renderCommandResult result =
    "  - "
        <> intercalate " " (resultCommand result)
        <> ": "
        <> renderExitCode (resultExitCode result)

commandResultsToJson :: String -> String -> [String] -> [CommandResult] -> String
commandResultsToJson schema linkerSummary selectedLinkerArgs results =
    jsonObject
        [ ("schemaVersion", jsonString schema)
        , ("status", jsonString (if combinedExitCode results == ExitSuccess then "pass" else "fail"))
        , ("linker", jsonString linkerSummary)
        , ("linkerArgs", jsonArray (map jsonString selectedLinkerArgs))
        , ("results", jsonArray (map commandResultToJson results))
        ]

commandResultToJson :: CommandResult -> String
commandResultToJson result =
    jsonObject
        [ ("command", jsonArray (map jsonString (resultCommand result)))
        , ("exitCode", jsonNumber (exitCodeNumber (resultExitCode result)))
        , ("status", jsonString (if resultExitCode result == ExitSuccess then "pass" else "fail"))
        ]

exitCodeNumber :: ExitCode -> Int
exitCodeNumber exitCode =
    case exitCode of
        ExitSuccess -> 0
        ExitFailure code -> code

renderExitCode :: ExitCode -> String
renderExitCode exitCode =
    case exitCode of
        ExitSuccess -> "pass"
        ExitFailure code -> "fail (" <> show code <> ")"

ciCommands :: [String] -> [[String]]
ciCommands selectedLinkerArgs =
    [ ["cabal", "build"] ++ selectedLinkerArgs ++ ["all"]
    , ["cabal", "test"] ++ selectedLinkerArgs ++ ["all"]
    ]

testCommands :: [String] -> [[String]]
testCommands selectedLinkerArgs =
    [ ["cabal", "test"] ++ selectedLinkerArgs ++ ["all"]
    ]

ciUsage :: String
ciUsage =
    unlines
        [ "hx ci - run the default project verification flow"
        , ""
        , "Usage:"
        , "  hx ci"
        , "  hx ci --json"
        ]

testUsage :: String
testUsage =
    unlines
        [ "hx test - run the default Cabal test flow"
        , ""
        , "Usage:"
        , "  hx test"
        , "  hx test --json"
        ]
