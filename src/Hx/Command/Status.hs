module Hx.Command.Status (runStatus) where

import Hx.Project (discoverProject, projectSnapshotToJson, renderProjectSnapshot)
import System.Exit (die)

runStatus :: [String] -> IO ()
runStatus args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn statusUsage
        else
            case filter (/= "--json") args of
                [] -> do
                    snapshot <- discoverProject
                    if "--json" `elem` args
                        then putStrLn (projectSnapshotToJson snapshot)
                        else putStr (renderProjectSnapshot snapshot)
                unknown : _ -> die ("Unknown status option: " <> unknown <> "\n\n" <> statusUsage)

statusUsage :: String
statusUsage =
    unlines
        [ "hx status - inspect the current Cabal project"
        , ""
        , "Usage:"
        , "  hx status"
        , "  hx status --json"
        ]
