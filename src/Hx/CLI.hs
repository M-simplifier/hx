module Hx.CLI (run) where

import Data.Version (showVersion)
import Hx.Command.Add (runAdd)
import Hx.Command.Build (runBuild)
import Hx.Command.Ci (runCi, runTest)
import Hx.Command.Doctor (runDoctor, runDoctorJson)
import Hx.Command.Init (runInit)
import Hx.Command.Linker (runLinker)
import Hx.Command.Run (runRun)
import Hx.Command.Status (runStatus)
import Paths_hx (version)
import System.Environment (getArgs)
import System.Exit (die)

run :: IO ()
run = do
    args <- getArgs
    case args of
        [] -> putStrLn usage
        ["help"] -> putStrLn usage
        "init" : initArgs -> runInit initArgs
        "add" : addArgs -> runAdd addArgs
        "status" : statusArgs -> runStatus statusArgs
        "ci" : ciArgs -> runCi ciArgs
        "test" : testArgs -> runTest testArgs
        "linker" : linkerArgs -> runLinker linkerArgs
        "build" : buildArgs -> runBuild buildArgs
        "run" : runArgs -> runRun runArgs
        ["doctor"] -> runDoctor
        ["doctor", "--json"] -> runDoctorJson
        ["version"] -> putStrLn ("hx " <> showVersion version)
        command : _ ->
            die ("Unknown command: " <> command <> "\n\n" <> usage)

usage :: String
usage =
    unlines
        [ "hx - Haskell toolchain helper"
        , ""
        , "Usage:"
        , "  hx init       Create a Cabal-first Haskell project"
        , "  hx add        Plan or apply Cabal dependency updates"
        , "  hx status     Inspect the current project model"
        , "  hx ci         Run the default verification flow"
        , "  hx test       Run the default test flow"
        , "  hx linker     Inspect or persist the local linker plan"
        , "  hx build      Run cabal build with an hx profile"
        , "  hx run        Run cabal run with an hx profile"
        , "  hx doctor     Inspect the local Haskell toolchain"
        , "  hx version    Print the current version"
        , "  hx help       Show this help text"
        ]
