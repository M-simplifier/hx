module Hx.Command.CabalSupport
    ( CabalPreflight (..)
    , CabalProfile (..)
    , LinkerPlan (..)
    , buildProfileArgs
    , decideLinkerPlan
    , fuseLdValue
    , linkerArgs
    , linkerPlanWarnings
    , profileRuntimeDefaults
    , profileRuntimeSupportBuildArgs
    , renderLinkerPlan
    , renderProfile
    , runCabalPreflight
    , runProfileArgs
    )
where

import Data.List (intercalate)
import Hx.Command.Doctor
    ( DiagnosticSnapshot (..)
    , buildPkgConfigBlocker
    , buildPkgConfigWarnings
    , configuredProjectLinkers
    , firstMissingConfiguredProjectLinker
    , preferredFastLinkerName
    , projectHasGenericLinkerFlags
    , renderProjectLinkerHints
    )

data CabalProfile
    = Dev
    | Release
    | Server
    deriving (Eq, Show)

data LinkerPlan
    = RespectProjectLinker [String]
    | AutoSelectFastLinker String
    | NoFastLinkerAvailable
    | NoFastLinkerAvailableWithProjectFlags
    | MissingProjectLinker String

data CabalPreflight = CabalPreflight
    { preflightWarnings :: [String]
    , preflightLinkerPlan :: LinkerPlan
    }

buildProfileArgs :: CabalProfile -> [String]
buildProfileArgs profile =
    "build" : commonProfileArgs profile

runProfileArgs :: CabalProfile -> [String]
runProfileArgs profile =
    "run" : commonProfileArgs profile

commonProfileArgs :: CabalProfile -> [String]
commonProfileArgs profile =
    profileOptimizationArgs profile
        ++ profileRuntimeSupportBuildArgs profile
        ++ profileEmbeddedRuntimeBuildArgs profile

profileOptimizationArgs :: CabalProfile -> [String]
profileOptimizationArgs profile =
    case profile of
        Dev ->
            [ "--disable-optimization"
            , "--disable-split-sections"
            , "--disable-profiling"
            ]
        Release ->
            [ "--enable-optimization=2"
            , "--enable-split-sections"
            , "--disable-profiling"
            ]
        Server ->
            [ "--enable-optimization=2"
            , "--enable-split-sections"
            , "--disable-profiling"
            ]

profileRuntimeSupportBuildArgs :: CabalProfile -> [String]
profileRuntimeSupportBuildArgs profile =
    case profileRuntimeDefaults profile of
        [] ->
            []
        runtimeArgs ->
            [ "--ghc-option=-rtsopts"
            ]
                ++ if any requiresThreadedRuntimeArg runtimeArgs
                    then ["--ghc-option=-threaded"]
                    else []

profileRuntimeDefaults :: CabalProfile -> [String]
profileRuntimeDefaults profile =
    case profile of
        Server -> ["-N"]
        _ -> []

profileEmbeddedRuntimeBuildArgs :: CabalProfile -> [String]
profileEmbeddedRuntimeBuildArgs profile =
    case profileRuntimeDefaults profile of
        [] ->
            []
        [runtimeArg] ->
            ["--ghc-option=-with-rtsopts=" <> runtimeArg]
        runtimeArgs ->
            ["--ghc-option=-with-rtsopts=" <> unwords runtimeArgs]

runCabalPreflight :: DiagnosticSnapshot -> Either String CabalPreflight
runCabalPreflight snapshot =
    case (buildPkgConfigBlocker snapshot, decideLinkerPlan snapshot) of
        (Just message, _) ->
            Left message
        (_, MissingProjectLinker linkerName) ->
            Left
                ( "The current project already selects `"
                    <> linkerName
                    <> "`, but that command is not available on this host.\n\n"
                    <> "Project linker wiring: "
                    <> renderProjectLinkerHints (snapshotProjectDiagnostics snapshot)
                    <> "\n\nRun `hx doctor` for installation guidance."
                )
        _ ->
            let linkerPlan = decideLinkerPlan snapshot
             in
            Right
                CabalPreflight
                    { preflightWarnings =
                        buildPkgConfigWarnings snapshot
                            ++ linkerPlanWarnings linkerPlan
                    , preflightLinkerPlan = linkerPlan
                    }

decideLinkerPlan :: DiagnosticSnapshot -> LinkerPlan
decideLinkerPlan snapshot =
    case firstMissingConfiguredProjectLinker (snapshotProjectDiagnostics snapshot) of
        Just linkerName ->
            MissingProjectLinker linkerName
        Nothing ->
            case configuredProjectLinkers (snapshotProjectDiagnostics snapshot) of
                linkerNames@(_ : _) ->
                    RespectProjectLinker linkerNames
                [] ->
                    case preferredFastLinkerName snapshot of
                        Just linkerName ->
                            AutoSelectFastLinker linkerName
                        Nothing ->
                            if projectHasGenericLinkerFlags (snapshotProjectDiagnostics snapshot)
                                then NoFastLinkerAvailableWithProjectFlags
                                else NoFastLinkerAvailable

linkerArgs :: LinkerPlan -> [String]
linkerArgs linkerPlan =
    case linkerPlan of
        AutoSelectFastLinker linkerName ->
            ["--ghc-option=-optl-fuse-ld=" <> fuseLdValue linkerName]
        _ ->
            []

linkerPlanWarnings :: LinkerPlan -> [String]
linkerPlanWarnings linkerPlan =
    case linkerPlan of
        AutoSelectFastLinker "mold" ->
            [ "GHC may print `Warning: Couldn't figure out linker information!` when using `mold`; if Cabal exits successfully, that message is a non-fatal GHC linker-classification warning."
            ]
        _ ->
            []

renderLinkerPlan :: DiagnosticSnapshot -> LinkerPlan -> String
renderLinkerPlan snapshot linkerPlan =
    case linkerPlan of
        RespectProjectLinker linkerNames ->
            "respecting project linker setting: " <> intercalate ", " linkerNames
        AutoSelectFastLinker linkerName ->
            if projectHasGenericLinkerFlags (snapshotProjectDiagnostics snapshot)
                then "augmenting existing project linker flags with `" <> linkerName <> "` for this invocation"
                else "auto-selecting `" <> linkerName <> "` for this invocation"
        NoFastLinkerAvailableWithProjectFlags ->
            "no fast linker available; keeping the project's existing linker flags"
        NoFastLinkerAvailable ->
            "no fast linker available; using the project's current linker posture"
        MissingProjectLinker linkerName ->
            "missing configured project linker `" <> linkerName <> "`"

fuseLdValue :: String -> String
fuseLdValue linkerName =
    case linkerName of
        "ld.lld" -> "lld"
        _ -> linkerName

renderProfile :: CabalProfile -> String
renderProfile profile =
    case profile of
        Dev -> "dev"
        Release -> "release"
        Server -> "server"

requiresThreadedRuntimeArg :: String -> Bool
requiresThreadedRuntimeArg runtimeArg =
    "-N" `prefixOf` runtimeArg

prefixOf :: String -> String -> Bool
prefixOf prefix value =
    take (length prefix) value == prefix
