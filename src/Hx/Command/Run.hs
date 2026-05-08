module Hx.Command.Run (runRun) where

import Control.Exception (IOException, try)
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import Hx.Command.CabalSupport
    ( CabalProfile (..)
    , linkerArgs
    , preflightLinkerPlan
    , preflightWarnings
    , profileRuntimeDefaults
    , profileRuntimeSupportBuildArgs
    , renderLinkerPlan
    , renderProfile
    , runCabalPreflight
    , runProfileArgs
    )
import Hx.Command.Doctor (DiagnosticSnapshot, defaultRunnableTargetArg, inspectDiagnostics, renderRunnableTargets)
import System.Exit (ExitCode, die, exitWith)
import System.IO (hFlush, stdout)
import System.Process (rawSystem)

data RunInvocation = RunInvocation
    { invocationProfile :: CabalProfile
    , invocationCabalArgs :: [String]
    , invocationRtsArgs :: [String]
    , invocationExecutableArgs :: [String]
    }

data RunTargetPlan
    = UseExplicitTarget
    | AutoSelectTarget String

data RuntimePlan = RuntimePlan
    { runtimeProfileDefaults :: [String]
    , runtimeSource :: RuntimeSource
    }

data RuntimeSource
    = NoExplicitRuntimeArgs
    | InjectRuntimeArgs [String]
    | UseExplicitExecutableRtsArgs [String]

runRun :: [String] -> IO ()
runRun rawArgs =
    if any (`elem` ["help", "--help", "-h"]) preArgs
        then putStrLn runUsage
        else
            case parseRunInvocation rawArgs of
                Left message ->
                    die message
                Right invocation -> do
                    snapshot <- inspectDiagnostics
                    case runCabalPreflight snapshot of
                        Left message ->
                            die message
                        Right preflight -> do
                            targetPlan <- resolveRunTargetPlan snapshot invocation
                            runtimePlan <- resolveRuntimePlan invocation
                            let cabalArgs =
                                    runProfileArgs (invocationProfile invocation)
                                        ++ runtimeBuildArgs (invocationProfile invocation) runtimePlan
                                        ++ linkerArgs (preflightLinkerPlan preflight)
                                        ++ targetArgs targetPlan
                                        ++ invocationCabalArgs invocation
                                        ++ executableSeparator (runtimeExecutableArgs runtimePlan (invocationExecutableArgs invocation))
                            putStrLn "hx run"
                            putStrLn ("Profile: " <> renderProfile (invocationProfile invocation))
                            mapM_ (putStrLn . ("Warning: " <>)) (preflightWarnings preflight)
                            putStrLn ("Target: " <> renderRunTargetPlan targetPlan)
                            putStrLn ("Runtime: " <> renderRuntimePlan runtimePlan)
                            putStrLn ("Linker: " <> renderLinkerPlan snapshot (preflightLinkerPlan preflight))
                            putStrLn ("Command: cabal " <> intercalate " " cabalArgs)
                            hFlush stdout
                            result <- try (rawSystem "cabal" cabalArgs) :: IO (Either IOException ExitCode)
                            case result of
                                Left _ ->
                                    die "Failed to execute `cabal`. Run `hx doctor` if the toolchain is not healthy."
                                Right exitCode ->
                                    exitWith exitCode
  where
    (preArgs, _execArgs) = splitExecutableArgs rawArgs

parseRunInvocation :: [String] -> Either String RunInvocation
parseRunInvocation rawArgs =
    case runtimeArgsFromFlags preArgs of
        Left message ->
            Left message
        Right runtimeArgs ->
            case (profileFromFlags preArgs, profileFromLeadingArg argsWithoutHxFlags) of
                (Left message, _) -> Left message
                (_, Left message) -> Left message
                (Right flagProfile, Right leadingProfile) ->
                    case resolveProfile flagProfile (fmap fst leadingProfile) of
                        Left message -> Left message
                        Right profile ->
                            Right
                                RunInvocation
                                    { invocationProfile = fromMaybe Dev profile
                                    , invocationCabalArgs = remainingCabalArgs leadingProfile argsWithoutHxFlags
                                    , invocationRtsArgs = runtimeArgs
                                    , invocationExecutableArgs = executableArgs
                                    }
  where
    (preArgs, executableArgs) = splitExecutableArgs rawArgs
    argsWithoutProfileFlags = filter (not . isProfileFlag) preArgs
    argsWithoutHxFlags = filter (not . isRuntimeFlag) argsWithoutProfileFlags

splitExecutableArgs :: [String] -> ([String], [String])
splitExecutableArgs args =
    case break (== "--") args of
        (beforeSeparator, []) -> (beforeSeparator, [])
        (beforeSeparator, _ : afterSeparator) -> (beforeSeparator, afterSeparator)

executableSeparator :: [String] -> [String]
executableSeparator args =
    case args of
        [] -> []
        _ -> "--" : args

remainingCabalArgs :: Maybe (CabalProfile, [String]) -> [String] -> [String]
remainingCabalArgs leadingProfile argsWithoutProfileFlags =
    case leadingProfile of
        Just (_, remaining) -> remaining
        Nothing -> argsWithoutProfileFlags

resolveRuntimePlan :: RunInvocation -> IO RuntimePlan
resolveRuntimePlan invocation =
    let defaults = profileRuntimeDefaults (invocationProfile invocation)
     in
    case (invocationRtsArgs invocation, executableArgsContainRtsBlock (invocationExecutableArgs invocation)) of
        (_ : _, True) ->
            die
                ( "Conflicting runtime arguments.\n\n"
                    <> "Use either `--rts=...` flags or pass raw `+RTS ... -RTS` arguments after `--`, but not both."
                )
        (runtimeArgs@(_ : _), False) ->
            pure RuntimePlan{runtimeProfileDefaults = defaults, runtimeSource = InjectRuntimeArgs runtimeArgs}
        ([], True) ->
            pure
                RuntimePlan
                    { runtimeProfileDefaults = defaults
                    , runtimeSource = UseExplicitExecutableRtsArgs (extractExplicitExecutableRtsArgs (invocationExecutableArgs invocation))
                    }
        ([], False) ->
            pure RuntimePlan{runtimeProfileDefaults = defaults, runtimeSource = NoExplicitRuntimeArgs}

runtimeBuildArgs :: CabalProfile -> RuntimePlan -> [String]
runtimeBuildArgs profile runtimePlan =
    case effectiveRuntimeArgs runtimePlan of
        [] ->
            []
        runtimeArgs ->
            filter
                (`notElem` profileRuntimeSupportBuildArgs profile)
                ( nub
                    ( ["--ghc-option=-rtsopts"]
                        ++ if runtimeArgsRequireThreaded runtimeArgs
                            then ["--ghc-option=-threaded"]
                            else []
                    )
                )

effectiveRuntimeArgs :: RuntimePlan -> [String]
effectiveRuntimeArgs runtimePlan =
    runtimeProfileDefaults runtimePlan ++ explicitRuntimeArgs runtimePlan

explicitRuntimeArgs :: RuntimePlan -> [String]
explicitRuntimeArgs runtimePlan =
    case runtimeSource runtimePlan of
        NoExplicitRuntimeArgs ->
            []
        InjectRuntimeArgs runtimeArgs ->
            runtimeArgs
        UseExplicitExecutableRtsArgs runtimeArgs ->
            runtimeArgs

runtimeExecutableArgs :: RuntimePlan -> [String] -> [String]
runtimeExecutableArgs runtimePlan executableArgs =
    case runtimeSource runtimePlan of
        NoExplicitRuntimeArgs ->
            executableArgs
        InjectRuntimeArgs runtimeArgs ->
            ["+RTS"] ++ runtimeArgs ++ ["-RTS"] ++ executableArgs
        UseExplicitExecutableRtsArgs _ ->
            executableArgs

renderRuntimePlan :: RuntimePlan -> String
renderRuntimePlan runtimePlan =
    case (runtimeProfileDefaults runtimePlan, runtimeSource runtimePlan) of
        ([], NoExplicitRuntimeArgs) ->
            "no explicit RTS arguments"
        (defaults, NoExplicitRuntimeArgs) ->
            "profile defaults via `-with-rtsopts`: " <> intercalate " " defaults
        ([], InjectRuntimeArgs runtimeArgs) ->
            "injecting RTS args via `+RTS ... -RTS`: " <> intercalate " " runtimeArgs
        (defaults, InjectRuntimeArgs runtimeArgs) ->
            "profile defaults via `-with-rtsopts`: "
                <> intercalate " " defaults
                <> "; additional RTS args via `+RTS ... -RTS`: "
                <> intercalate " " runtimeArgs
        ([], UseExplicitExecutableRtsArgs runtimeArgs) ->
            "respecting explicit executable RTS arguments: " <> intercalate " " runtimeArgs
        (defaults, UseExplicitExecutableRtsArgs runtimeArgs) ->
            "profile defaults via `-with-rtsopts`: "
                <> intercalate " " defaults
                <> "; respecting additional executable RTS arguments: "
                <> intercalate " " runtimeArgs

executableArgsContainRtsBlock :: [String] -> Bool
executableArgsContainRtsBlock =
    any (== "+RTS")

extractExplicitExecutableRtsArgs :: [String] -> [String]
extractExplicitExecutableRtsArgs executableArgs =
    case dropWhile (/= "+RTS") executableArgs of
        [] ->
            []
        _ : runtimeArgs ->
            takeWhile (/= "-RTS") runtimeArgs

runtimeArgsRequireThreaded :: [String] -> Bool
runtimeArgsRequireThreaded =
    any requiresThreadedRuntimeArg

requiresThreadedRuntimeArg :: String -> Bool
requiresThreadedRuntimeArg runtimeArg =
    "-N" `prefixOf` runtimeArg

resolveRunTargetPlan :: DiagnosticSnapshot -> RunInvocation -> IO RunTargetPlan
resolveRunTargetPlan snapshot invocation =
    if hasExplicitRunTarget (invocationCabalArgs invocation)
        then pure UseExplicitTarget
        else
            case defaultRunnableTargetArg snapshot of
                Just targetName ->
                    pure (AutoSelectTarget targetName)
                Nothing ->
                    die
                        ( "Unable to choose a default runnable component.\n\n"
                            <> "Available executable-like components: "
                            <> renderRunnableTargets snapshot
                            <> "\n\nPass an explicit target to `hx run`."
                        )

hasExplicitRunTarget :: [String] -> Bool
hasExplicitRunTarget =
    any (not . isFlagLike)

isFlagLike :: String -> Bool
isFlagLike value =
    "-" `prefixOf` value

isRuntimeFlag :: String -> Bool
isRuntimeFlag =
    looksLikeRuntimeFlag

looksLikeRuntimeFlag :: String -> Bool
looksLikeRuntimeFlag arg =
    "--rts=" `prefixOf` arg

parseRuntimeFlag :: String -> Maybe String
parseRuntimeFlag arg =
    case stripPrefix "--rts=" arg of
        Just runtimeArg
            | not (null runtimeArg) ->
                Just runtimeArg
        _ ->
            Nothing

runtimeArgsFromFlags :: [String] -> Either String [String]
runtimeArgsFromFlags args =
    traverse parseRuntimeFlagOrFail [arg | arg <- args, isRuntimeFlag arg]

parseRuntimeFlagOrFail :: String -> Either String String
parseRuntimeFlagOrFail arg =
    case parseRuntimeFlag arg of
        Just runtimeArg ->
            Right runtimeArg
        Nothing ->
            Left ("Malformed runtime flag: " <> arg <> "\n\n" <> runUsage)

targetArgs :: RunTargetPlan -> [String]
targetArgs targetPlan =
    case targetPlan of
        UseExplicitTarget -> []
        AutoSelectTarget targetName -> [targetName]

renderRunTargetPlan :: RunTargetPlan -> String
renderRunTargetPlan targetPlan =
    case targetPlan of
        UseExplicitTarget -> "using explicit cabal run target"
        AutoSelectTarget targetName -> "auto-selecting `" <> targetName <> "`"

profileFromFlags :: [String] -> Either String (Maybe CabalProfile)
profileFromFlags args =
    case traverse parseProfileFlag [arg | arg <- args, isProfileFlag arg] of
        Left message -> Left message
        Right [] -> Right Nothing
        Right [profile] -> Right (Just profile)
        Right profiles ->
            if all (== head profiles) profiles
                then Right (Just (head profiles))
                else Left "Conflicting `--profile=` flags.\n\nUse one of: dev, release, server."

profileFromLeadingArg :: [String] -> Either String (Maybe (CabalProfile, [String]))
profileFromLeadingArg args =
    case args of
        [] ->
            Right Nothing
        firstArg : restArgs ->
            case parseProfileName firstArg of
                Just profile -> Right (Just (profile, restArgs))
                Nothing ->
                    if looksLikeProfileFlag firstArg
                        then Left ("Unknown run profile: " <> firstArg <> "\n\n" <> runUsage)
                        else Right Nothing

resolveProfile :: Maybe CabalProfile -> Maybe CabalProfile -> Either String (Maybe CabalProfile)
resolveProfile flagProfile leadingProfile =
    case (flagProfile, leadingProfile) of
        (Nothing, Nothing) -> Right Nothing
        (Just profile, Nothing) -> Right (Just profile)
        (Nothing, Just profile) -> Right (Just profile)
        (Just byFlag, Just byArg)
            | byFlag == byArg -> Right (Just byFlag)
            | otherwise ->
                Left "Conflicting run profiles.\n\nUse one of: dev, release, server."

isProfileFlag :: String -> Bool
isProfileFlag =
    looksLikeProfileFlag

looksLikeProfileFlag :: String -> Bool
looksLikeProfileFlag arg =
    "--profile=" `prefixOf` arg

parseProfileFlag :: String -> Either String CabalProfile
parseProfileFlag arg =
    case stripPrefix "--profile=" arg of
        Just profileName ->
            case parseProfileName profileName of
                Just profile -> Right profile
                Nothing -> Left ("Unknown run profile: " <> profileName <> "\n\n" <> runUsage)
        Nothing -> Left ("Malformed profile flag: " <> arg <> "\n\n" <> runUsage)

parseProfileName :: String -> Maybe CabalProfile
parseProfileName rawName =
    case rawName of
        "dev" -> Just Dev
        "release" -> Just Release
        "server" -> Just Server
        _ -> Nothing

runUsage :: String
runUsage =
    unlines
        [ "hx run - profile-aware cabal run wrapper"
        , ""
        , "Usage:"
        , "  hx run"
        , "  hx run <profile> [cabal run args...] [--rts=<arg> ...] [-- executable args...]"
        , "  hx run --profile=<profile> [cabal run args...] [--rts=<arg> ...] [-- executable args...]"
        , ""
        , "Profiles:"
        , "  dev      Disable optimization and split sections for fast iteration"
        , "  release  Run with a release-style build"
        , "  server   Release profile plus embedded `-N` runtime defaults"
        , ""
        , "Runtime:"
        , "  --rts=<arg>  Inject executable RTS args without spelling `+RTS ... -RTS` manually"
        , ""
        , "Examples:"
        , "  hx run"
        , "  hx run release my-exe"
        , "  hx run release my-exe --rts=-N --rts=-A64m -- --port 8080"
        , "  hx run --profile=server my-exe -- --port 8080"
        ]

prefixOf :: String -> String -> Bool
prefixOf prefix value =
    take (length prefix) value == prefix

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value =
    if prefixOf prefix value
        then Just (drop (length prefix) value)
        else Nothing
