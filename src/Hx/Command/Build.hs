module Hx.Command.Build (runBuild) where

import Control.Exception (IOException, try)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Hx.Command.CabalSupport
    ( CabalProfile (..)
    , buildProfileArgs
    , linkerArgs
    , preflightLinkerPlan
    , preflightWarnings
    , renderLinkerPlan
    , renderProfile
    , runCabalPreflight
    )
import Hx.Command.Doctor (inspectDiagnostics)
import System.Exit (ExitCode, die, exitWith)
import System.IO (hFlush, stdout)
import System.Process (rawSystem)

data BuildInvocation = BuildInvocation
    { invocationProfile :: CabalProfile
    , invocationExtraArgs :: [String]
    }

runBuild :: [String] -> IO ()
runBuild args =
    if any (`elem` ["help", "--help", "-h"]) args
        then putStrLn buildUsage
        else
            case parseBuildInvocation args of
                Left message ->
                    die message
                Right invocation -> do
                    snapshot <- inspectDiagnostics
                    case runCabalPreflight snapshot of
                        Left message ->
                            die message
                        Right preflight -> do
                            let cabalArgs =
                                    buildProfileArgs (invocationProfile invocation)
                                        ++ linkerArgs (preflightLinkerPlan preflight)
                                        ++ invocationExtraArgs invocation
                            putStrLn "hx build"
                            putStrLn ("Profile: " <> renderProfile (invocationProfile invocation))
                            mapM_ (putStrLn . ("Warning: " <>) ) (preflightWarnings preflight)
                            putStrLn ("Linker: " <> renderLinkerPlan snapshot (preflightLinkerPlan preflight))
                            putStrLn ("Command: cabal " <> intercalate " " cabalArgs)
                            hFlush stdout
                            result <- try (rawSystem "cabal" cabalArgs) :: IO (Either IOException ExitCode)
                            case result of
                                Left _ ->
                                    die "Failed to execute `cabal`. Run `hx doctor` if the toolchain is not healthy."
                                Right exitCode ->
                                    exitWith exitCode

parseBuildInvocation :: [String] -> Either String BuildInvocation
parseBuildInvocation rawArgs =
    case (profileFromFlags args, profileFromLeadingArg argsWithoutProfileFlags) of
        (Left message, _) -> Left message
        (_, Left message) -> Left message
        (Right flagProfile, Right leadingProfile) ->
            case resolveProfile flagProfile (fmap fst leadingProfile) of
                Left message -> Left message
                Right profile ->
                    Right
                        BuildInvocation
                            { invocationProfile = fromMaybe Dev profile
                            , invocationExtraArgs =
                                case leadingProfile of
                                    Just (_, remaining) -> remaining
                                    Nothing -> argsWithoutProfileFlags
                            }
  where
    args = filter (/= "--") rawArgs
    argsWithoutProfileFlags = filter (not . isProfileFlag) args

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
                        then Left ("Unknown build profile: " <> firstArg <> "\n\n" <> buildUsage)
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
                Left "Conflicting build profiles.\n\nUse one of: dev, release, server."

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
                Nothing -> Left ("Unknown build profile: " <> profileName <> "\n\n" <> buildUsage)
        Nothing -> Left ("Malformed profile flag: " <> arg <> "\n\n" <> buildUsage)

parseProfileName :: String -> Maybe CabalProfile
parseProfileName rawName =
    case rawName of
        "dev" -> Just Dev
        "release" -> Just Release
        "server" -> Just Server
        _ -> Nothing

buildUsage :: String
buildUsage =
    unlines
        [ "hx build - profile-aware cabal build wrapper"
        , ""
        , "Usage:"
        , "  hx build"
        , "  hx build <profile> [cabal build args...]"
        , "  hx build --profile=<profile> [cabal build args...]"
        , ""
        , "Profiles:"
        , "  dev      Disable optimization and split sections for fast iteration"
        , "  release  Build with -O2 and split sections"
        , "  server   Release profile plus embedded `-N` runtime defaults"
        , ""
        , "Examples:"
        , "  hx build"
        , "  hx build release exe:my-app"
        , "  hx build --profile=server --dry-run"
        ]

prefixOf :: String -> String -> Bool
prefixOf prefix value =
    take (length prefix) value == prefix

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value =
    if prefixOf prefix value
        then Just (drop (length prefix) value)
        else Nothing
