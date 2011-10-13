{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Config where

import Data.Label
import Data.List.Split
import System.Console.GetOpt
import System.Environment
import Version

data Config = Config
  { _bumpMajor  :: [String]
  , _bumpMinor  :: [String]
  , _setVersion :: [(String,Version)]
  , _ignore     :: [String]
  , _transitive :: Bool
  , _global     :: Maybe String
  , _showDeps   :: Bool
  } deriving Show

$(mkLabels [''Config])

defaultConfig :: Config
defaultConfig = Config
    { _bumpMajor = []
    , _bumpMinor = []
    , _setVersion = []
    , _ignore     = ["server"]
    , _transitive = False
    , _global     = Nothing
    , _showDeps   = False
    }

versionPar :: String -> [(String, Version)]
versionPar = foldr addVer [] . splitOn ","
  where addVer ver ac = 
          case splitOn "@" ver of
            [p,v] -> (p, parseVersion v) : ac
            _     -> ac

options :: [OptDescr (Config -> Config)]
options = [ Option ['m'] ["major"]      (ReqArg (\v -> modify bumpMajor (++ splitOn "," v)) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a major bump"
          , Option ['l'] ["minor"]      (ReqArg (\v -> modify bumpMinor (++ splitOn "," v)) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a minor bump"
          , Option ['v'] ["versions"]   (ReqArg (\v -> modify setVersion (++ versionPar v)) "PACKAGE@VERSION(,PACKAGE@VERSION)*") "Comma-separated list of packages and their versions"
          , Option ['i'] ["ignore"]     (ReqArg (\v -> modify ignore (++ splitOn "," v)) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will be ignored when transitive bumping"
          , Option ['t'] ["transitive"] (NoArg  (set transitive True))   "Apply bumping transitively"
          , Option ['g'] ["global"]     (OptArg  (set global) "PATH")   "Bump according to latest version number in package database"
          , Option ['d'] ["dependants"] (NoArg (set showDeps True))     "Just output the dependencies that will be updated"
          ]

getConfig :: IO Config
getConfig =
  do args <- getArgs
     (opts, _) <- processArgs defaultConfig options "Usage: deploy [OPTIONS...], with the following options (no options deploys as a library):" args
     return opts

processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs def opts header args =
    case getOpt Permute opts args of
        (oargs, nonopts, []    ) -> return (foldl (flip ($)) def oargs, nonopts)
        (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo header opts
