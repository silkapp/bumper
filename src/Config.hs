{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Config where

import Data.Label
import Data.List.Split
import qualified Data.Map as M
import Data.Version
import Distribution.Package
import Distribution.Text
import System.Console.GetOpt
import System.Environment

data Action = Run
            | ShowDeps
            | ShowHelp
            | ShowVersion
            deriving Show

data Config = Config
  { _bump       :: M.Map Int [PackageName] -- Map of which packages to bump at which position
  , _setVersion :: [(PackageName,Version)]
  , _transitive :: Bool
  , _ignore     :: [PackageName]
  , _global     :: Maybe String
  , _action     :: Action
  } deriving Show

$(mkLabels [''Config])

defaultConfig :: Config
defaultConfig = Config
    { _bump       = M.empty
    , _setVersion = []
    , _transitive = True
    , _ignore     = []
    , _global     = Nothing
    , _action     = Run
    }

versionPar :: String -> [(PackageName, Version)]
versionPar = foldr addVer [] . splitOn ","
  where addVer fld ac =
          case splitOn "@" fld of
            [p,v] -> case simpleParse v of
                        (Just ver) -> (PackageName p, ver) : ac
                        Nothing    -> ac
            _     -> ac

options :: [OptDescr (Config -> Config)]
options = [ Option ['m'] ["major"]         (ReqArg (addBumps 1) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a major bump (bump at position 1)."
          , Option ['n'] ["minor"]         (ReqArg (addBumps 2) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a minor bump (bump at position 2)."
          , Option ['0'] ["bump-0"]        (ReqArg (addBumps 0) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a bump at position 0."
          , Option ['1'] ["bump-1"]        (ReqArg (addBumps 1) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a bump at position 1."
          , Option ['2'] ["bump-2"]        (ReqArg (addBumps 2) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a bump at position 2."
          , Option ['3'] ["bump-3"]        (ReqArg (addBumps 3) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will get a bump at position 3."
          , Option []    ["set-versions"]  (ReqArg (\v -> modify setVersion (++ versionPar v)) "PACKAGE@VERSION(,PACKAGE@VERSION)*") "Comma-separated list of packages and their versions."
          , Option ['t'] ["no-transitive"] (NoArg  (set transitive False))   "Do not apply bumping transitively."
          , Option ['i'] ["ignore"]        (ReqArg (\v -> modify ignore (++ map PackageName (splitOn "," v))) "PACKAGE(,PACKAGE)*") "Comma-separated list of packages which will be ignored when transitive bumping."
          , Option ['g'] ["global"]        (ReqArg (\v -> set global (Just v)) "PATH")     "Bump according to latest version number in the given package database."
          , Option ['d'] ["dry-run"]       (NoArg  (set action ShowDeps))                  "Just output the dependencies that will be updated."
          , Option ['?'] ["help"]          (NoArg  (set action ShowHelp))                  "Show usage help and exit."
          , Option ['v'] ["version"]       (NoArg  (set action ShowVersion))               "Show version info and exit."
          ]
      where addBumps :: Int -> String -> Config -> Config
            addBumps p pks = modify bump (M.insertWith (++) p (map PackageName $ splitOn "," pks))

getConfig :: IO Config
getConfig =
  do args <- getArgs
     (opts, _) <- processArgs defaultConfig options header args
     return opts

processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs def opts hdr args =
    case getOpt Permute opts args of
        (oargs, nonopts, []    ) -> return (foldl (flip ($)) def oargs, nonopts)
        (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo hdr opts

header :: String
header = "Usage: bumper [OPTIONS...], with the following options:"

printUsage :: [OptDescr a] -> IO ()
printUsage opts = putStrLn $ usageInfo header opts
