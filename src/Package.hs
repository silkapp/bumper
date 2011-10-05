{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Package where

import Control.Applicative
import Control.Monad
import Data.Label
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import System.IO
import System.Process
import Version

data Package = Package
    { _name         :: String
    , _path         :: String
    , _version      :: Version
    , _dependencies :: [String]
    , _dependants   :: [String]
    } deriving (Show, Eq, Ord)

$(mkLabels [''Package])

type Packages = [Package]

-- | Loading functions    
packages :: IO Packages
packages = 
  do (_, hOut, _, _) <- runInteractiveCommand "find . -name *.cabal -type f"
     paths <- lines <$> hGetContents hOut
     ps <- forM paths $ \p -> do
            v <- getVersion p
            n <- getName p
            return $ Package n p v [] []
     makeDependants <$> makeDependencies ps

getName :: String -> IO String
getName p = 
  do (_, hOut, _, _) <- runInteractiveCommand $ "cat " ++ p ++ " | sed -n 's/^[ \\t]*[Nn]ame[ \\t]*:[ \\t]*\\(.*\\)/\\1/g p'"
     head . lines <$> hGetContents hOut

getVersion :: String -> IO [Int]
getVersion p =
  do (_, hOut, _, _) <- runInteractiveCommand $ "cat " ++ p ++ " | sed -n 's/^[ \\t]*[Vv]ersion[ \\t]*:[ \\t]*\\(.*\\)/\\1/g p'"
     fmap read . splitOn "." <$> hGetContents hOut

makeDependencies :: Packages -> IO Packages
makeDependencies ps = 
  do let names = map _name ps
         qs = intercalate "\n" $ map (\n -> "s/^[ \\t,]*" ++ n ++ "[ \t].*/" ++ n ++ "/g p") names  
     forM ps $ \p -> do
        (_, hOut, _, _) <- runInteractiveCommand $ "sed -n '" ++ qs ++ "' " ++ (_path p)
        deps <- lines <$> hGetContents hOut
        return (set dependencies deps p)

makeDependants :: Packages -> Packages
makeDependants ps =
  do p  <- ps
     return $ set dependants [ _name p' | p' <- ps, elem (_name p) (_dependencies p')] p

getBaseVersions :: String -> Packages -> IO Packages
getBaseVersions ind ps =
  do (_, hOut, _, _) <- runInteractiveCommand $ "tar -tf " ++ ind
     gps <- lines <$> hGetContents hOut
     let vs = map ((\(n:v:_) -> (n, parseVersion v)) . splitOn "/") gps
         globver = M.fromListWith (\a b -> if a > b then a else b) vs
         updVer p = modify version (maybe id (\v -> if _version p < v then const v else id) $ M.lookup (_name p) globver) p
     return $ map updVer ps

-- | Manipulation functions

lookupPackage :: String -> Packages -> Maybe Package
lookupPackage s = find ((== s) . _name)

lookupPackages ::  Packages -> [String] -> Packages
lookupPackages ps = catMaybes . map (flip lookupPackage ps)

hasPackage :: Package -> Packages -> Bool
hasPackage p = isJust . lookupPackage (_name p)

removePackage :: String -> Packages -> Packages
removePackage s = filter ((/= s). _name)

removeAll :: [String] -> Packages -> Packages 
removeAll = flip $ foldr removePackage

-- | Code for applying bumps

bumpAll :: Packages -> Package -> IO ()
bumpAll ps p =
  do putStrLn $ "Bumping " ++ _name p ++ " to " ++ showVersion (_version p)
     bumpPackage p
     putStrLn $ "Bumping dependant packages: " ++ show (_dependants p)
     bumpConstraints p ps

bumpPackage :: Package -> IO ()
bumpPackage p =
  do pid <- runCommand $ "sed -i -e 's/^\\([ \\t]*[Vv]ersion[ \\t]*:[ \\t]*\\).*/\\1" ++ showVersion (_version p) ++ "/g' '" ++ _path p ++ "'"
     waitForProcess pid
     return ()

bumpConstraint :: Package -> Package -> IO ()
bumpConstraint depp inp =
    let (v0: v1: v2: v3: _) = map show $ _version depp ++ repeat 0
        qs =
           [ -- Replace entire number when entire number is used for equality
             "s/\\(==[ \\t]*\\)\\([0-9]\\+\\.\\)*[0-9]\\+\\([ \\t]\\|$\\)/\\1" ++ showVersion (_version depp) ++ "\\3/g"
             -- Replace individual digits when ending with asterisk
           , "s/\\(==[ \\t]*\\)[0-9]\\+\\(.*\\*\\)/\\1" ++ v0 ++"\\2/g"
           , "s/\\(==[ \\t]*[0-9]\\+\\.\\)[0-9]\\+\\(.*\\*\\)/\\1" ++ v1 ++ "\\2/g"
           , "s/\\(==[ \\t]*\\([0-9]\\+\\.\\)\\{2\\}\\)[0-9]\\+\\(.*\\*\\)/\\1" ++ v2 ++ "\\3/g"
           , "s/\\(==[ \\t]*\\([0-9]\\+\\.\\)\\{3\\}\\)[0-9]\\+\\(.*\\*\\)/\\1" ++ v3 ++ "\\3/g"
             -- Replace lower than constraints
           , "s/<\\([ \\t]*\\)\\([0-9]\\+\\.\\)*[0-9]\\+/<=\\1" ++ showVersion (_version depp) ++ "/g"
           , "s/\\(<=[ \\t]*\\)\\([0-9]\\+\\.\\)*[0-9]\\+/\\1" ++ showVersion (_version depp) ++ "/g"
           ]
        mkLine q = "sed -i -e '/" ++ _name depp ++ "/ " ++ q ++ "' '" ++ _path inp ++ "'"
    in do putStrLn $ "Bumping dependencies on " ++ _name depp ++ " in " ++ _name inp
          pid <- mapM_ (\q -> runCommand (mkLine q) >>= waitForProcess) qs
          return ()

bumpConstraints :: Package -> Packages -> IO ()
bumpConstraints p = mapM_ (bumpConstraint p) . flip lookupPackages (_dependants p)
