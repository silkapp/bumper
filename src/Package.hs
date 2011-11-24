-- * Contains helper functions to load and manipulate .cabal files
{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  , TupleSections
  #-}
module Package where

import Prelude hiding (readFile)

import Control.Applicative
import Control.Monad
import Data.Label
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Version
import Distribution.Package hiding (Package)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Text
import Distribution.Verbosity
import System.IO.Strict
import System.Process
import Text.Regex

data Package = Package
    { _name         :: PackageName
    , _path         :: String
    , _version      :: Version
    , _dependencies :: [Dependency]
    , _dependants   :: [PackageName]
    } deriving (Show, Eq)

$(mkLabels [''Package])

type Packages = [Package]

-- | Helper functions

lookupPackage :: PackageName -> Packages -> Maybe Package
lookupPackage s = find ((== s) . _name)

lookupPackages ::  [PackageName] -> Packages -> Packages
lookupPackages ns ps = catMaybes . map (flip lookupPackage ps) $ ns

hasPackage :: PackageName -> Packages -> Bool
hasPackage n = isJust . lookupPackage n

removePackage :: PackageName -> Packages -> Packages
removePackage s = filter ((/= s). _name)

removeAll :: [PackageName] -> Packages -> Packages
removeAll = flip $ foldr removePackage

-- | Loading packages
packages :: IO Packages
packages =
  do (_, hOut, _, _) <- runInteractiveCommand "find . -name *.cabal -type f"
     paths <- lines <$> hGetContents hOut
     ps <- forM paths $ \p ->
            do pkg <- flattenPackageDescription `liftM` readPackageDescription normal p
               return $ Package
                          { _name = pkgName $ package pkg
                          , _path = p
                          , _version = pkgVersion $ package pkg
                          , _dependencies = buildDepends pkg
                          , _dependants = []
                          }
     return $ makeDependants ps

-- | Calculate the dependants from the dependencies
-- the boolean represents whether packages which are out of the version range of dependencies are treated as dependants
makeDependants :: Packages -> Packages
makeDependants ps =
  do p <- ps
     return $ set dependants [ _name p' | p' <- ps, any (\(Dependency n _) -> n == _name p) (_dependencies p')] p

getBaseVersions :: String -> Packages -> IO Packages
getBaseVersions ind ps =
 do (_, hOut, _, _) <- runInteractiveCommand $ "tar -tf " ++ ind
    gps <- lines <$> hGetContents hOut
    let vs = catMaybes $ map (parseVer . splitOn "/") gps
        parseVer (n:v:_) = fmap (PackageName n, ) $ simpleParse v
        parseVer _       = Nothing
        globver = M.fromListWith (\a b -> if a > b then a else b) vs
        updVer p = modify version (maybe id (\v -> if get version p < v then const v else id) $ M.lookup (_name p) globver) p
    return $ map updVer ps

-- | Manipulating package contents
whiteReg :: String
whiteReg = "[ \n\t]*"

modifyVersion :: Version -> String -> String
modifyVersion v s = subRegex (mkRegexWithOpts regex False False) s result
  where regex = "(version" ++ whiteReg ++ ":" ++ whiteReg ++ ") ([0-9.a-zA-Z]+)"
        result = "\\1 " ++ display v

modifyDependency :: Dependency -> String -> String
modifyDependency (Dependency nm range) s = subRegex (mkRegexWithOpts regex False False) s result
  where regex = "(build-depends" ++ whiteReg ++ ":" ++ "[^:]*"
              ++ "[ ,\n\t]" ++ display nm ++ whiteReg ++ ")[, ]([" ++ rangeChar ++ " \t\n]*[" ++ rangeChar ++ "])"
        rangeChar = "0-9.*&|()<>="
        result = "\\1 " ++ display range

-- | Data structure containing package modifications
type PackageChanges = (Maybe Version, [Dependency])

-- | Writing to packages
modifyPackage :: PackageChanges -> String -> String
modifyPackage (mv, deps) = flip (foldr modifyDependency) deps
                         . maybe id modifyVersion mv

updatePackage :: Package -> PackageChanges -> IO ()
updatePackage p ch = readFile (get path p) >>= writeFile (get path p) . modifyPackage ch
