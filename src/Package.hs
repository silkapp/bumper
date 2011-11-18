{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Package where

import Prelude hiding (readFile)

import Control.Applicative
import Control.Monad
import Data.Label
import Data.List
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
import Version

data Package = Package
    { _name         :: PackageName
    , _path         :: String
    , _version      :: Version
    , _dependencies :: [Dependency]
    , _dependants   :: [PackageName]
    } deriving (Show, Eq)

$(mkLabels [''Package])

type Packages = [Package]

-- | Accessor functions

lookupPackage :: PackageName -> Packages -> Maybe Package
lookupPackage s = find ((== s) . _name)

lookupPackages ::  Packages -> [PackageName] -> Packages
lookupPackages ps = catMaybes . map (flip lookupPackage ps)

hasPackage :: Package -> Packages -> Bool
hasPackage p = isJust . lookupPackage (_name p)

removePackage :: PackageName -> Packages -> Packages
removePackage s = filter ((/= s). _name)

removeAll :: [PackageName] -> Packages -> Packages
removeAll = flip $ foldr removePackage

-- | Creates packages which represent the difference between the original and updated packages
diffPackages :: Packages -> Packages -> Packages
diffPackages original new = concat $
  do orig <- original
     n    <- new
     let changedDeps = _dependencies n \\ _dependencies orig
     if _name orig == _name n && (_version orig /= _version n || not (null changedDeps))
      then return [orig { _version = _version n, _dependencies = changedDeps }]
      else return []

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
     return $ set dependants [ _name p' | p' <- ps, any (\(Dependency n _) -> n == _name p') (_dependencies p')] p

-- | Update all dependencies on one package
updateDependencies :: Package -> Packages -> Packages
updateDependencies p = map $ modify dependencies (map updateDep)
  where updateDep d@(Dependency n r) | _name p == n = Dependency n (addVersionToRange (_version p) r)
                                     | otherwise    = d

updateAllDependencies :: Packages -> Packages
updateAllDependencies ps = foldr updateDependencies ps ps

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

modifyPackage :: Package -> String -> String
modifyPackage p = flip (foldr modifyDependency) (_dependencies p) . modifyVersion (_version p)

savePackage :: Package -> IO ()
savePackage p = readFile (_path p) >>= writeFile (_path p) . modifyPackage p

{-
getBaseVersions :: String -> Packages -> IO Packages
getBaseVersions ind ps =
  do (_, hOut, _, _) <- runInteractiveCommand $ "tar -tf " ++ ind
     gps <- lines <$> hGetContents hOut
     let vs = map ((\(n:v:_) -> (n, parseVersion v)) . splitOn "/") gps
         globver = M.fromListWith (\a b -> if a > b then a else b) vs
         updVer p = modify version (maybe id (\v -> if _version p < v then const v else id) $ M.lookup (_name p) globver) p
     return $ map updVer ps
-}