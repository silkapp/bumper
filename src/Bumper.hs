module Main where

import Config
import Control.Monad
import Data.Label
import Data.List
import Data.Maybe
import Data.Version
import Distribution.Package hiding (Package)
import Distribution.Text
import Distribution.Version
import Package
import Version
import qualified Data.Map as M
import qualified Paths_bumper as Paths

main :: IO ()
main =
  do conf <- getConfig
     case get action conf of
       ShowHelp    -> printUsage options
       ShowVersion -> putStrLn $ "bumper, version " ++ showVersion Paths.version
       ShowDeps    -> run conf showDeps
       Run         -> run conf updateDeps
  where
    showDeps   _    changed = putStr $ intercalate " " $ map display $ M.keys changed
    updateDeps base changed = mapM_ (\p -> updatePackage p (makeUpdates p)) base
      where
        makeUpdates p = (M.lookup (get name p) changed, dependencyUpdates changed p)

run :: Config -> (Packages -> Changes -> IO ()) -> IO ()
run conf act =
  do -- Load packages
     ps   <- packages

     --Check for non-existent packages
     let changePks = map fst (get setVersion conf) ++ concat (M.elems (get bump conf))
         notFound = filter (not . isJust . flip lookupPackage ps) changePks
     when (not $ null notFound) $ putStrLn $ "[Warning] packages not found: " ++ (intercalate "," $ map display notFound)

     -- Retrieve base versions
     base <- maybe (return ps) (flip getBaseVersions ps) $ get global conf
     let changed = (if get transitive conf then trans base else id)
                 $ concatChanges (map (\(p,pks) -> bumpVersions p pks base) (M.toAscList (get bump conf)))
               <.> userVersions (get setVersion conf) base
     act base changed

type Changes = M.Map PackageName Version

-- Combine changes, not updating already changed packages
infixr 5 <.>
(<.>) :: Changes -> Changes -> Changes
(<.>) = M.unionWith (flip const)

concatChanges :: [Changes] -> Changes
concatChanges = foldr (<.>) M.empty

-- | Update versions
userVersions :: [(PackageName, Version)] -> Packages -> Changes
userVersions vs ps = M.fromList $ filter (\nv -> hasPackage (fst nv) ps) vs

bumpVersions :: Int -> [PackageName] -> Packages -> Changes
bumpVersions pos ns ps = M.fromList $ map (\p -> (get name p, bumpPosition pos (get version p))) $ lookupPackages ns ps

-- | Make transitive changes
trans :: Packages -> Changes -> Changes
trans ps = fix (transStep ps)

transStep :: Packages -> Changes -> Changes
transStep ps old = new <.> old
  where deps = filter (not . null . dependencyUpdates old) ps
        new  = M.fromList . map (\p -> (get name p, bumpPosition 2 (get version p))) $ deps

fix :: (Eq a) => (a -> a) -> a -> a
fix f a | b == a    = a
        | otherwise = fix f b
  where b = f a

-- | Caclulate updated dependencies
dependencyUpdates :: Changes -> Package -> [Dependency]
dependencyUpdates ch = foldr addDep [] . get dependencies
  where addDep (Dependency n r) dps =
          case M.lookup n ch of
            Just v  -> if withinRange v r
                        then dps
                        else Dependency n (addVersionToRange v r) : dps
            Nothing -> dps
