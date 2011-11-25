module Main where

import Data.Label
import Data.List
import qualified Data.Map as M
import Distribution.Package hiding (Package)
import Distribution.Text
import Distribution.Version
import Package
import Config
import Version

main :: IO ()
main =
  do conf <- getConfig
     -- Load packages
     ps   <- packages
     -- Retrieve base versions
     base <- maybe (return ps) (flip getBaseVersions ps) $ get global conf
     let changed = (if get transitive conf then trans base else id)
                 $ concatChanges (map (\(p,pks) -> bumpVersions p pks base) (M.toAscList (get bump conf)))
               <.> userVersions (get setVersion conf) base
         makeUpdates p = (M.lookup (get name p) changed, dependencyUpdates changed p)
     if get showDeps conf
       then putStr $ intercalate " " $ map display $ M.keys changed
       else mapM_ (\p -> updatePackage p (makeUpdates p)) base

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