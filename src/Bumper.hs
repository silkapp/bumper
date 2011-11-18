module Main where

import Data.Label
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Distribution.Package hiding (Package)
import Distribution.Text
import Distribution.Version
import Package
import Config
import Version

main :: IO ()
main =
  do conf <- getConfig
     ps   <- packages
     let base = ps
--     base <- maybe (return ps) (flip getBaseVersions ps) $ _global conf
     let updated = updateAllDependencies -- Update dependencies
                 . addJobs ps            -- Add all packages which have not been changed
                 $ new
         new = (if _transitive conf then trans $ removeAll (_ignore conf ) base else id) -- Apply transitivity
             . concatJobs            -- Make changed and bumped versions
             $ map (\(p,pks) -> applyPosBumps p (lookupPackages base pks)) (M.toAscList (_bump conf))
               ++ [makeVersions base (_setVersion conf)]
     if get showDeps conf
       then putStr $ intercalate " " $ map (display . get name) new
       else mapM_ savePackage (diffPackages ps updated)

-- | Type holding the packages to be bumped
type BumpJob = Packages

-- | Only adds a job if it doesn't already exist
addJob :: Package -> Packages -> Packages
addJob p ps | hasPackage p ps = ps
            | otherwise       = p : ps

addJobs :: Packages -> Packages -> Packages
addJobs = flip (foldr addJob)

infixr 5 <+>
(<+>) :: Packages -> Packages -> Packages
(<+>) = addJobs

concatJobs :: [Packages] -> Packages
concatJobs = foldr (<+>) []


makeVersions :: Packages -> [(PackageName, Version)] -> Packages
makeVersions ps = catMaybes . map (\(n,v) -> fmap (set version v) $ lookupPackage n ps)

applyPosBumps :: Int -> Packages -> Packages
applyPosBumps pos = map (modify version (bumpPosition pos))


-- | Transitive bumping

trans :: Packages -> Packages -> Packages
trans ps = fix (transStep ps)

transStep :: Packages -> Packages -> Packages
transStep ps old = new <+> old
  where new = applyPosBumps 2 . lookupPackages ps . concatMap _dependants $ old

fix :: (Eq a) => (a -> a) -> a -> a
fix f a | b == a    = a
        | otherwise = fix f b
  where b = f a
