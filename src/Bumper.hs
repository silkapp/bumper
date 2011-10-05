module Main where

import Data.Label
import Data.List
import Data.Maybe
import Package
import Config
import Version

main :: IO ()
main =
  do conf <- getConfig
     ps <- packages
     base <- maybe (return ps) (flip getBaseVersions ps) $ _global conf
     let new = (if _transitive conf then trans $ removeAll (_ignore conf ) base else id) $
               applyBumps addMinor (lookupPackages base (_bumpMinor conf))
           <+> applyBumps addMajor (lookupPackages base (_bumpMajor conf))
           <+> makeVersions base (_setVersion conf)
     if get showDeps conf
       then putStr $ intercalate " " $ map (get name) new
       else mapM_ (bumpAll base) new

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


makeVersions :: Packages -> [(String, [Int])] -> Packages
makeVersions ps = catMaybes . map (\(n,v) -> fmap (set version v) $ lookupPackage n ps)

applyBumps :: ([Int] -> [Int]) -> Packages -> Packages
applyBumps bump = map (modify version bump)


-- | Transitive bumping

trans :: Packages -> Packages -> Packages
trans ps = fix (transStep ps)

transStep :: Packages -> Packages -> Packages
transStep ps old = new <+> old
  where new = applyBumps addMinor . lookupPackages ps . concatMap _dependants $ old

fix :: (Eq a) => (a -> a) -> a -> a
fix f a | b == a    = a
        | otherwise = fix f b
  where b = f a
