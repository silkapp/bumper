{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Version where

import Data.Label
import Distribution.Version

$(mkLabels [''Version])
$(mkLabels [''VersionRange])

-- | Function to bump the nth position in a version to a higher number
-- trailing version number will be discarded
bumpPosition :: Int -> Version -> Version
bumpPosition p = modify lVersionBranch (addPos p)
  where
    addPos 0 []      = [1]
    addPos 0 (v: _)  = [v+1]
    addPos x []      = 0 : addPos (x - 1) []
    addPos x (v: vs) = v : addPos (x - 1) vs

previousVersion :: Version -> Version
previousVersion = modify lVersionBranch mkPrevious
  where mkPrevious = reverse . prevHelp . reverse
        prevHelp []     = []
        prevHelp (x:xs) = if x <= 1 then xs else (x - 1): xs -- No trailing zeros

nextVersion :: Version -> Version
nextVersion = modify lVersionBranch mkNext
  where mkNext = reverse . nextHelp . reverse
        nextHelp []     = []
        nextHelp (x:xs) = (x + 1) : xs

addVersionToRange :: Version -> VersionRange -> VersionRange
addVersionToRange new r =
  if withinRange new r
  then r
  else
    let cVersion = const (thisVersion new)
        c2Version = const $ const (thisVersion new)
    in foldVersionRange'
        anyVersion      -- any
        cVersion        -- (==)
        cVersion        -- >
        cVersion        -- <
        cVersion        -- >=
        cVersion        -- <=
        (\v _ -> withinVersion $ v { versionBranch = take (length $ versionBranch v) $ versionBranch new })  -- .*
        c2Version       -- (||)
        c2Version       -- (&&)
        id              -- (_)
        r


{-
This is a more complex solution, which tries to add the version to the range while
keeping the old intact. This remains here becasue we may need this behavior in the future


-- | Make a range compatible with a version while keeping the orginal version as much intact as possible
addVersionToRange :: Version -> VersionRange -> VersionRange
addVersionToRange new =
  let memoV :: (Version -> VersionRange) -> (Version -> Version) -> Version -> (VersionRange, VersionRange)
      memoV f newF oldV = (f oldV, f . newF $ oldV)
      --Builds up a tuple of (oldValue, newValue), we need the old value fo the union and intersection parts
  in snd . foldVersionRange'
            (anyVersion, anyVersion)
            (memoV thisVersion (const new))
            (memoV laterVersion     $ \v -> if new > v then v else previousVersion new)
            (memoV earlierVersion   $ \v -> if new < v then v else nextVersion new)
            (memoV orLaterVersion   $ \v -> if new >= v then v else new)
            (memoV orEarlierVersion $ \v -> if new <= v then v else new)
            (\v _ -> ( withinVersion v
                     , withinVersion $
                        if versionBranch v `isPrefixOf` versionBranch new
                        then v
                        -- Place the wildcard at the same position as before or eralier if the new version is s
                        else v { versionBranch = take (length $ versionBranch v) $ versionBranch new }
                     )
            )
            (\(o1, _) (o2, _) -> if withinRange new o1 || withinRange new o2
                                   then (unionVersionRanges o1 o2, unionVersionRanges o1 o2)
                                   else (unionVersionRanges o1 o2, unionVersionRanges (thisVersion new) (unionVersionRanges o1 o2))
            )
            (\(o1, _) (o2, _) -> if withinRange new o1 && withinRange new o2
                                   then (intersectVersionRanges o1 o2, intersectVersionRanges o1 o2)
                                   else (intersectVersionRanges o1 o2, unionVersionRanges (thisVersion new) (intersectVersionRanges o1 o2))
            )
            id
-}