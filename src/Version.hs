-- * Manipulation functions for cabal versions and version ranges
{-# LANGUAGE
    TypeOperators
  , TemplateHaskell
  #-}
module Version where

import Data.Label
import Distribution.Text
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

printRange :: VersionRange -> String
printRange =
  let sShow s = ((s ++ " ") ++) . display
  in foldVersionRange'
      "*"
      (sShow "==")
      (sShow ">")
      (sShow "<")
      (sShow ">=")
      (sShow "<=")
      (\v _   -> "== " ++ display v ++ ".*")
      (\v1 v2 -> v1 ++ " || " ++ v2)
      (\v1 v2 -> v1 ++ " && " ++ v2)
      (\v -> "(" ++ v ++ ")")
