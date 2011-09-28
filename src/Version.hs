module Version where

import Data.List
import Data.List.Split

type Version = [Int]

parseVersion :: String -> [Int]
parseVersion = map read . splitOn "."

showVersion :: [Int] -> String
showVersion = intercalate "." . map show

addMinor :: [Int] -> [Int]
addMinor (m1:m2:m3:r)= m1 : m2 : (m3+1) : r
addMinor [m1,m2]     = [m1, m2, 1]
addMinor [m]         = [m, 0, 1]
addMinor []          = [0, 0, 1]

addMajor :: [Int] -> [Int]
addMajor (m1:m2:r) = m1 : (m2+1) : r
addMajor [m]       = [m, 1]
addMajor []        = [0, 1]
