module Main where

import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

import qualified Advent01
import qualified Advent02
import qualified Advent03
import qualified Advent04
import qualified Advent05
import qualified Advent06
import qualified Advent07
import qualified Advent08
import qualified Advent09
import qualified Advent09b
import qualified Advent10
import qualified Advent11
import qualified Advent12
import qualified Advent13
import qualified Advent14
import qualified Advent15
import qualified Advent16
import qualified Advent16b

interactShow :: Show a => (String -> a) -> IO ()
interactShow f = interact ((++ "\n") . show . f)

days :: [(String, IO ())]
days =
    [ ("01",  interactShow Advent01.day1)
    , ("01b", interactShow Advent01.day1b)
    , ("02",  interactShow Advent02.day2)
    , ("02b", interactShow Advent02.day2b)
    , ("03",  interactShow Advent03.day3)
    , ("03b", interactShow Advent03.day3b)
    , ("04",  interactShow Advent04.day4)
    , ("04b", interactShow Advent04.day4b)
    , ("05",  interact ((++"\n") . Advent05.day5))
    , ("05b", interact ((++"\n") . Advent05.day5b))
    , ("06",  interactShow Advent06.day6)
    , ("06b", interactShow Advent06.day6b)
    , ("07",  interactShow Advent07.day7)
    , ("07b", interactShow Advent07.day7b)
    , ("08",  interactShow Advent08.day8)
    , ("08b", interactShow Advent08.day8b)
    , ("09",  interactShow Advent09.day9)
    , ("09b", interactShow Advent09b.day9b)
    , ("10",  interactShow Advent10.day10)
    , ("10b", interact ((++"\n") . Advent10.day10b ' '))
    , ("11",  interactShow Advent11.day11)
    , ("11b", interactShow Advent11.day11b)
    , ("12",  interactShow Advent12.day12)
    , ("12b", interactShow Advent12.day12b)
    , ("13",  interactShow Advent13.day13)
    , ("13b", interactShow Advent13.day13b)
    , ("14",  interactShow Advent14.day14)
    , ("14b", interactShow Advent14.day14b)
    , ("15",  interactShow Advent15.day15)
    , ("15b", interactShow Advent15.day15b)
    , ("16",  interactShow Advent16.day16)
    , ("16b", interactShow Advent16.day16b)
    , ("16b2", interactShow Advent16b.day16b2)
    ]

help :: a
help = error $ "Usage: advent2022 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
