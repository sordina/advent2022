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
    ]

help :: a
help = error $ "Usage: advent2021 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
