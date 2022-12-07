module Main where

import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

import qualified Advent01
import qualified Advent02
import qualified Advent03

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
    ]

help :: a
help = error $ "Usage: advent2021 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
