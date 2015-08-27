{-# LANGUAGE Rank2Types #-}

module Main where

import Data.MarkovChain
import System.Environment
import Safe
import System.Random
import System.IO

type F = RandomGen g => Int -> Int -> String -> g -> String

main :: IO ()
main = getArgs >>= args mChars

args :: F -> [String] -> IO ()
args _ ( "-h"      : _  ) = help
args _ ( "--help"  : _  ) = help
args _ ( "-w"      : xs ) = args mWords xs
args _ ( "--words" : xs ) = args mWords xs
args f [ out            ] = args  f [out, "10"]
args f [ out, con       ] = start f (readMay con) (readMay out)
args _ _                  = help

start :: F -> Maybe Int -> Maybe Int -> IO ()
start f (Just con) (Just out) = do
  g <- newStdGen
  i <- getContents
  putStrLn $ f out con i g

start _ _ _ = help

mChars :: F
mChars out con i g = take out $ run con i (con * 10) g

mWords :: F
mWords out con i g = unwords $ take out $ run con (words i) (con * 10) g

help :: IO ()
help = hPutStrLn stderr "Usage: <input> | markov [--help | -h] [--words | -w] <output-length> [context-length]"
