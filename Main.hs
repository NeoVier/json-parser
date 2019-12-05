module Main
  ( main
  ) where

import BrainFuckInterpreter
import BrainFuckP
import JsonP
import Parser

main :: IO ()
main = do
  executeBrainFuck "tests/bubblesort.bf"
  return ()

executeBrainFuck :: FilePath -> IO ()
executeBrainFuck file = do
  operations <- parseFile file brainFuckValue
  case operations of
    Nothing -> putStrLn $ "Could not parse file: " ++ file
    Just ops -> do
      runBrainFuck newTape ops
      return ()
