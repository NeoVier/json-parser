module Main
  ( main
  ) where

import BrainFuckInterpreter
import BrainFuckP
import JsonP
import Parser
import System.Environment (getArgs)
import System.FilePath (takeExtension)

data Extension
  = BF FilePath
  | JSon FilePath

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  let extension = fileExtension file
  case extension of
    Nothing -> print $ parseError file
    Just ext -> executeExtension ext

fileExtension :: String -> Maybe Extension
fileExtension name =
  case takeExtension name of
    ".bf" -> Just $ BF name
    ".json" -> Just $ JSon name
    _ -> Nothing

executeExtension :: Extension -> IO ()
executeExtension (BF file) = executeBrainFuck file
executeExtension (JSon file) = executeJSon file

parseError :: FilePath -> String
parseError file = "Could not parse file: " ++ file

executeJSon :: FilePath -> IO ()
executeJSon file = do
  result <- parseFile file jsonValue
  case result of
    Nothing -> putStrLn $ parseError file
    Just contents -> print contents

executeBrainFuck :: FilePath -> IO ()
executeBrainFuck file = do
  operations <- parseFile file brainFuckValue
  case operations of
    Nothing -> putStrLn $ parseError file
    Just ops -> do
      runBrainFuck newTape ops
      return ()
