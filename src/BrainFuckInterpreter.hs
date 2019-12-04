module BrainFuckInterpreter
  ( Tape(..)
  , executeBFOperation
  , runBrainFuck
  ) where

import BrainFuckP
import Data.Char (chr, ord)

data Tape =
  Tape
    { tape :: [Int]
    , index :: Int
    }
  deriving (Show)

newTape :: Tape
newTape = Tape [0,0 ..] 0

executeBFOperation :: Tape -> BrainFuckValue -> Tape
executeBFOperation (Tape inputTape index) BrainFuckIncrementPointer =
  Tape inputTape (index + 1)
executeBFOperation (Tape inputTape index) BrainFuckDecrementPointer
  | index == 0 =
    error "BrainFuckInterpreter.executeBFOperation: Decremented pointer below 0"
  | otherwise = Tape inputTape (index - 1)
executeBFOperation (Tape inputTape index) BrainFuckIncrement =
  Tape (replaceNth index newValue inputTape) index
  where
    newValue = (inputTape !! index) + 1
executeBFOperation (Tape inputTape index) BrainFuckDecrement =
  Tape (replaceNth index newValue inputTape) index
  where
    newValue = (inputTape !! index) - 1
executeBFOperation inputTape BrainFuckOutput = inputTape -- TODO
executeBFOperation inputTape BrainFuckAccept = inputTape -- TODO
executeBFOperation inputTape (BrainFuckWhile operations)
  | currByte inputTape == 0 = inputTape
  | otherwise =
    executeBFOperation
      (runBrainFuck inputTape operations)
      (BrainFuckWhile operations)
executeBFOperation inputTape BrainFuckComment = inputTape

currByte :: Tape -> Int
currByte inputTape = tape inputTape !! index inputTape

runBrainFuck :: Tape -> [BrainFuckValue] -> Tape
runBrainFuck = foldl executeBFOperation

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 newValue (x:xs) = newValue : xs
replaceNth idx newValue (x:xs) = x : replaceNth (idx - 1) newValue xs
