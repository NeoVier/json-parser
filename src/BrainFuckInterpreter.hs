module BrainFuckInterpreter
  ( Tape(..)
  , executeBFOperation
  , runBrainFuck
  , newTape
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
executeBFOperation inputTape BrainFuckComment = inputTape

currByte :: Tape -> Int
currByte inputTape = tape inputTape !! index inputTape

runBrainFuck :: Tape -> [BrainFuckValue] -> IO Tape
runBrainFuck inputTape [] = return inputTape
runBrainFuck inputTape (BrainFuckOutput:ops) = do
  let char = chr $ currByte inputTape
  putChar char
  runBrainFuck inputTape ops
runBrainFuck inputTape (BrainFuckAccept:ops) = do
  inputChar <- getChar
  let charOrd = ord inputChar
  let newTape =
        Tape
          (replaceNth (index inputTape) charOrd (tape inputTape))
          (index inputTape)
  runBrainFuck newTape ops
runBrainFuck inputTape (BrainFuckWhile operations:ops)
  | currByte inputTape == 0 = runBrainFuck inputTape ops
  | otherwise = do
    resultingTape <- runBrainFuck inputTape operations
    runBrainFuck resultingTape (BrainFuckWhile operations : ops)
runBrainFuck inputTape (op:ops) = do
  let resultingTape = executeBFOperation inputTape op
  runBrainFuck resultingTape ops

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 newValue (x:xs) = newValue : xs
replaceNth idx newValue (x:xs) = x : replaceNth (idx - 1) newValue xs
