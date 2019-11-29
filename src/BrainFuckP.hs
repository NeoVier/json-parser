module BrainFuckP
  ( BrainFuckValue(..)
  , brainFuckValue
  , brainFuckComment
  ) where

import Control.Applicative
import Parser

data BrainFuckValue
  = BrainFuckIncrementPointer
  | BrainFuckDecrementPointer
  | BrainFuckIncrement
  | BrainFuckDecrement
  | BrainFuckOutput
  | BrainFuckAccept
  | BrainFuckWhile [BrainFuckValue]
  | BrainFuckComment
  deriving (Show, Eq)

brainFuckIncrementPointerP :: Parser BrainFuckValue
brainFuckIncrementPointerP = BrainFuckIncrementPointer <$ charP '>'

brainFuckDecrementPointerP :: Parser BrainFuckValue
brainFuckDecrementPointerP = BrainFuckDecrementPointer <$ charP '<'

brainFuckIncrementP :: Parser BrainFuckValue
brainFuckIncrementP = BrainFuckIncrement <$ charP '+'

brainFuckDecrementP :: Parser BrainFuckValue
brainFuckDecrementP = BrainFuckDecrement <$ charP '-'

brainFuckOutputP :: Parser BrainFuckValue
brainFuckOutputP = BrainFuckOutput <$ charP '.'

brainFuckAcceptP :: Parser BrainFuckValue
brainFuckAcceptP = BrainFuckAccept <$ charP ','

brainFuckWhileP :: Parser BrainFuckValue
brainFuckWhileP =
  BrainFuckWhile <$>
  (charP '[' *> brainFuckComment *> elements <* brainFuckComment <* charP ']')
  where
    elements = sepBy brainFuckComment brainFuckSymbol

brainFuckComment :: Parser BrainFuckValue
brainFuckComment =
  BrainFuckComment <$ (whitespace *> spanP notBrainFuckSymbol <* whitespace)
  where
    notBrainFuckSymbol x = x `notElem` "><+-.,[]"

brainFuckSymbol :: Parser BrainFuckValue
brainFuckSymbol =
  brainFuckIncrementPointerP <|> brainFuckDecrementPointerP <|>
  brainFuckIncrementP <|>
  brainFuckDecrementP <|>
  brainFuckOutputP <|>
  brainFuckAcceptP <|>
  brainFuckWhileP

brainFuckValue :: Parser [BrainFuckValue]
brainFuckValue = sepBy brainFuckComment brainFuckSymbol
