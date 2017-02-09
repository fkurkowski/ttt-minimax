{-# LANGUAGE FlexibleInstances #-}
module Board
  ( Board
  , Player (..)
  , Position
  , newBoard
  , move
  , winner
  , gameOver
  ) where

import Data.List as List
import Data.Maybe as Maybe

-- Board size
size = 3

data Player = X | O deriving (Eq, Show)
type Position = Int
type Board = [Maybe Player]

instance {-# OVERLAPPING #-} Show Board where
  show = formatChunks . chunksOfBoardSize . posOrPlayer
    where
      posOrPlayer = zipWith showPosOrPlayer [0..]
      showPosOrPlayer i Nothing = show i
      showPosOrPlayer _ (Just p) = show p
      formatChunks = List.intercalate "\n" . map formatRow
      formatRow = List.intercalate " | "

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = init : chunksOf n rest
  where (init, rest) = splitAt n xs

chunksOfBoardSize :: [a] -> [[a]]
chunksOfBoardSize = chunksOf size

combos :: [a] -> [[a]]
combos board = rows ++ cols ++ diags
  where rows = chunksOfBoardSize board
        cols = transpose rows
        diags = [[a1,ab2,a3], [b1,ab2,b3]]
          where [[a1,_,b1],
                 [_,ab2,_],
                 [b3,_,a3]] = rows

newBoard :: Board
newBoard = replicate (size*size) Nothing

move :: Player -> Position -> Board -> Maybe Board
move p pos board
  | pos < 0 || pos >= length board = Nothing
  | isJust (board !! pos) = Nothing
  | otherwise =
      let (xs, ys) = splitAt pos board
       in Just $ xs ++ (pure p : tail ys)

winner :: Board -> Maybe Player
winner board = do
  let isWinningSet [a,b,c] = a == b && b == c && Maybe.isJust a
  set <- find isWinningSet . combos $ board
  head set

gameOver :: Board -> Bool
gameOver = (||) <$> isBoardFull <*> hasWinner
  where isBoardFull = all isJust
        hasWinner = isJust . winner
