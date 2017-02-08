{-# LANGUAGE FlexibleInstances #-}
module Board where

import Data.List as List

-- Board size
size = 3

data Player = X | O deriving (Eq, Show)
type Position = Int
type Board = [Maybe Player]

instance {-# OVERLAPPING #-} Show (Maybe Player) where
  show Nothing = " "
  show (Just p) = show p

instance {-# OVERLAPPING #-} Show Board where
  show = joinRows . chunksOfBoardSize
    where joinRows = List.intercalate "\n" . map singleRow
          singleRow = List.intercalate " | " . map show

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = init : chunksOf n rest
  where (init, rest) = splitAt n xs

chunksOfBoardSize :: [a] -> [[a]]
chunksOfBoardSize = chunksOf size

newBoard :: Board
newBoard = replicate (size*size) Nothing

move :: Player -> Position -> Board -> Board
move p pos board = xs ++ (pure p : tail ys)
  where (xs, ys) = splitAt pos board
