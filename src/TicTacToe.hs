module TicTacToe
  ( someFunc
  ) where

import Board

board = move X 0 . move O 1 . move X 2 $ newBoard 3

someFunc :: IO ()
someFunc = print board
