module TicTacToe
  ( someFunc
  ) where

import Board

board = move X 0 . move X 3 . move O 6 $ newBoard

someFunc :: IO ()
someFunc = print board
