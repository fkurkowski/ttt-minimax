module TicTacToe
  ( runGame
  ) where

import Board
import System.IO
import Data.Char (digitToInt)
import Data.Maybe (maybe)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

announce :: Maybe Player -> String
announce (Just p) = show p ++ " has won. Congratulations!"
announce _ = "It's a draw, folks."

runGame :: IO ()
runGame = do
  hSetBuffering stdout NoBuffering
  runGame' X newBoard

runGame' :: Player -> Board -> IO ()
runGame' p board = do
  putStrLn "\n"
  print board
  if gameOver board
     then putStrLn . announce . winner $ board
     else do
       putStr $ show p ++ "'s turn (0-8): "
       digit <- getChar

       let pos = digitToInt digit
           board' = move p pos board
           p' = nextPlayer p

       maybe
         (do putStrLn "\nNot valid."
             runGame' p board)
         (runGame' p')
         board'
