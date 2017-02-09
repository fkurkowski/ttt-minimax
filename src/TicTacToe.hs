module TicTacToe
  ( runGame
  ) where

import Board
import Control.Monad (forever)
import Data.Char (digitToInt)
import Data.Maybe (maybe)
import System.IO

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

announce :: Maybe Player -> String
announce (Just p) = show p ++ " has won. Congratulations!"
announce _ = "It's a draw, folks."

runGame :: IO ()
runGame = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "\n\nStarting a new game..."
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
