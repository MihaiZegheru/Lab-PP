{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

import Data.Matrix
import Data.List

-- Vec2
type Vec2 = (Int, Int)
-- end Vec2

-- Cell
data Cell = X | O | EmptyCell deriving Eq

instance Show Cell where
  show X = "X"
  show O = "O"
  show EmptyCell = " "
-- end Cell

-- Player
data Player = Player { symbol :: Cell }

cyclePlayer :: Player -> Player
cyclePlayer player
  | symbol player == X = Player O
  | symbol player == O = Player X
-- end Player

-- Board
data Board = Board (Matrix Cell) | EmptyBoard

instance Show Board where
  show EmptyBoard = ""
  show (Board mat) = show mat 

board :: Vec2 -> Board
board pos@(x, y) = Board (fromList x y (replicate (x * y) EmptyCell))

boardSize :: Board -> Vec2
boardSize (Board mat) = (ncols mat, nrows mat)

setPiece :: Board -> Vec2 -> Player -> Board
setPiece EmptyBoard _ _ = EmptyBoard
setPiece brd@(Board mat) pos@(x, y) (Player sym)
  | inBounds brd pos = Board (setElem sym pos mat)
  | otherwise = brd

-- TODO: Delete inBounds.
--       Refactor and use safeGet by treating Maybe Cell return value

inBounds :: Board -> Vec2 -> Bool
inBounds (Board mat) pos@(x, y)
  | x >= 0 && y >= 0 && x < ncols mat && y < nrows mat = True
  | otherwise = False
-- end Board

-- TicTacToe
data TicTacToe = Game
  { gameBoard :: Board
  , activePlayer :: Player
  }

instance Show TicTacToe where
  show game = text ++  "\n" ++ show (gameBoard game)
    where
      text = "TicTacToe 5X"

startGame :: Board -> TicTacToe
startGame brd = Game brd (Player X)

makeMove :: TicTacToe -> Vec2 -> TicTacToe
makeMove game pos = Game (setPiece (gameBoard game) pos (activePlayer game)) (cyclePlayer (activePlayer game))
-- end TicTacToe
