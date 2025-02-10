{-# LANGUAGE TupleSections #-}
module Lib
    ( playGame
    , emptyBoard
    , Player(..)
    ) where

import qualified Data.Map.Strict as Map
--import System.Random 
import Data.Maybe (isJust)
import Data.List (maximumBy)
import Data.List (minimumBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (group)
import Data.List (sortBy)
import Data.Ord (comparing)



data Player = Yellow | Red deriving (Eq, Show)
data Cell = Empty | Occupied Player deriving (Eq, Show)
type Board = Map.Map (Int, Int) Cell
type Move = Int


emptyBoard :: Board
emptyBoard = Map.fromList [((row, col), Empty) | row <- [0..5], col <- [0..6]]


displayBoard :: Board -> String
displayBoard board = unlines [concatMap (displayCell . (row,)) [0..6] ++ "\n" | row <- [0..5]]
  where
    displayCell pos = case Map.lookup pos board of
      Just Empty -> "|   "
      Just (Occupied Yellow) -> "| Y "
      Just (Occupied Red) -> "| R "
      Nothing -> "|   "


makeMove :: Board -> Player -> Move -> Maybe Board
makeMove board player column
  | column < 0 || column >= cols || not (isValidMove board column) = Nothing
  | otherwise = Just (dropDisc board column player)
  where
    cols = 7

isValidMove :: Board -> Move -> Bool
isValidMove board column = column >= 0 && column < cols && Map.lookup (0, column) board == Just Empty
  where
    cols = 7


dropDisc :: Board -> Move -> Player -> Board
dropDisc board column player = updateBoard board (emptyRow, column) player
  where
    emptyRow = findEmptyRow board column


findEmptyRow :: Board -> Move -> Int
findEmptyRow board column = case dropWhile (\row -> Map.lookup (row, column) board /= Just Empty) [5,4,3,2,1,0] of
  [] -> -1  -- Column is full
  (row:_) -> row


countOccupiedCells :: Board -> Int
countOccupiedCells = Map.size . Map.filter (\cell -> cell /= Empty)


updateBoard :: Board -> (Int, Int) -> Player -> Board
updateBoard board (row, col) player = Map.update (\_ -> Just (Occupied player)) (row, col) board


checkWinner :: Board -> Maybe Player
checkWinner board =
  let rows = 6
      cols = 7

      horizontalWin = any (hasConsecutive 4) [Map.elems (Map.filterWithKey (\(_, c) _ -> c == col) board) | col <- [0..cols-1]]
      verticalWin = any (hasConsecutive 4) [Map.elems (Map.filterWithKey (\(r, _) _ -> r == row) board) | row <- [0..rows-1]]
      diagonalWin1 = any (hasConsecutive 4) (getDiagonals board)
      diagonalWin2 = any (hasConsecutive 4) (getDiagonals (Map.mapKeys (\(r, c) -> (r, 6 - c)) board))

  in if horizontalWin || verticalWin || diagonalWin1 || diagonalWin2
       then Just Yellow
       else Nothing

hasConsecutive :: Int -> [Cell] -> Bool
hasConsecutive n cells = any (\group -> length group >= n &&
                              (all (== Occupied Yellow) group ||
                              (all (== Occupied Red) group ))) (group cells)


getDiagonals :: Board -> [[Cell]]
getDiagonals board = map (\diagonalIndices -> map (\(row, col) -> Map.findWithDefault Empty (row, col) board) diagonalIndices) allDiagonalGroups
  where
    numRows = 6
    numCols = 7
    allDiagonalGroups = diagonalsFromTopLeft ++ diagonalsFromTopRight

    diagonalsFromTopLeft = [[(row + k, col + k) | k <- [0 .. min (numRows - row - 1) (numCols - col - 1)]] | row <- [0 .. numRows - 1], col <- [0 .. numCols - 1]]
    diagonalsFromTopRight = [[(row + k, col - k) | k <- [0 .. min (numRows - row - 1) col]] | row <- [0 .. numRows - 1], col <- [0 .. numCols - 1]]


isFull :: Board -> Bool
isFull board = all (/= Empty) (Map.elems board)

humanMove :: IO Move
humanMove = do
  putStrLn "Enter your move (column number):"
  input <- getLine
  case reads input of
    [(move, _)] -> return move
    _ -> do
      putStrLn "Invalid input. Please enter a valid move."
      humanMove

playGame :: Board -> Player -> IO ()
playGame board player = do
  putStrLn "Current Board:"
  putStrLn (displayBoard board)

  if isFull board
  then putStrLn "It's a draw!"
  else do
    move <- if player == Yellow then humanMove else minimaxMove board player 
    case makeMove board player move of
      Just newBoard -> do
--        putStrLn "Current Board:"
        putStrLn ("Move: " ++ show move)
        putStrLn (displayBoard newBoard)
        case checkWinner newBoard of
          Just winner -> putStrLn (show player ++ " wins!")
          Nothing -> playGame newBoard (nextPlayer player)
      Nothing -> do
        putStrLn "Invalid move. Please try again."
        putStrLn ("Invalid Move: " ++ show move)
        playGame board player

nextPlayer :: Player -> Player
nextPlayer Yellow = Red
nextPlayer Red = Yellow


minimaxMove :: Board -> Player -> IO Move
minimaxMove board player = do
  let possibleMoves = filter (\m -> isValidMove board m) [0..6]
  let bestMove = minimax board player (depthLimit board) True
  return bestMove



minimax :: Board -> Player -> Int -> Bool -> Move
minimax board player depth maximizingPlayer
  | depth == 0 || isFull board || isJust (checkWinner board) = evaluate board player
  | maximizingPlayer = maximumBy (\m1 m2 -> compare (minimax (makeMove' board player m1) (nextPlayer player) (depth - 1) False) (minimax (makeMove' board player m2) (nextPlayer player) (depth - 1) False)) possibleMoves
  | otherwise = minimumBy (\m1 m2 -> compare (minimax (makeMove' board player m1) (nextPlayer player) (depth - 1) True) (minimax (makeMove' board player m2) (nextPlayer player) (depth - 1) True)) possibleMoves
  where
    possibleMoves = filter (\m -> isValidMove board m) [0..6]
    makeMove' :: Board -> Player -> Move -> Board
    makeMove' b p m = fromMaybe b (makeMove b p m)


evaluate :: Board -> Player -> Move
evaluate board player =
  let winningPlayer = checkWinner board
      opponent = nextPlayer player

      immediateWin = case winningPlayer of
        Just Yellow | player == Red -> -20
        Just Red | player == Yellow -> -20
        _ -> 0

      immediateThreat = case winningPlayer of
        Just Yellow | player == Yellow -> 10
        Just Red | player == Red -> 10
        _ -> 0

  in immediateWin + immediateThreat

depthLimit :: Board -> Int
depthLimit board
  | countOccupiedCells board < 12 = 4
  | otherwise = 2



