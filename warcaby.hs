module Main where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Control.Monad (when)

type Board = [[Char]]
type Position = (Int, Int)

initialBoard :: Board
initialBoard =
  [ "x.x.x.x."
  , ".x.x.x.x"
  , "x.x.x.x."
  , "........"
  , "........"
  , ".o.o.o.o"
  , "o.o.o.o."
  , ".o.o.o.o"
  ]

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "  0 1 2 3 4 5 6 7"
  mapM_ putStrLn [show r ++ " " ++ intercalate " " [ [board !! r !! c] | c <- [0..7]] | r <- [0..7]]

getPiece :: Board -> Position -> Char
getPiece board (r, c) = board !! r !! c

setPiece :: Board -> Position -> Char -> Board
setPiece board (r, c) piece =
  take r board ++
  [take c (board !! r) ++ [piece] ++ drop (c + 1) (board !! r)] ++
  drop (r + 1) board

isInside :: Position -> Bool
isInside (r, c) = r >= 0 && r < 8 && c >= 0 && c < 8

moveDirs :: Char -> [Position]
moveDirs p
  | p == 'o' = [(-1,-1), (-1,1)]
  | p == 'x' = [(1,-1), (1,1)]
  | toUpper p == 'O' = [(-1,-1), (-1,1), (1,-1), (1,1)]
  | toUpper p == 'X' = [(-1,-1), (-1,1), (1,-1), (1,1)]
  | otherwise = []

enemy :: Char -> Char -> Bool
enemy p target =
  let enemies = if toLower p == 'o' then "xX" else "oO"
  in target `elem` enemies

makeMove :: Board -> Position -> Position -> Maybe Board
makeMove board from@(fr, fc) to@(tr, tc)
  | not (isInside from && isInside to) = Nothing
  | piece == '.' || toPiece /= '.' = Nothing
  | abs (fr - tr) == 1 && abs (fc - tc) == 1 =
      Just $ promoteIfNeeded (setPiece (setPiece board from '.') to piece) to
  | abs (fr - tr) == 2 && abs (fc - tc) == 2 =
      let jumped = ((fr + tr) `div` 2, (fc + tc) `div` 2)
          jumpedPiece = getPiece board jumped
      in if enemy piece jumpedPiece
         then Just $ promoteIfNeeded (setPiece (setPiece (setPiece board from '.') jumped '.') to piece) to
         else Nothing
  | otherwise = Nothing
  where
    piece = getPiece board from
    toPiece = getPiece board to

promoteIfNeeded :: Board -> Position -> Board
promoteIfNeeded board (r, c)
  | piece == 'o' && r == 0 = setPiece board (r, c) 'O'
  | piece == 'x' && r == 7 = setPiece board (r, c) 'X'
  | otherwise = board
  where piece = getPiece board (r, c)

hasAnotherJump :: Board -> Position -> Bool
hasAnotherJump board pos@(r, c) =
  any canJump (moveDirs piece)
  where
    piece = getPiece board pos
    canJump (dr, dc) =
      let middle = (r + dr, c + dc)
          target = (r + 2*dr, c + 2*dc)
      in isInside middle && isInside target &&
         enemy piece (getPiece board middle) &&
         getPiece board target == '.'

gameLoop :: Board -> Bool -> IO ()
gameLoop board isWhiteTurn = do
  printBoard board
  putStrLn $ if isWhiteTurn then "Białe (o) ruszają" else "Czarne (x) ruszają"
  putStrLn "Podaj ruch w formacie: r1 c1 r2 c2"
  input <- getLine
  case map readMaybe (words input) of
    [Just r1, Just c1, Just r2, Just c2] -> do
      let from = (r1, c1)
          to = (r2, c2)
          piece = getPiece board from
      if (isWhiteTurn && toLower piece /= 'o') || (not isWhiteTurn && toLower piece /= 'x')
        then putStrLn "Nie twoja figura!" >> gameLoop board isWhiteTurn
        else case makeMove board from to of
          Just newBoard -> 
            if abs (r1 - r2) == 2 && hasAnotherJump newBoard to
              then gameLoop newBoard isWhiteTurn
              else gameLoop newBoard (not isWhiteTurn)
          Nothing -> putStrLn "Nieprawidłowy ruch!" >> gameLoop board isWhiteTurn
    _ -> putStrLn "Zły format!" >> gameLoop board isWhiteTurn

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

main :: IO ()
main = gameLoop initialBoard True
