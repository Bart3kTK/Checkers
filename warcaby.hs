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
  any canJump directions
  where
    piece = getPiece board pos
    directions = [(-1,-1), (-1,1), (1,-1), (1,1)]  -- wszystkie kierunki
    canJump (dr, dc) =
      let middle = (r + dr, c + dc)
          target = (r + 2*dr, c + 2*dc)
      in isInside middle && isInside target &&
         enemy piece (getPiece board middle) &&
         getPiece board target == '.'
         
makeMovesSequence :: Board -> [Position] -> Maybe Board
makeMovesSequence board positions = 
  case positions of
    (start:_) ->
      applyMoves board positions
    _ -> Nothing
  where
    applyMoves :: Board -> [Position] -> Maybe Board
    applyMoves b [from, to] =
      let dr = abs (fst from - fst to)
          dc = abs (snd from - snd to)
      in case (dr, dc) of
           (1, 1) -> 
             makeMove b from to
           (2, 2) ->
             case makeMove b from to of
               Just nb ->
                 if hasAnotherJump nb to
                   then Nothing
                   else Just nb
               Nothing -> Nothing
           _ -> Nothing

    applyMoves b (from:to:next:rest) =
      let dr = abs (fst from - fst to)
          dc = abs (snd from - snd to)
      in if (dr, dc) == (2, 2)
           then case makeMove b from to of
                  Just nb ->
                    if hasAnotherJump nb to
                      then applyMoves nb (to:next:rest)
                      else Nothing
                  Nothing -> Nothing
           else Nothing
    applyMoves _ _ = Nothing

-- TODO nie możemy się cofaać dla x/o
gameLoop :: Board -> Bool -> IO ()
gameLoop board isWhiteTurn = do
  printBoard board
  putStrLn $ if isWhiteTurn then "Białe (o) ruszają" else "Czarne (x) ruszają"
  putStrLn "Podaj ruch w formacie: r1 c1 r2 c2 ..."
  input <- getLine
  let maybeInts = map readMaybe (words input) :: [Maybe Int]
  if any (== Nothing) maybeInts
    then putStrLn "Zły format!" >> gameLoop board isWhiteTurn
    else
      let ints = map (\(Just x) -> x) maybeInts
      in if length ints < 4 || odd (length ints)
           then putStrLn "Zły format!" >> gameLoop board isWhiteTurn
           else
             let coords = pairUp ints
                 start@(r1, c1) = head coords
                 piece = getPiece board start
                 isCorrectPiece = (isWhiteTurn && toLower piece == 'o') || (not isWhiteTurn && toLower piece == 'x')
             in if not isCorrectPiece
                  then putStrLn "Nie twoja figura!" >> gameLoop board isWhiteTurn
                  else case makeMovesSequence board coords of
                         Just newBoard -> gameLoop newBoard (not isWhiteTurn)
                         Nothing       -> putStrLn "Nieprawidłowy ruch!" >> gameLoop board isWhiteTurn

pairUp :: [Int] -> [Position]
pairUp []         = []
pairUp (r:c:rest) = (r, c) : pairUp rest
pairUp _          = []

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

main :: IO ()
main = gameLoop initialBoard True
