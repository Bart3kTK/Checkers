{-# LANGUAGE TupleSections #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
-- import System.Random (randomRIO)


type Board = [[Maybe Char]]
type Position = (Int, Int)

windowSize, cellSize :: Int
windowSize = 800
cellSize = windowSize `div` 8

data GameState = GameState
  { board :: Board
  , selected :: [Position]
  , isWhiteTurn :: Bool
  , statusMessage :: String
  }

cellSizeF :: Float
cellSizeF = 100

windowSizeF :: Int
windowSizeF = 800

initialBoard :: Board
initialBoard =
  map (map toMaybe)
  [ "........"
  , "........"
  , "..x.x.x."
  , "........"
  , "..x.x.x."
  , "........"
  , "..x.x.x."
  , ".o...o.o"
  ]
  where
    toMaybe '.' = Nothing
    toMaybe x   = Just x


-- initialBoard = -- end game debug
--   , "........"
--   , "........"
--   , "........"
--   , "........"
--   , "......x."
--   , ".o.o.o.o"
--   , "o.o.o.o."
--   , ".o.o.o.o"
--   ]

-- initialBoard = --debug (taki ruch jest możliwy: 7 7 5 5 7 3 5 1 3 3 5 5 3 7 1 5 3 3 1 1)
--   [ "........"
--   , "........"
--   , "..x.x.x."
--   , "........"
--   , "..x.x.x."
--   , "........"
--   , "..x.x.x."
--   , ".o...o.o"
--   ]

-- initialBoard = --promocja
--   [ "........"
--   , ".......o"
--   , "........"
--   , "........"
--   , ".x......"
--   , "........"
--   , "........"
--   , ".o...o.o"
--   ]

-- initialBoard = [ --obliczanie heurystyki
--     "........"
--   , "........"
--   , ".....x.."
--   , "........"
--   , "...x...."
--   , "..o....."
--   , ".....O.."
--   , "........"
--   ]
countPossibleJumps :: Board -> Char -> Int
countPossibleJumps board player =
  length [ (r, c)
         | r <- [0..7], c <- [0..7]
         , let mp = getPiece board (r, c)
         , Just p <- [mp], toLower p == player
         , any (canJumpFrom (r, c) p) [(-1,-1), (-1,1), (1,-1), (1,1)]
         ]
  where
    canJumpFrom (r, c) piece (dr, dc) =
      let middle = (r + dr, c + dc)
          target = (r + 2*dr, c + 2*dc)
      in isInside middle && isInside target &&
         maybe False (enemy piece) (getPiece board middle) &&
         getPiece board target == Nothing

scorePiece :: Char -> Int
scorePiece p
        | p == 'o' = 1
        | p == 'O' = 3
        | p == 'x' = -1
        | p == 'X' = -3
        | otherwise = 0

evaluateBoard :: Board -> Int
evaluateBoard board =
  let allPieces = concat board
      jumpBonus = 2 * countPossibleJumps board 'o' - 2 * countPossibleJumps board 'x'
  in sum (map (maybe 0 scorePiece) allPieces) + jumpBonus

countPieces :: Board -> (Int, Int)
countPieces board =
  let allPieces = concat board
      countX = length (filter (\p -> p == Just 'x' || p == Just 'X') allPieces)
      countO = length (filter (\p -> p == Just 'o' || p == Just 'O') allPieces)
  in (countX, countO)


-- Zwraca wszystkie możliwe sekwencje bić dla danego pionka
allJumpSequences :: Board -> Position -> [[Position]]
allJumpSequences board start =
  let piece = getPiece board start
  in case piece of
      Nothing -> []
      Just p  -> go board [start]
  where
    go b path@(current:_) =
      let jumps = [ target
                  | (dr, dc) <- moveDirs (unwrap (getPiece b current))
                  , let middle = (fst current + dr, snd current + dc)
                  , let target = (fst current + 2*dr, snd current + 2*dc)
                  , isInside middle, isInside target
                  , maybe False (enemy (unwrap (getPiece b current))) (getPiece b middle)
                  , getPiece b target == Nothing
                  ]
      in if null jumps
         then [reverse path]
         else concat [ go (fromJust (makeMovesSequence b [current, target])) (target:path)
                    | target <- jumps
                    , let mb = makeMovesSequence b [current, target]
                    , mb /= Nothing
                    ]
    unwrap (Just x) = x
    unwrap Nothing  = error "Unexpected Nothing"
    fromJust (Just x) = x
    fromJust Nothing  = error "Unexpected Nothing"

-- Zwraca wszystkie możliwe ruchy (tylko bicie jeśli jest możliwe, w przeciwnym razie zwykłe ruchy)
allMoves :: Board -> Bool -> [(Position, Position, Board)]
allMoves board isWhite =
  if not (null allJumps)
    then [ (head seq, last seq, fromJust (makeMovesSequence board seq))
         | seq <- maxJumpSeqs, length seq > 1 ]
    else [ (from, to, newBoard)
         | r <- [0..7], c <- [0..7]
         , let from = (r, c)
         , Just p <- [getPiece board from]
         , (isWhite && toLower p == 'o') || (not isWhite && toLower p == 'x')
         , (dr, dc) <- moveDirs p
         , let to = (r + dr, c + dc)
         , isInside to
         , Just newBoard <- [makeMove board from to]
         ]
  where
    positions = [ (r, c)
                | r <- [0..7], c <- [0..7]
                , Just p <- [getPiece board (r, c)]
                , (isWhite && toLower p == 'o') || (not isWhite && toLower p == 'x')
                ]
    allJumps = concatMap (allJumpSequences board) positions
    maxLen = maximum (0 : map length allJumps)
    maxJumpSeqs = filter (\seq -> length seq == maxLen && length seq > 1) allJumps
    fromJust (Just x) = x
    fromJust Nothing  = error "Unexpected Nothing"

minimax :: Board -> Int -> Bool -> Int -> Int -> Int
minimax board depth isWhite alpha beta
  | depth == 0 = evaluateBoard board
  | null moves = evaluateBoard board
  | isWhite = maxValue moves alpha beta
  | otherwise = minValue moves alpha beta
  where
    moves = [b | (_, _, b) <- allMoves board isWhite]

    maxValue [] a _ = a
    maxValue (m:ms) a b =
      let v = max a (minimax m (depth-1) False a b)
      in if v >= b then v else maxValue ms v b

    minValue [] _ b = b
    minValue (m:ms) a b =
      let v = min b (minimax m (depth-1) True a b)
      in if v <= a then v else minValue ms a v


bestMove :: Board -> Int -> Bool -> Maybe Board
bestMove board depth isWhite =
  let moves = allMoves board isWhite
      scored = [ (minimax b (depth-1) (not isWhite) (-10000) 10000, b) | (_, _, b) <- moves ]
  in if null scored then Nothing else Just (snd (maximum scored))

-- Zwraca najlepszy ruch (planszę) jeśli jest, a jeśli nie ma - losowy lub pierwszy możliwy ruch
chooseBestOrAnyMove :: Board -> Int -> Bool -> Maybe Board
chooseBestOrAnyMove board depth isWhite =
  let moves = allMoves board isWhite
      scored = [ (minimax b (depth-1) (not isWhite) (-10000) 10000, b) | (_, _, b) <- moves ]
  in if not (null scored)
       then Just (snd (maximum scored))
       else if not (null moves)
         then Just (let (_, _, b) = head moves in b)
         else Nothing


printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "  0 1 2 3 4 5 6 7"
  mapM_ putStrLn [show r ++ " " ++ intercalate " " [maybe "." (:[]) (board !! r !! c) | c <- [0..7]] | r <- [0..7]]

getPiece :: Board -> Position -> Maybe Char
getPiece board (r, c) = board !! r !! c

setPiece :: Board -> Position -> Maybe Char -> Board
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
  | piece == Nothing || toPiece /= Nothing = Nothing
  | isRegularPawn && movingBackward && not isJump = Nothing
  | abs (fr - tr) == 1 && abs (fc - tc) == 1 =
      Just $ promoteIfNeeded (setPiece (setPiece board from Nothing) to piece) to
  | abs (fr - tr) == 2 && abs (fc - tc) == 2 =
      let jumped = ((fr + tr) `div` 2, (fc + tc) `div` 2)
          jumpedPiece = getPiece board jumped
      in if maybe False (enemy (unwrap piece)) jumpedPiece
         then Just $ promoteIfNeeded (setPiece (setPiece (setPiece board from Nothing) jumped Nothing) to piece) to
         else Nothing
  | otherwise = Nothing
  where
    isJump = abs (fr - tr) == 2 && abs (fc - tc) == 2
    piece = getPiece board from
    toPiece = getPiece board to
    isRegularPawn = maybe False (`elem` "xo") piece
    movingBackward = case piece of
      Just 'x' -> tr < fr
      Just 'o' -> tr > fr
      _        -> False
    unwrap (Just x) = x
    unwrap Nothing  = error "Unexpected Nothing"

promoteIfNeeded :: Board -> Position -> Board
promoteIfNeeded board (r, c) =
  case getPiece board (r, c) of
    Just 'o' | r == 0 -> setPiece board (r, c) (Just 'O')
    Just 'x' | r == 7 -> setPiece board (r, c) (Just 'X')
    _                 -> board

hasAnotherJump :: Board -> Position -> Bool
hasAnotherJump board pos@(r, c) =
  case getPiece board pos of
    Nothing -> False
    Just piece -> any canJump [(-1,-1), (-1,1), (1,-1), (1,1)]
      where
        canJump (dr, dc) =
          let middle = (r + dr, c + dc)
              target = (r + 2*dr, c + 2*dc)
          in isInside middle && isInside target &&
             maybe False (enemy piece) (getPiece board middle) &&
             getPiece board target == Nothing

makeMovesSequence :: Board -> [Position] -> Maybe Board
makeMovesSequence board positions = 
  case positions of
    (start:_) -> applyMoves board positions
    _ -> Nothing
  where
    applyMoves b [from, to] =
      let dr = abs (fst from - fst to)
          dc = abs (snd from - snd to)
      in case (dr, dc) of
           (1, 1) -> makeMove b from to
           (2, 2) -> case makeMove b from to of
                       Just nb -> if hasAnotherJump nb to then Nothing else Just nb
                       Nothing -> Nothing
           _ -> Nothing

    applyMoves b (from:to:next:rest) =
      let dr = abs (fst from - fst to)
          dc = abs (snd from - snd to)
      in if (dr, dc) == (2, 2)
           then case makeMove b from to of
                  Just nb -> if hasAnotherJump nb to
                               then applyMoves nb (to:next:rest)
                               else Nothing
                  Nothing -> Nothing
           else Nothing
    applyMoves _ _ = Nothing

-- gameLoop :: Board -> Bool -> IO ()
-- gameLoop board isWhiteTurn = do
--   printBoard board
--   let (countX, countO) = countPieces board
--   if countX == 0 then putStrLn "Białe (o) wygrały!!"
--   else if countO == 0 then putStrLn "Czarne (x) wygrały!"
--   else do
--     putStrLn $ if isWhiteTurn then "Białe (o) ruszają" else "Czarne (x) ruszają"
--     putStrLn "Podaj ruch w formacie: r1 c1 r2 c2 ..."
--     input <- getLine
--     let maybeInts = map readMaybe (words input) :: [Maybe Int]
--     if any (== Nothing) maybeInts
--       then putStrLn "Zły format!" >> gameLoop board isWhiteTurn
--       else
--         let ints = map (\(Just x) -> x) maybeInts
--         in if length ints < 4 || odd (length ints)
--             then putStrLn "Zły format!" >> gameLoop board isWhiteTurn
--             else
--               let coords = pairUp ints
--                   start@(r1, c1) = head coords
--                   piece = getPiece board start
--                   isCorrectPiece = case piece of
--                     Just p -> (isWhiteTurn && toLower p == 'o') || (not isWhiteTurn && toLower p == 'x')
--                     _      -> False
--               in if not isCorrectPiece
--                     then putStrLn "Nie twoja figura!" >> gameLoop board isWhiteTurn
--                     else case makeMovesSequence board coords of
--                           Just newBoard -> gameLoop newBoard (not isWhiteTurn)
--                           Nothing       -> putStrLn "Nieprawidłowy ruch!" >> gameLoop board isWhiteTurn

-- pairUp :: [Int] -> [Position]
-- pairUp []         = []
-- pairUp (r:c:rest) = (r, c) : pairUp rest
-- pairUp _          = []

-- readMaybe :: Read a => String -> Maybe a
-- readMaybe s = case reads s of
--   [(val, "")] -> Just val
--   _           -> Nothing


renderCell :: Position -> Maybe Char -> Picture
renderCell (r, c) mPiece =
  let x = fromIntegral c * cellSizeF - 4 * cellSizeF + cellSizeF / 2
      y = 4 * cellSizeF - fromIntegral r * cellSizeF - cellSizeF / 2
      baseColor = if even (r + c) then greyN 0.8 else greyN 0.3
      base = Color baseColor $ rectangleSolid cellSizeF cellSizeF
      piece = case mPiece of
        Just 'o' -> translate 0 0 $ Color white $ thickCircle (cellSizeF/4) 10
        Just 'x' -> translate 0 0 $ Color black $ thickCircle (cellSizeF/4) 10
        Just 'O' -> translate 0 0 $ Color white $ circleSolid (cellSizeF/4)
        Just 'X' -> translate 0 0 $ Color black $ circleSolid (cellSizeF/4)
        _        -> Blank
  in translate x y $ Pictures [base, piece]

renderBoard :: Board -> Picture
renderBoard b = Pictures [renderCell (r, c) (b !! r !! c) | r <- [0..7], c <- [0..7]]

renderGame :: GameState -> Picture
renderGame gs = Pictures
  [ renderBoard (board gs)
  , translate (-390) (-380) $ scale 0.15 0.15 $ color white $ Text (statusMessage gs)
  , case selected gs of
      [] -> Blank
      xs -> translate (-390) (-350) $
              scale 0.15 0.15 $
                color yellow $
                  Text ("Wybrano: " ++ show xs)
  ]
hasPiecesLeft :: Board -> Bool
hasPiecesLeft board =
  let (countX, countO) = countPieces board
  in countX > 0 && countO > 0

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Up modifiers mousePos) gs =
  let (mx, my) = mousePos
      row = floor ((4 * cellSizeF - my) / cellSizeF)
      col = floor ((mx + 4 * cellSizeF) / cellSizeF)
      pos = (row, col)
  in if not (isInside pos)
     then gs { statusMessage = "Kliknij w planszę" }
     else case selected gs of
       [] ->
         let piece = getPiece (board gs) pos
         in case piece of
              Just p | (isWhiteTurn gs && toLower p == 'o') || (not (isWhiteTurn gs) && toLower p == 'x') ->
                gs { selected = [pos], statusMessage = "Wybrano figurę " ++ show pos }
              _ -> gs { statusMessage = "Nieprawidłowy wybór" }

       selectedPositions@(start:_) ->
         if isCtrlPressed modifiers
         then
           let newSelection = selectedPositions ++ [pos]
           in gs { selected = newSelection, statusMessage = "Dodano pozycję: " ++ show pos }
         else
           let fullSequence = selectedPositions ++ [pos]
           in case makeMovesSequence (board gs) fullSequence of
                Just newBoard ->
                  let updatedGameState = gs { board = newBoard, selected = [], isWhiteTurn = not (isWhiteTurn gs), statusMessage = "Wykonano sekwencję ruchów" }
                  in if not (hasPiecesLeft newBoard)
                     then updatedGameState { statusMessage = "Koniec gry! Jeden z graczy stracił wszystkie pionki." }
                     else updatedGameState
                Nothing ->
                  gs { selected = [], statusMessage = "Nieprawidłowa sekwencja ruchów" }

handleEvent _ gs = gs

isCtrlPressed :: Modifiers -> Bool
isCtrlPressed (Modifiers { ctrl = Down }) = True
isCtrlPressed _ = False

handleEventBot :: Event -> GameState -> GameState
handleEventBot (EventKey (MouseButton LeftButton) Up modifiers mousePos) gs =
  let board' = board gs
      (countX, countO) = countPieces board'
  in if countX == 0 
     then gs { statusMessage = "Białe wygrywają!" }
     else if countO == 0
          then gs { statusMessage = "Czarne wygrywają!" }
          else let (mx, my) = mousePos
                   row = floor ((4 * cellSizeF - my) / cellSizeF)
                   col = floor ((mx + 4 * cellSizeF) / cellSizeF)
                   pos = (row, col)
               in if not (isInside pos)
                  then gs { statusMessage = "Kliknij w planszę" }
                  else case selected gs of
                       [] ->
                         let piece = getPiece board' pos
                         in case piece of
                              Just p | isWhiteTurn gs && toLower p == 'o' ->
                                gs { selected = [pos], statusMessage = "Wybrano figurę " ++ show pos }
                              _ -> gs { statusMessage = "Nie możesz wybrać tej figury!" }

                       selectedPositions@(start:_) ->
                         if isCtrlPressed modifiers
                         then
                           let newSelection = selectedPositions ++ [pos]
                           in gs { selected = newSelection, statusMessage = "Dodano pozycję: " ++ show pos }
                         else
                           let fullSequence = selectedPositions ++ [pos]
                           in case makeMovesSequence board' fullSequence of
                                Just newBoard ->
                                  let (newCountX, newCountO) = countPieces newBoard
                                  in if newCountX == 0
                                     then gs { board = newBoard, selected = [], isWhiteTurn = True, statusMessage = "Białe wygrywają!" }
                                     else if newCountO == 0
                                          then gs { board = newBoard, selected = [], isWhiteTurn = True, statusMessage = "Czarne wygrywają!" }
                                          else let botMove = chooseBestOrAnyMove newBoard 3 False 
                                               in case botMove of
                                                    Just botBoard ->
                                                      let (finalCountX, finalCountO) = countPieces botBoard
                                                      in if finalCountX == 0
                                                         then gs { board = botBoard, selected = [], isWhiteTurn = True, statusMessage = "Białe wygrywają!" }
                                                         else if finalCountO == 0
                                                              then gs { board = botBoard, selected = [], isWhiteTurn = True, statusMessage = "Czarne wygrywają!" }
                                                              else gs { board = botBoard, selected = [], isWhiteTurn = True, statusMessage = "Bot wykonał ruch" }
                                                    Nothing -> gs { board = newBoard, selected = [], isWhiteTurn = True, statusMessage = "Białe wygrywają!" }
                                Nothing -> gs { selected = [], statusMessage = "Nieprawidłowa sekwencja ruchów" }

handleEventBot _ gs = gs


updateGame :: Float -> GameState -> GameState
updateGame _ gs = gs

main :: IO ()
main = play
  (InWindow "Warcaby" (windowSize, windowSize) (100, 100))
  black
  30
  (GameState initialBoard [] True "Kliknij sobie")
  renderGame
  handleEvent
  updateGame

mainBot :: IO ()
mainBot = play
  (InWindow "Warcaby" (windowSize, windowSize) (100, 100))
  black
  30
  (GameState initialBoard [] True "Kliknij sobie")
  renderGame
  handleEventBot
  updateGame
