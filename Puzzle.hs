module Puzzle where

import Data.Maybe
import Data.List
import Data.Vector ((//), (!), Vector)
import qualified Data.Vector as V

-- Backwards recursive Steinhaus-Johnson-Trotter algorithm (see
-- writeup.rst for more info)
nthPermutation :: Int -> [a] -> [a]
nthPermutation 0 [] = []
nthPermutation n l@(x:xs) = insertZigZag n x $
                            nthPermutation n' xs
  where
    n' = n `div` genericLength l

    insertZigZag :: Int -> a -> [a] -> [a]
    insertZigZag k y ys = take offset ys ++ y : drop offset ys
      where
        zigLength = genericLength ys + 1
        offset' = k `mod` (2 * zigLength)
        offset
          | offset' < zigLength
            = offset'
          | otherwise
            = zigLength - (offset' - zigLength) - 1

-- The main board data type. It's stored as a one dimensional vector
-- of Maybe Int values.
data Board = Board Int (Vector (Maybe Int)) deriving (Eq)

-- This allows us to print boards easily. The code looks ugly but the
-- display comes out looking nice :)
instance Show Board where
  show (Board i v)
    | sort (map isNothing l) /= take 15 (repeat False) ++ [True]
      || isJust (l !! i)    -- sanity check
      = error "Broken Board object!"
    | otherwise
      = unlines [ ""
                , ",----+----+----+----."
                , set $ take 4 $ l
                , "+----+----+----+----+"
                , set $ take 4 $ drop 4  l
                , "+----+----+----+----+"
                , set $ take 4 $ drop 8  l
                , "+----+----+----+----+"
                , set $ take 4 $ drop 12 l
                , "`----+----+----+----'"
                ]
    where
      l = V.toList v
      show' Nothing = "  "
      show' (Just p) = show p
      pad [c] = [c, ' ']
      pad p@[c, c'] = p
      set xs = "| " ++ intercalate " | " (map (pad . show') xs) ++ " |"

-- This function takes a list of the numbers from 1 through 15 in some
-- order, and a position at which to insert the empty space, and creates
-- a board from that information.
makeBoardFromList :: [Int] -> Int -> Board
makeBoardFromList nums emptyPos =
  Board emptyPos $ V.fromList
  $ take emptyPos nums' ++ Nothing : drop emptyPos nums'
  where
    nums' = map Just nums

-- There are 10461394944000 possible boards, which we can retrieve
-- quickly by their ID. See writeup.rst for more information about how
-- this works.
makeBoardFromID :: Int -> Board
makeBoardFromID n = makeBoardFromList (nthPermutation i [1..15]) j'
  where
    i = n `div` 8
    j = n `mod` 8
    evens = [15, 14, 13, 12, 7, 6, 5, 4]
    odds  = [11, 10, 9, 8, 3, 2, 1, 0]
    j' | even i = evens !! j
       | odd  i = odds  !! j

-- This, board #0, is the "perfect" board, i.e. the one which the player
-- tries to reach.
perfectBoard :: Board
perfectBoard = makeBoardFromID 0

-- Find the Manhattan distance between two points in the puzzle, as
-- indexed linearly according to our data structure
manhattan :: Int -> Int -> Int
manhattan a b = abs (mod a 4 - mod b 4) +
                abs (div a 4 - div b 4)


-- This is an enumerated data type representing possible moves you can
-- make. The direction U/D/L/R refers to which direction the space, not
-- the piece, moves.
data Move = MvU | MvD | MvL | MvR deriving (Enum, Eq, Ord)

-- This is easier to read than the constructor names
instance Show Move where
  show MvU = "↑"
  show MvD = "↓"
  show MvL = "←"
  show MvR = "→"

-- How to find the piece to move into the space, given a direction for
-- the space to move
moveOffset :: Move -> Int
moveOffset MvU = -4
moveOffset MvD = 4
moveOffset MvL = -1
moveOffset MvR = 1

-- Opposite directions mapping
moveOpposite :: Move -> Move
moveOpposite MvU = MvD
moveOpposite MvD = MvU
moveOpposite MvL = MvR
moveOpposite MvR = MvL

-- Check whether a move would go out of bounds from a given position
canMoveFrom :: Int -> Move -> Bool
canMoveFrom i MvU = i `div` 4 /= 0
canMoveFrom i MvD = i `div` 4 /= 3
canMoveFrom i MvL = i `mod` 4 /= 0
canMoveFrom i MvR = i `mod` 4 /= 3

-- Find what move would go from the first board to the second, if any.
-- Not a super necessary function, but was useful for some debugging
-- code...
diffBoards :: Board -> Board -> Maybe Move
diffBoards b1 b2 = find ((== b2) . flip makeMove b1) [MvU, MvD, MvL, MvR]

-- Check what moves are possible from the current state.
possibleMoves :: Board -> [Move]
possibleMoves (Board i _) = filter (canMoveFrom i) [MvU, MvD, MvL, MvR]

-- Find out how a board would look after making a given move.
makeMove :: Move -> Board -> Board
makeMove m (Board i v)
  = Board (i+d) $ v // [(i+d, Nothing), (i, v ! (i+d))]
  where d = moveOffset m


-- A type for restrictions on the game state which we might want to
-- abide by during AI search.
type Restriction = (Board -> Bool)

-- The most common kind of restriction - the fixing of a certain piece
-- in its current position.
fixPos :: Board -> Int -> Restriction
fixPos (Board _ v) i (Board _ v') = v' ! i == v ! i


-- AI stuff

-- Check what moves are possible from the current state, while also
-- obeying certain restrictions.
possibleMoves' :: Board -> [Restriction] -> [Move]
possibleMoves' b@(Board i v) rs
  = filter (\move -> and (map ($ makeMove move b) rs)) $ possibleMoves b

-- See writeup.rst for more info about the following stuff, which
-- doesn't work yet.

-- findShiftSequence :: (Int, Move) -> Board -> [Restriction] ->
--                      Maybe (Board, [Move])
-- findShiftSequence (pos, direction) b rs
--   | not $ canMoveFrom pos direction = Nothing
--   | isNothing res = Nothing
--   | otherwise =
--     Just (last boardSeq, map fromJust $
--                          zipWith diffBoards boardSeq (tail boardSeq))
--   where
--     res = aStar getAdj (curry $ const 1) heur doneQ b
--     getAdj b0 = S.fromList $ map (flip makeMove b0) $ possibleMoves' b0 rs'
--     heur (Board i _) = manhattan i pos
--     doneQ (Board i _) = pos + moveOffset direction == i
--     rs' = fixPos b pos : rs
-- 
--     Just res' = res
--     finalBoard = makeMove (moveOpposite direction) (last res')
--     boardSeq = b : res' ++ [finalBoard]

-- findMigration :: Int -> Int -> Board -> [Restriction] ->
--                  Maybe (Board, [Move])
-- findMigration piece dest b rs
--   | undefined = undefined
--   where
--     res :: Maybe [(Board, [Move])]
--     res = aStar getAdj (curry $ const 1) heur doneQ (b, [])
--     -- getAdj b0 = S.fromList $ filter 
--     getAdj = undefined
--     heur (Board _ v, _) = manhattan (fromJust . fromJust $ V.find (==Just piece) v) dest
--     doneQ (Board _ v, _) = v ! dest == Just piece

-- findShiftSequence :: (Int, Move) -> Board -> [Restriction] ->
--                      Maybe [(Move, Board)]
-- findShiftSequence (pos, direction) b@(Board i _) rs
--   | canMoveFrom pos direction && isJust res
--     = Just $ reverse $ (moveOpposite direction,
--                         makeMove (moveOpposite direction)
--                                  (snd $ head (fromJust res))
--                        ) : fromJust res
--   | otherwise = Nothing
--   where
--     rs' = fixPos b pos : rs
--     res = findShiftSequence' b [i] []
--     findShiftSequence' :: Board -> [Int] -> [(Move, Board)] ->
--                           Maybe [(Move, Board)]
--     findShiftSequence' b@(Board i _) seen moves
--       | i == pos + moveOffset direction
--         = Just moves
--       | isNothing res
--         = Nothing
--       | otherwise
--         = Just ((m, b):moves')
--       where
--         res = find (isJust . snd) $
--               [ (m, findShiftSequence' (makeMove m b) (i:seen)
--                     ((m, makeMove m b):moves))
--               | m <- possibleMoves' b rs'
--               , not ((i + moveOffset m) `elem` seen)
--               ]
--         Just (m, Just moves') = res

-- heuristic :: Int -> Board -> Board -> Int
-- heuristic limit (Board _ v1) (Board _ v2) = sum $ zipWith manhattan l r
--   where
--     l = map (fromJust . flip V.elemIndex v1 . Just) [1..limit]
--     r = map (fromJust . flip V.elemIndex v2 . Just) [1..limit]

--solve :: Board -> [Move]
--solve cur = solve' (cur, [], [])
--  where
--    solve' :: (Board, [Int], [Move]) -> (Board, [Int], [Move])
--    solve' b fixed
----  stage1 = 
----  firstRowFinished (Board _ v1) = take 4 (V.toList v1) == map Just [1, 2, 3, 4]
----  Just stage1 = aStar getAdj (curry $ const 1) (heuristic 4 perfectBoard)
----                firstRowFinished cur
----  in stage1
----  = perfectBoard : (fromJust $ aStar getAdj (curry $ const 1) ((5*) . heuristic goal) (== cur) goal)
--   where
--     getAdj b = S.fromList $ map (flip makeMove b) $ possibleMoves b []

---- Test
--main = do
--  x <- readLn
--  print $ solve $ makeBoardFromID x
