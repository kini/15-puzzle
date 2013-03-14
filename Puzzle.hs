module Puzzle where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Vector ((//), (!), Vector)
import qualified Data.Vector as V
import Data.Graph.AStar
-- import qualified Data.Heap as H
import qualified Data.Set  as S
import System.Timeout

-- Backwards recursive Steinhaus-Johnson-Trotter algorithm
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

data Board = Board Int (Vector (Maybe Int)) deriving (Eq)

instance Ord Board where
  compare (Board _ v1) (Board _ v2) = compare v1 v2

instance Show Board where
  show (Board i v)
    | sort (map isNothing l) /= take 15 (repeat False) ++ [True]
      || isJust (l !! i)
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

makeBoardFromList :: [Int] -> Int -> Board
makeBoardFromList nums emptyPos =
  Board emptyPos $ V.fromList
  $ take emptyPos nums' ++ Nothing : drop emptyPos nums'
  where
    nums' = map Just nums

-- There are 10461394944000 possible boards, which we can retrieve
-- quickly by their ID
makeBoardFromID :: Int -> Board
makeBoardFromID n = makeBoardFromList (nthPermutation i [1..15]) j'
  where
    i = n `div` 8
    j = n `mod` 8
    evens = [15, 14, 13, 12, 7, 6, 5, 4]
    odds  = [11, 10, 9, 8, 3, 2, 1, 0]
    j' | even i = evens !! j
       | odd  i = odds  !! j

perfectBoard :: Board
perfectBoard = makeBoardFromID 0

manhattan :: Int -> Int -> Int
manhattan a b = abs (mod a 4 - mod b 4) +
                abs (div a 4 - div b 4)


data Move = MvU | MvD | MvL | MvR deriving (Enum, Eq, Ord)

instance Show Move where
  show MvU = "↑"
  show MvD = "↓"
  show MvL = "←"
  show MvR = "→"

moveOffset :: Move -> Int
moveOffset MvU = -4
moveOffset MvD = 4
moveOffset MvL = -1
moveOffset MvR = 1

moveOpposite :: Move -> Move
moveOpposite MvU = MvD
moveOpposite MvD = MvU
moveOpposite MvL = MvR
moveOpposite MvR = MvL

canMoveFrom :: Int -> Move -> Bool
canMoveFrom i MvU = i `div` 4 /= 0
canMoveFrom i MvD = i `div` 4 /= 3
canMoveFrom i MvL = i `mod` 4 /= 0
canMoveFrom i MvR = i `mod` 4 /= 3

diffBoards :: Board -> Board -> Maybe Move
diffBoards b1 b2 = find ((== b2) . flip makeMove b1) [MvU, MvD, MvL, MvR]

possibleMoves :: Board -> [Move]
possibleMoves (Board i _) = filter (canMoveFrom i) [MvU, MvD, MvL, MvR]

makeMove :: Move -> Board -> Board
makeMove m (Board i v)
  = Board (i+d) $ v // [(i+d, Nothing), (i, v ! (i+d))]
  where d = moveOffset m

type Restriction = (Board -> Bool)

fixPos :: Board -> Int -> Restriction
fixPos (Board _ v) i (Board _ v') = v' ! i == v ! i


-- AI stuff
possibleMoves' :: Board -> [Restriction] -> [Move]
possibleMoves' b@(Board i v) rs
  = filter (\move -> and (map ($ makeMove move b) rs)) $ possibleMoves b

findShiftSequence :: (Int, Move) -> Board -> [Restriction] ->
                     Maybe (Board, [Move])
findShiftSequence (pos, direction) b rs
  | not $ canMoveFrom pos direction = Nothing
  | isNothing res = Nothing
  | otherwise =
    Just (last boardSeq, map fromJust $
                         zipWith diffBoards boardSeq (tail boardSeq))
  where
    res = aStar getAdj (curry $ const 1) heur doneQ b
    getAdj b0 = S.fromList $ map (flip makeMove b0) $ possibleMoves' b0 rs'
    heur (Board i _) = manhattan i pos
    doneQ (Board i _) = pos + moveOffset direction == i
    rs' = fixPos b pos : rs

    Just res' = res
    finalBoard = makeMove (moveOpposite direction) (last res')
    boardSeq = b : res' ++ [finalBoard]

findMigration :: Int -> Int -> Board -> [Restriction] ->
                 Maybe (Board, [Move])
findMigration piece dest b rs
  | undefined = undefined
  where
    res :: Maybe [(Board, [Move])]
    res = aStar getAdj (curry $ const 1) heur doneQ (b, [])
    -- getAdj b0 = S.fromList $ filter 
    getAdj = undefined
    heur (Board _ v, _) = manhattan (fromJust . fromJust $ V.find (==Just piece) v) dest
    doneQ (Board _ v, _) = v ! dest == Just piece

-- findShiftSequence :: (Int, Move) -> Board -> [Restriction] -> Maybe [(Move, Board)]
-- findShiftSequence (pos, direction) b@(Board i _) rs
--   | canMoveFrom pos direction && isJust res
--     = Just $ reverse $ (moveOpposite direction, makeMove (moveOpposite direction) (snd $ head (fromJust res))) : fromJust res
--   | otherwise = Nothing
--   where
--     rs' = fixPos b pos : rs
--     res = findShiftSequence' b [i] []
--     findShiftSequence' :: Board -> [Int] -> [(Move, Board)] -> Maybe [(Move, Board)]
--     findShiftSequence' b@(Board i _) seen moves
--       | i == pos + moveOffset direction
--         = Just moves
--       | isNothing res
--         = Nothing
--       | otherwise
--         = Just ((m, b):moves')
--       where
--         res = find (isJust . snd) $
--               [ (m, findShiftSequence' (makeMove m b) (i:seen) ((m, makeMove m b):moves))
--               | m <- possibleMoves' b rs'
--               , not ((i + moveOffset m) `elem` seen)
--               ]
--         Just (m, Just moves') = res

heuristic :: Int -> Board -> Board -> Int
heuristic limit (Board _ v1) (Board _ v2) = sum $ zipWith manhattan l r
  where
    l = map (fromJust . flip V.elemIndex v1 . Just) [1..limit]
    r = map (fromJust . flip V.elemIndex v2 . Just) [1..limit]

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

--main = do
--  x <- readLn
--  print $ solve $ makeBoardFromID x

-- And here's the search function
--astar :: Board -> (Board -> Bool) -> (Board -> [(Move, Board)]) -> 
--astar start isEnd getOuts cost heuristic

-- astar start succ end cost heur
--     = astar' (S.singleton start) (Q.singleton (heur start) [start])
--  where
--    astar' seen q
--      | Q.null q = error "No Solution."
--      | end n = next
--      | othewise = astar' seen' q'
--      where
--        ((c,next), dq) = Q.deleteFindMin q
--        n = head next
--        succs = filter (`S.notMember` seen) $ succ n
--        costs = map ((+ c) . (subtract $ heur n) . liftM2 (+) cost heur) succs
--        q' = dq `Q.union` Q.fromList (zip costs (map (:next) succs))
--        seen' = seen `S.union` S.fromList succs
