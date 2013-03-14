module Main where

import Graphics.Vty
import System.Environment

import Puzzle

-- This function is the main interaction loop in which the player makes
-- moves and observes the results of those moves.
playWithUser :: Vty -> Board -> String -> IO ()
playWithUser vty b@(Board i v) message = do
  release_display $ terminal vty
  reserve_display $ terminal vty
  putStrLn message
  putStrLn (if b == perfectBoard
            then "Note: This board is solved"
            else "")
  print b
  e <- next_event vty
  let continue m = do
        if canMoveFrom i m
          then playWithUser vty (makeMove m b) ("Moved " ++ show m)
          else playWithUser vty b ("Cannot move " ++ show m)
  case e of
    EvKey KEsc [] -> return ()
    EvKey (KASCII 'q') [] -> return ()
    EvKey KUp [] -> continue MvU
    EvKey KDown [] -> continue MvD
    EvKey KLeft [] -> continue MvL
    EvKey KRight [] -> continue MvR
    _ -> playWithUser vty b "Invalid command"

-- Here is the main function, which calls the interaction loop.
main = do
  args <- getArgs
  let boardID = case args of
        [] -> 0
        (n:_) -> read n
  vty <- mkVty
  playWithUser vty (makeBoardFromID boardID)
    "Welcome! Play using the ↑, ↓, ←, and → keys."
  shutdown vty
  putStrLn "Thanks for playing!"
