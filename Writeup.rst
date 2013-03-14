---------
15-puzzle
---------

Name:  Keshav Kini

Email: kini@pdx.edu

My aim in this project was to write an implementation of the well-known
"`15-puzzle`_" game in Haskell, using the `hsqml`_ library by Robin Kay,
which implements a "low- to medium-level interface" to the Qt Quick
component of the C++ GUI toolkit Qt (commonly known as the foundation of
the `KDE Software Compilation`_, a full-featured desktop environment for
Linux). Ever since Qt Quick was announced in 2009, I've been interested
in trying it out, and I was particularly happy to see last September
that Robin Kay had written hsqml, allowing Haskell programs to use
QtDeclarative.

Unfortunately, I miscalculated and wasn't even able to begin on the
graphical interface to the program, as I found the intricacy of the AI
programming required to solve the 15-puzzle in a reasonable amount of
time (i.e. less than one second) to be much greater than I had
anticipated.

So while the final product (so far) is a simple terminal-based
interactive 15-puzzle game with no AI solver, I still have some things
to report about what I tried but didn't manage to succeed in doing.

Note: only some of the code is displayed within this file - please refer
to the actual .hs files if you wish to see all of it. Also, some code
displayed within this file is not in the .hs files because it isn't
complete yet.

.. _15-puzzle: http://en.wikipedia.org/wiki/15-puzzle
.. _hsqml: http://gekkou.co.uk/software/hsqml/
.. _KDE Software Compilation: http://kde.org/

Generating the 15-puzzle
------------------------

It is clear that each move flips the sign of the permutation of
Nothing : [1..15] the board represents, because a move consists of
transposing the empty tile (Nothing) with one of the numbered tiles. At
the same time, each move also increases or decreases by one the
Manhattan distance of the empty tile from the lower right hand corner
(or from any given point on the puzzle, for that matter). Reducing this
modulo 2 we get another flipping parity bit. Therefore, xoring these two
parity bits gives us an invariant value; all solvable instances of the
15-puzzle must have the same value of this invariant as the solved
board.

This gives us a necessary condition for solvability; it is also known
that this is a sufficient condition [A99]_.

I used a variant of the `Steinhaus-Johnson-Trotter algorithm`_ to create
an enumeration of all possible 15-puzzles which can be queried
instantly. Here is the implementation of the permutation finder::

> -- Backwards recursive Steinhaus-Johnson-Trotter algorithm
> nthPermutation :: Int -> [a] -> [a]
> nthPermutation 0 [] = []
> nthPermutation n l@(x:xs) = insertZigZag n x $
>                             nthPermutation n' xs
>   where
>     n' = n `div` genericLength l
> 
>     insertZigZag :: Int -> a -> [a] -> [a]
>     insertZigZag k y ys = take offset ys ++ y : drop offset ys
>       where
>         zigLength = genericLength ys + 1
>         offset' = k `mod` (2 * zigLength)
>         offset
>           | offset' < zigLength
>             = offset'
>           | otherwise
>             = zigLength - (offset' - zigLength) - 1

`insertZigZag` inserts an element into a list at a given position (like
`insert` in `Data.List`) but allows the index to exceed the length of
the list, and wraps around in a zig-zag pattern, like so::

    0 -> _abc
    1 -> a_bc
    2 -> ab_c
    3 -> abc_
    4 -> abc_
    5 -> ab_c
    6 -> a_bc
    7 -> _abc
    etc.

The main feature of the Steinhaus-Johnson-Trotter enumeration is that
each permutation differs from its neighbors only by a single
transposition in the symmetric group. This allows us to assume that the
sign of the nth permutation is equal to the parity of n, which is the
key point.

Of course, it's not as simple as just that - we also need to worry about
the Manhattan distance of the empty tile from the lower right hand
corner. To do both of these at the same time, we can use the fact that
the parity of some permutation of 16 tiles is equal to the parity of the
number of moves it would take to move the empty space from where it is
to the lower right hand corner, not by normal puzzle moves, but by
moving only to the right and wrapping down to the beginning of the next
line when the empty space reaches the edge of the square.

Here are the parity maps of the 15-puzzle, first for Manhattan distance
from the lower right hand corner, then for the distance described by the
above horizontal-motion-with-wrapping::

    E O E O
    O E O E
    E O E O
    O E O E

    O E O E
    O E O E
    O E O E
    O E O E

Then we can xor the two maps to get what we need to keep track of in
terms of enumerating our 15-puzzles::

    O O O O
    E E E E
    O O O O
    E E E E

Here's the function that implements what I've just described. ::

> makeBoardFromList :: [Int] -> Int -> Board
> -- snip --
> 
> makeBoardFromID :: Int -> Board
> makeBoardFromID n = makeBoardFromList (nthPermutation i [1..15]) j'
>   where
>     i = n `div` 8
>     j = n `mod` 8
>     evens = [15, 14, 13, 12, 7, 6, 5, 4]
>     odds  = [11, 10, 9, 8, 3, 2, 1, 0]
>     j' | even i = evens !! j
>        | odd  i = odds  !! j

Appropriately, the solved board is the very first one::

> perfectBoard :: Board
> perfectBoard = makeBoardFromID 0


Manipulating the 15-puzzle
--------------------------

There's nothing particularly interesting here. We have a board data type
which uses the `Data.Vector` library for speed::

> data Board = Board Int (Vector (Maybe Int)) deriving (Eq)

I wrote a very ugly looking instance of the Show class, which
nevertheless produces pretty nice looking pictures of 15-puzzles::

    Puzzle> makeBoardFromID 321878651

    ,----+----+----+----.
    | 3  | 1  | 9  | 8  |
    +----+----+----+----+
    | 4  | 10 | 5  | 2  |
    +----+----+----+----+
    |    | 11 | 12 | 13 |
    +----+----+----+----+
    | 7  | 14 | 6  | 15 |
    `----+----+----+----'

And there are various functions which allow you to find what moves are
possible, change a board by making a move, etc.

.. [A99] http://dx.doi.org/10.2307%2F2589612
.. _Steinhaus-Johnson-Trotter algorithm:
    http://en.wikipedia.org/wiki/Plain_changes

Solving the 15-puzzle
---------------------

My first attempt to write an AI solver for the 15-puzzle was to use an
A* search, with a distance heuristic calculated by summing the Manhattan
distances of each tile in the current board to where the same tile
should lie on a solved board. Clearly this is an admissible heuristic,
as each move only changes the position of a single tile, and changes its
Manhattan distance from any given point on the board by either -1, 0, or
1; thus a single move can at best reduce the distance heuristic by 1,
and so to reduce the heuristic to 0 requires a number of moves equal to
the heuristic.

This appeared to work, but was extremely slow. And no wonder, since
there are 16 * (factorial 16 / 2) = 10461394944000 possible states in
the search space! I had hoped that the heuristic function, which seems
to give a reasonable amount of gradation (more than a couple dozen in
many cases), would make the search space manageable, but I guess that
was a bit naive.

Instead, I decided to emulate a common human-oriented strategy; namely
to first solve the top row, then the second row, and finally the bottom
two rows simultaneously (a much smaller search space). This of course
produces much longer solutions than actually necessary, but since a human
is able to do it in a reasonable amount of time, it should be much much
faster than the A* search when done by a computer. Another benefit (as
mentioned by commenters on `a relevant StackExchange question`_) is that
the solutions produced will be more comprehensible to the player, rather
than appearing "magical.

A more precise description of the algorithm described above can be seen,
for example, `at ChessandPoker.com`_.

To implement this algorithm, I threaded a new "restrictions" argument
through a few of my board manipulation functions. The idea is that the
AI solver should be able to proceed while satisfying certain constraints
on what boards it is allowed to move through. This is important most
obviously because you don't want to unsolve the first row in the second
stage after you've solved it in the first stage of the algorithm, but
also because the algorithm proceeds in terms of higher level steps than
simple shuffling of the empty space; humans think in terms of which
*piece* they want to move in which direction.

To this end, I tried to write a function that would prescribe a series
of moves required to move an individual tile in a given direction::

> findShiftSequence :: (Int, Move) -> Board -> [Restriction] ->
>                      Maybe [(Move, Board)]
> findShiftSequence (pos, direction) b@(Board i _) rs
>   | canMoveFrom pos direction && isJust res
>     = Just $ reverse $ (moveOpposite direction,
>                         makeMove (moveOpposite direction)
>                                  (snd $ head (fromJust res))
>                        ) : fromJust res
>   | otherwise = Nothing
>   where
>     rs' = fixPos b pos : rs
>     res = findShiftSequence' b [i] []
>     findShiftSequence' :: Board -> [Int] -> [(Move, Board)] ->
>                           Maybe [(Move, Board)]
>     findShiftSequence' b@(Board i _) seen moves
>       | i == pos + moveOffset direction
>         = Just moves
>       | isNothing res
>         = Nothing
>       | otherwise
>         = Just ((m, b):moves')
>       where
>         res = find (isJust . snd) $
>               [ (m, findShiftSequence' (makeMove m b) (i:seen)
>                     ((m, makeMove m b):moves))
>               | m <- possibleMoves' b rs'
>               , not ((i + moveOffset m) `elem` seen)
>               ]
>         Just (m, Just moves') = res

This function uses a standard dumb depth-first search to find a path by
which the empty space can move to the appropriate side of the piece
which we want to move, thus allowing us to move the piece in the
direction we want. Rather than simply avoiding previously visited board
configurations, it avoids any board where the empty space lies in the
same position as it did in some previously visited board. This vastly
reduces the search space, but there are a couple of problems.

First, I didn't prove that this is a complete search (i.e. it might fail
even when there is a way to move the empty space where we want without
breaking the restrictions given). Speculation: the proof might be easier
if I assume that all the restrictions are of the form "x piece must
remain in y position".

Second, there is some massive bug which causes the list of moves
returned to often contain illegal moves (moving the empty space out of
the puzzle bounds). I spent way too long trying to figure out why this
was happening, and I still have no idea. As far as I can see, the only
time that a move is ever generated and stuck onto the list that is
eventually returned is within the list comprehension on the last few
lines, which explicitly forbids illegal moves by the selection of the
move by the simple function `possibleMoves'`, which I've tested
extensively and am quite sure is correct (here it is below). ::

> possibleMoves' :: Board -> [Restriction] -> [Move]
> possibleMoves' b@(Board i v) rs
>   = filter (\move -> and (map ($ makeMove move b) rs)) $ possibleMoves b

The return type of `findShiftSequence`, namely `[(Board, Move)]`, was
originally `[Move]`; I changed it in an attempt to debug. As
I suspected, the boards didn't match the moves that were supposedly
made. Something very odd was going on.

Finally I decided to abandon this search method and try to reimplement
this function using the A* search from earlier, and a heuristic just
based on how close the empty space is to its destination::

> findShiftSequence :: (Int, Move) -> Board -> [Restriction] ->
>                      Maybe (Board, [Move])
> findShiftSequence (pos, direction) b rs
>   | not $ canMoveFrom pos direction = Nothing
>   | isNothing res = Nothing
>   | otherwise =
>     Just (last boardSeq, map fromJust $
>                          zipWith diffBoards boardSeq (tail boardSeq))
>   where
>     res = aStar getAdj (curry $ const 1) heur doneQ b
>     getAdj b0 = S.fromList $ map (flip makeMove b0) $ possibleMoves' b0 rs'
>     heur (Board i _) = manhattan i pos
>     doneQ (Board i _) = pos + moveOffset direction == i
>     rs' = fixPos b pos : rs
> 
>     Just res' = res
>     finalBoard = makeMove (moveOpposite direction) (last res')
>     boardSeq = b : res' ++ [finalBoard]

This works splendidly, at least when there is a solution, but of course
the search space is still very large, and so when there is no solution,
the A* search takes a very long time to discover that fact. This becomes
a big problem during the next higher level of the algorithm, which is to
attempt to move a tile with a certain number on it to a certain
destination position -- in that process, there will surely be plenty of
junctures at which an impossible move is attempted.

My next plan would be to try to rewrite the A* search to use a different
equality relation when checking for already seen vertices, in order to
replicate the smaller search space achieved by the above-described
depth-first search. Unfortunately I didn't have time to do this.

.. _a relevant StackExchange question:
    http://gamedev.stackexchange.com/questions/42096
.. _at ChessandPoker.com:
    http://www.chessandpoker.com/fifteen-puzzle-solution.html

Player interaction in the terminal
----------------------------------

I used the Graphics.Vty library (provided by the `vty`_ package on
hackage) to make a terminal interface for my 15-puzzle game. The main
loop looks like this::

> playWithUser :: Vty -> Board -> String -> IO ()
> playWithUser vty b@(Board i v) message = do
>   release_display $ terminal vty
>   reserve_display $ terminal vty
>   putStrLn message
>   putStrLn (if b == perfectBoard
>             then "Note: This board is solved"
>             else "")
>   print b
>   e <- next_event vty
>   let continue m = do
>         if canMoveFrom i m
>           then playWithUser vty (makeMove m b) ("Moved " ++ show m)
>           else playWithUser vty b ("Cannot move " ++ show m)
>   case e of
>     EvKey KEsc [] -> return ()
>     EvKey (KASCII 'q') [] -> return ()
>     EvKey KUp [] -> continue MvU
>     EvKey KDown [] -> continue MvD
>     EvKey KLeft [] -> continue MvL
>     EvKey KRight [] -> continue MvR
>     _ -> playWithUser vty b "Invalid command"

Not really that much to say here, I suppose. There is an interesting UI
library built on top of Graphics.Vty, by the way, called `vty-ui`_.
Looks interesting.

.. _vty: http://hackage.haskell.org/package/vty
.. _vty-ui: http://jtdaugherty.github.com/vty-ui/

The idea behind hsqml
---------------------

I didn't actually get to this, sadly, but it's an interesting concept.

QML (Qt Markup Language?) is a declarative language used to define user
interfaces in the Qt Quick system introduced to Qt 4.x in 2009, and as
far as I can tell seems to be intended as the standard way to do UI
design in Qt5 (which has yet to be released as of this writing, in March
2013). In Qt Quick you define various kinds of objects and their
interactions in a declarative fashion; the backend of your application
is typically written in C++, though non-declarative behavior can be
easily connected to your QML UI designs if you write them in Javascript,
with which QML has a direct interface.

hsqml allows you you to write an interactive application using Haskell
for the pure or mostly pure backend, and QML for the declarative or
mostly declarative UI. On the surface, this seems to me to be quite
a propitious arrangement, much closer to the Haskell mindset than, say,
GtkHs yet equipped with the full power of a very robust and mature UI
toolkit.

The example application that Robin Kay has written seems to use
Javascript to handle the interaction between Haskell and the QML UI, so
perhaps you don't get as much declarative goodness as you might be able
to in general. As I said, though, I wasn't able to further investigate
this.

