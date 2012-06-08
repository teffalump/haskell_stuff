-- chess game
-- chessboard config saved as list of tuples  of tuples
-- --> [((b/w, piece), (row, column))....]; 'b', 'w', 
-- 0 = pawn
-- 1 = horse
-- 2 = bishop
-- 3 = castle
-- 4 = queen
-- 5 = king
-- though maybe i'll just use text

import Data.List

data Piece = Piece ((Char, Char), (Int, Int))
data Board = Board [Piece]

-- starting board
chessBoard :: Board
chessBoard = [
                (('w', 'p'),(0,1)),
                (('w', 'p'),(1,1)),
                (('w', 'p'),(2,1)),
                (('w', 'p'),(3,1)),
                (('w', 'p'),(4,1)),
                (('w', 'p'),(5,1)),
                (('w', 'p'),(6,1)),
                (('w', 'p'),(7,1)),
                (('b', 'p'),(0,7)),
                (('b', 'p'),(1,7)),
                (('b', 'p'),(2,7)),
                (('b', 'p'),(3,7)),
                (('b', 'p'),(4,7)),
                (('b', 'p'),(5,7)),
                (('b', 'p'),(6,7)),
                (('b', 'p'),(7,7)),
                (('w', 'c'),(0,0)),
                (('w', 'c'),(7,0)),
                (('b', 'c'),(7,7)),
                (('b', 'c'),(7,0)),
                (('w', 'h'),(0,1)),
                (('w', 'h'),(0,6)),
                (('b', 'h'),(1,7)),
                (('b', 'h'),(6,7)),
                (('w', 'b'),(2,0)),
                (('w', 'b'),(6,0)),
                (('b', 'b'),(2,7)),
                (('b', 'b'),(5,7)),
                (('w', 'q'),(3,0)),
                (('b', 'q'),(3,7)),
                (('w', 'k'),(4,0)),
                (('b', 'k'),(4,7))
             ]



-- find piece at position given (column, row)
-- return piece or nothing
whatPiece :: (Int, Int) -> Maybe Piece
whatPiece (x,y) = find (\z -> let 
                                  a = fst . snd $ z
                                  b = snd . snd $ z
                              in
                                  if (a == x) && (b == y)
                                      then
                                        True
                                      else
                                        False) chessBoard


-- move function needs: piece position and target position
-- returns True if successful, else position invalid
{-move :: (Int, Int) -> (Int, Int) -> Bool-}
