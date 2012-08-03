-- chess game
-- chessboard config saved as list of pieces
-- --> [b/w, piece, row, column....] 
-- b/w = 'b' or 'w'
-- piece = 'c', 'p', etc.
-- row = number
-- column = number

import Data.List
import Data.Maybe
import UtilList

-- define new types
data Position = Position Int Int
data Piece = Piece {
                    color :: Char,
                    piece :: Char,
                    column :: Int,
                    row :: Int
                   } deriving (Show)

-- starting board
chessBoard :: [Piece]
chessBoard = [
                Piece 'w' 'p' 0 1,
                Piece 'w' 'p' 1 1,
                Piece 'w' 'p' 2 1,
                Piece 'w' 'p' 3 1,
                Piece 'w' 'p' 4 1,
                Piece 'w' 'p' 5 1,
                Piece 'w' 'p' 6 1,
                Piece 'w' 'p' 7 1,
                Piece 'b' 'p' 0 1,
                Piece 'b' 'p' 1 1,
                Piece 'b' 'p' 2 1,
                Piece 'b' 'p' 3 1,
                Piece 'b' 'p' 4 1,
                Piece 'b' 'p' 5 1,
                Piece 'b' 'p' 6 1,
                Piece 'b' 'p' 7 1,
                Piece 'w' 'c' 0 0,
                Piece 'w' 'c' 7 0,
                Piece 'b' 'c' 0 7,
                Piece 'b' 'c' 7 7,
                Piece 'w' 'h' 1 0,
                Piece 'w' 'h' 6 0,
                Piece 'b' 'h' 1 7,
                Piece 'b' 'h' 6 7,
                Piece 'w' 'b' 2 0,
                Piece 'w' 'b' 6 0,
                Piece 'b' 'b' 2 7,
                Piece 'b' 'b' 6 7,
                Piece 'w' 'q' 3 0,
                Piece 'b' 'q' 3 7,
                Piece 'w' 'k' 4 0,
                Piece 'b' 'k' 1 7
             ]

-- naive possible move vectors
moves :: Char -> [[Int]]
moves p = case p of 'p' -> [[0,1], [1,1], [-1,1],
                            [0,-1], [1,-1], [-1,-1]] -- pawns, filter diag, direct, bound
                    'h' -> filter (\x -> if abs (x !! 0) == abs (x !! 1)
                                    then False
                                    else True) . make_lists 2 $ [-2,1,-1,2] -- horse, filter bound
                    'b' -> concatMap (make_lists 2) $ [[x,negate x] | x <- [1..7]] -- bish, filter bound, piece
                    'c' -> cMap (\x -> [x !! 1, x !! 0]) [[0,x] | x <- [-7..7], x /= 0] -- cast, filter bound, piece
                    'k' -> [[0,1], [1,1], [-1,1],
                            [0,-1], [1,-1], [-1,-1],
                            [1,0], [-1,0]] -- king, filter bound
                    'q' -> concatMap (make_lists 2) [[x,-x] | x <- [1..7]] 
                            ++ cMap (\x -> [x !! 1, x !! 0]) [[0,x] | x <- [-7..7], x /= 0] -- qu, filter bound, piece

-- possible moves for a piece
possibleMoves :: Piece -> [Position]
possibleMoves (Piece c p col r) = case p of 'p' -> if c == 'w' then [Position col r+1]
                                                               else [Position col r-1]
                                            'h' -> [Position col+1 r+2, 
                                                    Position col r]


-- find piece at position given (column, row)
-- return piece or nothing
whatPiece :: (Int, Int) -> Maybe Piece
whatPiece (x,y) = find (\z -> let 
                                  a = column z
                                  b = row z
                              in
                                  if (a == x) && (b == y)
                                      then
                                        True
                                      else
                                        False) chessBoard


-- move function needs: piece position and target position
-- returns True if successful, else position invalid
{-move :: (Int, Int) -> (Int, Int) -> Bool-}
