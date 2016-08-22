module Reversi where

import qualified Data.Sequence as S
import Data.Sequence (Seq)

-- Row and Col indexes start at 0
type Row = Int
type Col = Int

data Piece = PieceX | PieceO deriving (Eq)

instance Show Piece where
    show PieceX = "x"
    show PieceO = "o"

type Reversi = Seq (Maybe Piece)

size = 8

-- | Returns a completely empty game board
empty :: Reversi
empty = S.fromList $ take (size * size) $ repeat Nothing

-- | Returns a new game with the default pieces in place
new :: Reversi
new = set (middle', middle') PieceX $
      set (middle', middle) PieceO $
      set (middle, middle') PieceO $
      set (middle, middle) PieceX empty
    where middle = size `div` 2
          middle' = middle - 1

-- | Sets the given row and column index to the given piece and returns a new game
set :: (Row, Col) -> Piece -> Reversi -> Reversi
set (row, col) = S.update (row * size + col) . Just

format :: Reversi -> String
format = S.foldlWithIndex formatter ""
    where
        formatter acc i tile =
            acc ++ " | " ++ (formatTile tile) ++ (lineEnding i)
        formatTile Nothing = " "
        formatTile (Just piece) = show piece
        lineEnding index
            | succ index `rem` size == 0 = " |\n"
            | otherwise = ""

