module Reversi where

import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.List (intercalate)
import Data.Foldable (toList)

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
set pos = S.update (_index pos) . Just

-- | Gets the piece at the given row and column index
get :: (Row, Col) -> Reversi -> Maybe Piece
get pos tiles = S.index tiles $ _index pos

-- | Returns the raw Seq index for a given row and col
_index :: (Row, Col) -> Int
_index (row, col) = row * size + col

format :: Reversi -> String
format tiles = columnRow ++ formatRows tiles
    where
        columnRow = (intercalate sep $ cell " " : (map (\c -> cell [c]) $ take size ['A'..'Z'])) ++ sep ++ "\n" ++ divider
        formatRows tiles = concatMap formatRow $ zip [0..] $ rows $ toList tiles
        rows tiles = map (\i -> take size $ drop (i * size) tiles) [0..size-1]
        formatRow (i, row) = (intercalate sep $ rowNumber i : (map (cell . formatTile) row)) ++ sep ++ "\n" ++ divider
        formatTile Nothing = " "
        formatTile (Just piece) = show piece
        cell content = " " ++ content ++ " "
        cellWidth = 3
        rowNumber index = cell $ show $ succ index
        sep = "|"
        -- the succ and +1 are because of the extra row number column
        divider = (take (succ size * cellWidth + size + 1) $ repeat '-') ++ "\n"

