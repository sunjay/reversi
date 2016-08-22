module Reversi where

import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Maybe (isJust, isNothing, fromJust)
import System.Console.ANSI (
    setSGRCode,
    SGR(SetColor),
    ConsoleLayer(Foreground),
    ColorIntensity(Vivid),
    Color(Red, Blue, Yellow))

-- Row and Col indexes start at 0
type Row = Int
type Col = Int

data Piece = PieceX | PieceO deriving (Eq, Enum)

instance Show Piece where
    show PieceX = _color Red "\x25CF"
    show PieceO = _color Blue "\x25CF"

_color :: Color -> [Char] -> [Char]
_color color text = setSGRCode [SetColor Foreground Vivid color] ++ text ++ (setSGRCode [])

type Tiles = Seq (Maybe Piece)

data Reversi = Reversi {
    tiles :: Tiles,
    currentPiece :: Piece
}

size :: Int
size = 8

-- | Returns a completely empty game board
empty :: Reversi
empty = Reversi {
    tiles = S.fromList $ take (size * size) $ repeat Nothing,
    currentPiece = PieceX
}

-- | Returns a new game with the default pieces in place
new :: Reversi
new = set (middle', middle') PieceX $
      set (middle', middle) PieceO $
      set (middle, middle') PieceO $
      set (middle, middle) PieceX empty
    where middle = size `div` 2
          middle' = middle - 1

-- | Returns all the valid moves that can be played
validMoves :: Reversi -> [(Row, Col)]
validMoves game = map fromJust $ filter isJust $ map (uncurry findValid) searchSpace
    -- Find the position of each piece that is the same as currentPiece
    -- Go in each direction while the succ piece is being found
    -- If at least one succ piece is found, this is a valid move
    where searchSpace = [((r, c), d) | (r, c, _) <- searchPieces, d <- directions]
          findValid = searchDirection 0
          searchDirection count (row, col) (drow, dcol)
              | isNothing nextPiece =
                  if count > 0 then Just next else Nothing
              | nextPiece == Just target =
                  searchDirection (succ count) next (drow, dcol)
              | otherwise = Nothing
              where next = (row + drow, col + dcol)
                    nextPiece = get next game

          directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]
          searchPieces = filter isPiece $ positions game
          isPiece (_, _, p) = p == piece
          piece = currentPiece game
          target = succ piece

-- | Returns the position of every piece (ignores empty tiles)
positions :: Reversi -> [(Row, Col, Piece)]
positions = S.foldrWithIndex position [] . tiles
    where position _ Nothing acc = acc
          position i (Just piece) acc = (row, col, piece) : acc
              where (row, col) = _position i

-- | Returns all the rows of the board
rows :: Reversi -> [Tiles]
rows game = map row [0..size-1]
    where row i = S.take size $ S.drop (i * size) (tiles game)

-- | Returns all the rows of the board
cols :: Reversi -> [Tiles]
cols game = map col [0..size-1]
    where col index = S.fromList $ map (S.index tiles') [index,index+size..length tiles'-1]
          tiles' = tiles game

-- | Returns all diagonals of every size and direction
diagonals :: Reversi -> [Tiles]
diagonals game = map (diagonalTLBR colDiagSize) colsStartIndexes
                 ++ map (diagonalTLBR rowDiagSize) rowsStartIndexes
                 ++ map (diagonalTRBL colDiagSize) colsStartIndexes
                 ++ map (diagonalTRBL rowDiagSize) rowsStartIndexes
    -- You can derive these formulas by drawing out a matrix of the indexes
    -- and noticing that top-left to bottom-right (TLBR) diagonals go up
    -- by indexes of size + 1 and top-right to bottom-left (TRBL) diagonals
    -- go up by indexes of size - 1
    -- For TRBL diagonals, we have to start from the last column instead of
    -- the first column so we use: size - 1 + start
    where diagonalTLBR maxSize start = pickIndexes $ map (\i -> start + i * (size + 1)) [0..maxSize start - 1]
          diagonalTRBL maxSize start = pickIndexes $ map (\i -> size - 1 + start + i * (size - 1)) [0..maxSize start - 1]
          pickIndexes indexes = S.fromList $ map (S.index tiles') indexes
          tiles' = tiles game
          colDiagSize start = size - start
          rowDiagSize start = colDiagSize $ start `div` size
          -- Start indexes in the first row
          colsStartIndexes = [0..size-minimumSize]
          -- Start indexes in the first column
          -- No need to get the first diagonal again (so start at 1)
          rowsStartIndexes = map (*size) [1..size-minimumSize]
          -- A diagonal less than this size isn't worth checking
          minimumSize = 3

-- | Sets the given row and column index to the given piece and returns a new game
set :: (Row, Col) -> Piece -> Reversi -> Reversi
set pos piece game = game {
    tiles = S.update (_index pos) (Just piece) (tiles game)
}

-- | Gets the piece at the given row and column index
get :: (Row, Col) -> Reversi -> Maybe Piece
get pos game = S.index (tiles game) (_index pos)

-- | Returns the raw Seq index for a given row and col
_index :: (Row, Col) -> Int
_index (row, col) = row * size + col

-- | Returns the position (row, col) for a raw Seq index
_position :: Int -> (Row, Col)
_position index = index `divMod` size

format :: Reversi -> String
format game = columnRow ++ formatRows game
    where
        columnRow = (intercalate sep $ cell " " : (map (\c -> cell [c]) $ take size ['A'..'Z'])) ++ sep ++ "\n" ++ divider
        formatRows game' = concatMap formatRow $ zip [0..size] $ rows game'
        formatRow (i, row) = (intercalate sep $ rowNumber i : (map (cell . formatTile i) (zip [0..] $ toList row))) ++ sep ++ "\n" ++ divider
        formatTile rowIndex (colIndex, Nothing) = if elem (rowIndex, colIndex) valid then _color Yellow "\x25CB" else " "
        formatTile _ (_, Just piece) = show piece
        cell content = " " ++ content ++ " "
        cellWidth = 3
        rowNumber index = cell $ show $ succ index
        sep = "\x2502"
        -- the succ and +1 are because of the extra row number column
        divider = (take (succ size * cellWidth + size + 1) $ repeat '\x2500') ++ "\n"
        valid = validMoves game

