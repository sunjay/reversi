{-# LANGUAGE BangPatterns #-}

module Reversi (
    Row, Col,
    Piece(PieceX, PieceO),
    ValidMoves(SkipTurn, ValidMoves),
    validMoveMarker,
    Tiles,
    Reversi,
    tiles,
    lastMove,
    currentPiece,
    size,
    other,
    skipTurn,
    empty,
    new,
    scores,
    move,
    validMoves,
    set,
    get,
    format
) where

import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Maybe (isNothing, mapMaybe)

-- Row and Col indexes start at 0
type Row = Int
type Col = Int

data Piece = PieceX | PieceO deriving (Eq)

instance Show Piece where
    show PieceX = "x"
    show PieceO = "o"

data ValidMoves = SkipTurn | ValidMoves [(Row, Col)]

validMoveMarker :: String
validMoveMarker = "*"

type Tiles = Seq (Maybe Piece)

data Reversi = Reversi {
    tiles :: Tiles,
    lastMove :: Maybe (Row, Col),
    currentPiece :: Piece
}

-- The horizontal and vertical dimension of the board
size :: Int
size = 8

-- | Returns the opposite piece from the given piece
other :: Piece -> Piece
other PieceX = PieceO
other PieceO = PieceX

-- | Returns whether the given position is valid on the board
isValidPos :: (Row, Col) -> Bool
isValidPos (row, col) = (isValid row) && (isValid col)
    where isValid x = x >= 0 && x < size

-- | Skips the turn of the current piece and returns the same game
-- | with the other piece as the current piece
skipTurn :: Reversi -> Reversi
skipTurn game = game {
    currentPiece = other $ currentPiece game,
    lastMove = Nothing
}

-- | Returns a completely empty game board
empty :: Reversi
empty = Reversi {
    tiles = S.fromList $ replicate (size * size) Nothing,
    lastMove = Nothing,
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

-- | Gets the score for both pieces (PieceX score, PieceO score)
scores :: Reversi -> (Integer, Integer)
scores game = foldl counter (0, 0) $ tiles game
    where counter acc Nothing = acc
          counter (!x, !o) (Just PieceX) = (succ x, o)
          counter (!x, !o) (Just PieceO) = (x, succ o)

-- | Attempts to place the current piece at the given position, flip any
-- | surrounding pieces and then return an updated board
-- | Returns bottom (error) if no tiles were flipped
move :: (Row, Col) -> Reversi -> Reversi
move (row, col) game
    | not $ null pendingFlips = updatedGame {currentPiece = target, lastMove = Just (row, col)}
    | otherwise = error "Invalid move: Resulted in no flips"
    where updatedGame = set (row, col) piece flippedGame
          flippedGame = foldr flipPiece game pendingFlips
          pendingFlips = concatMap (flippablePositions (row, col) []) _directions
          flippablePositions (crow, ccol) fpositions (drow, dcol)
              | not $ isValidPos next = []
              | nextPiece == Just target =
                  flippablePositions next (next : fpositions) (drow, dcol)
              | nextPiece == Just piece = fpositions
              | otherwise = []
              where next = (crow + drow, ccol + dcol)
                    nextPiece = get next game
          piece = currentPiece game
          target = other piece

-- | Returns the valid moves available for a given game's current piece
-- | If there are no moves leftover for that piece but the next piece can
-- | still proceed, returns SkipTurn
validMoves :: Reversi -> ValidMoves
validMoves game = if null valid && (not $ null otherValidMoves) then SkipTurn else (ValidMoves valid)
    where valid = validMoves' game
          otherValidMoves = validMoves' $ skipTurn game

-- | Returns all the valid moves that can be played
validMoves' :: Reversi -> [(Row, Col)]
validMoves' game = mapMaybe (uncurry findValid) searchSpace
    -- Find the position of each piece that is the same as currentPiece
    -- Go in each direction while the other piece is being found
    -- If at least one other piece is found, this is a valid move
    where searchSpace = [((r, c), d) | (r, c, _) <- searchPieces, d <- _directions]
          findValid = searchDirection 0
          searchDirection count (row, col) (drow, dcol)
              | not $ isValidPos next = Nothing
              | isNothing nextPiece =
                  if count > 0 then Just next else Nothing
              | nextPiece == Just target =
                  searchDirection (succ count) next (drow, dcol)
              | otherwise = Nothing
              where next = (row + drow, col + dcol)
                    nextPiece = get next game

          searchPieces = filter isPiece $ positions game
          isPiece (_, _, p) = p == piece
          piece = currentPiece game
          target = other piece

-- All 8 directions
_directions :: [(Row, Col)]
_directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

-- | Returns the position of every piece (ignores empty tiles)
positions :: Reversi -> [(Row, Col, Piece)]
positions = mapMaybe position . (zip [0..]) . toList . tiles
    where position (_, Nothing) = Nothing
          position (i, Just piece) = Just (row, col, piece)
              where (row, col) = _position i

-- | Returns all the rows of the board
rows :: Reversi -> [Tiles]
rows game = map row [0..size-1]
    where row i = S.take size $ S.drop (i * size) (tiles game)

-- | Flips the piece at the given position, error if no piece is there
flipPiece :: (Row, Col) -> Reversi -> Reversi
flipPiece pos game = maybe (error "No piece to flip") flipper piece
    where piece = get pos game
          flipper p = set pos (other p) game

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
        formatTile rowIndex (colIndex, Nothing) = if elem (rowIndex, colIndex) valid then validMoveMarker else " "
        formatTile _ (_, Just piece) = show piece
        cell content = " " ++ content ++ " "
        cellWidth = 3
        rowNumber index = cell $ show $ succ index
        sep = "\x2502"
        -- the succ and +1 are because of the extra row number column
        divider = replicate (succ size * cellWidth + size + 1) '\x2500' ++ "\n"
        valid = case validMoves game of
            SkipTurn -> []
            ValidMoves moves -> moves

