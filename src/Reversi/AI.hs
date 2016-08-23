module Reversi.AI (negamax) where

import Reversi

-- | Gets a move that the currentPiece should make for the given situation
-- | If there are no valid moves, returns bottom (error)
negamax :: Reversi -> (Row, Col)
negamax game = head $ validMoves game

