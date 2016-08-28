module Reversi.AI.Negamax (negamax) where

import Data.Foldable (maximumBy)

-- Lib imports
import Reversi (Reversi, Row, Col, Piece(PieceX))

import qualified Reversi as R

-- Reversi.AI imports
import Reversi.AI.Common (AI)

import qualified Reversi.AI.GameTree as GT

-- | Gets a move that the currentPiece should make for the given situation
negamax :: AI
negamax _ game = snd $ negamax' targetDepth (R.currentPiece game) (-1, -1) game
    where targetDepth = 5 -- how many moves deep to think

negamax' :: Int -> Piece -> (Row, Col) -> Reversi -> (Integer, (Row, Col))
negamax' depth player game
    | depth > 0 = maximumBy compareMoves $ (-1000000, (-1, -1)) : (map search valid)
    | otherwise = score player game
    where search move = (deepScore move (R.move move game), move)
          deepScore move game' = maybeNegate game' $ fst $ negamax' (pred depth) player move game'
          compareMoves (s1, _) (s2, _) = compare s1 s2
          maybeNegate game' score' = if R.currentPiece game' == player then score' else (negate score')
          valid = R.validMoves game

-- | Scores the game for the given player
score :: Piece -> Reversi -> Integer
score piece game = playerScore gameScores - (opponentScore gameScores)
    where (playerScore, opponentScore) =
              if piece == PieceX
                  then (fst, snd)
                  else (snd, fst)
          gameScores = R.scores game

