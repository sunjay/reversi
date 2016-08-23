module Reversi.AI (AI, negamax, random, eager) where

import System.Random (StdGen)
import Data.Foldable (maximumBy)

import qualified System.Random as Rand

import Reversi (Reversi, Row, Col, Piece(PieceX))

import qualified Reversi as R

-- | Type representing a general AI function
-- | If there are no valid moves, AI should return bottom (error)
type AI = StdGen -> Reversi -> (Row, Col)

-- | A random AI
random :: AI
random gen game = valid !! index
    where index = fst $ Rand.randomR (0, length valid - 1) gen
          valid = R.validMoves game

-- | An AI that is so eager to pick a move, it just returns the first one
eager :: AI
eager _ game = head $ R.validMoves game

-- | Gets a move that the currentPiece should make for the given situation
negamax :: AI
negamax _ game = maximumBy compareMove $ R.validMoves game
    where scoreMove move = score (R.currentPiece game) $ R.move move game
          compareMove m1 m2 = compare (scoreMove m1) (scoreMove m2)

-- | Scores the game for the given player
score :: Piece -> Reversi -> Integer
score piece game = playerScore gameScores - (opponentScore gameScores)
    where (playerScore, opponentScore) =
              if piece == PieceX
                  then (fst, snd)
                  else (snd, fst)
          gameScores = R.scores game

