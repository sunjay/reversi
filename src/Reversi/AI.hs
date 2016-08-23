module Reversi.AI (AI, negamax, random, eager) where

import System.Random (StdGen)

import qualified System.Random as Rand

import Reversi

-- | Type representing a general AI function
-- | If there are no valid moves, AI should return bottom (error)
type AI = StdGen -> Reversi -> (Row, Col)

-- | Gets a move that the currentPiece should make for the given situation
negamax :: AI
negamax = eager

-- | A random AI
random :: AI
random gen game = valid !! index
    where index = fst $ Rand.randomR (0, length valid - 1) gen
          valid = validMoves game

-- | An AI that is so eager to pick a move, it just returns the first one
eager :: AI
eager _ game = head $ validMoves game
