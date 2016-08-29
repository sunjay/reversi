module Reversi.AI.Negamax (negamax) where

import Data.Maybe (fromJust)
import Data.Foldable (maximumBy)

-- Lib imports
import Reversi (Reversi, Row, Col, Piece(PieceX))

import qualified Reversi as R

-- Reversi.AI imports
import Reversi.AI.Common (AI)
import Reversi.AI.GameTree (GameTreeNode, gameTree)

import qualified Reversi.AI.GameTree as GT

-- | Gets a move that the currentPiece should make for the given situation
negamax :: AI
negamax _ game = fromJust $ snd $ negamax' targetDepth (R.currentPiece game) $ gameTree game
    where targetDepth = 5 -- how many moves deep to think

negamax' :: Int -> Piece -> GameTreeNode -> (Integer, Maybe (Row, Col))
negamax' depth player tree
    | (depth == 0) || (null children) = (sign * (score player game), R.lastMove game)
    | otherwise = maximumBy compareMoves $ map search children
    where sign = if R.currentPiece game == player then 1 else -1
          game = GT.game tree
          search child = (negate $ fst $ negamax' (pred depth) player child, R.lastMove $ GT.game child)
          compareMoves (s1, _) (s2, _) = compare s1 s2
          children = GT.children tree

-- | Scores the game for the given player
score :: Piece -> Reversi -> Integer
score piece game = playerScore gameScores - (opponentScore gameScores)
    where (playerScore, opponentScore) =
              if piece == PieceX
                  then (fst, snd)
                  else (snd, fst)
          gameScores = R.scores game

