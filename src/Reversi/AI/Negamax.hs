{-# LANGUAGE BangPatterns #-}

module Reversi.AI.Negamax (negamax) where

import Data.Maybe (fromJust)

import System.Random (StdGen)

import qualified System.Random as Rand

-- Lib imports
import Reversi (Reversi, Row, Col, Piece(PieceX, PieceO))

import qualified Reversi as R

-- Reversi.AI imports
import Reversi.AI.Common (AI)
import Reversi.AI.GameTree (GameTreeNode, gameTree)

import qualified Reversi.AI.GameTree as GT

-- | Gets a move that the currentPiece should make for the given situation
negamax :: AI
negamax rng game = fromJust $ snd $ fst $ negamax' rng targetDepth (R.currentPiece game) $ gameTree game
    where targetDepth = 5 -- how many moves deep to think

negamax' :: StdGen -> Int -> Piece -> GameTreeNode -> ((Integer, Maybe (Row, Col)), StdGen)
negamax' rng depth player tree
    | (depth == 0) || (null children) = ((sign * (score player game), R.lastMove game), rng)
    | otherwise = (options !! picked, rng'')
    where (picked, rng'') = Rand.randomR (0, pred $ length options) rng'
          options = filter (\(s, _) -> s == maxScore) childScores
          maxScore = maximum $ map fst childScores
          (childScores, rng') = foldl search ([], rng) children
          sign = if R.currentPiece game == player then 1 else -1
          game = GT.game tree
          search (scores, crng) child = (deepScore : scores, crng')
              where deepScore = (negate $ fst deeper, R.lastMove $ GT.game child)
                    (deeper, crng') = negamax' crng (pred depth) player child
          children = GT.children tree

-- | Scores the game for the given player
score :: Piece -> Reversi -> Integer
-- The multipliers are the weights of each scoring criteria
-- Corners are worth more than side which are worth more than anything else
-- Note that corners and sides are already counted in deltaScore
-- So the additional terms in the equation are piling onto that weight
score piece game = deltaScore + 2 * deltaSides + 4 * deltaCorners
    where deltaScore = player gameScores - (opponent gameScores)
          deltaSides = player sidesCount - (opponent sidesCount)
          deltaCorners = player cornersCount - (opponent cornersCount)

          sidesCount = countSides game
          cornersCount = countCorners game
          gameScores = R.scores game
          (player, opponent) =
              if piece == PieceX
                  then (fst, snd)
                  else (snd, fst)

-- | Returns (side pieces with PieceX, side pieces with PieceO)
countSides :: Reversi -> (Integer, Integer)
countSides game = foldl counter (0, 0) $ R.sides game

-- | Returns (corner pieces with PieceX, corner pieces with PieceO)
countCorners :: Reversi -> (Integer, Integer)
countCorners game = foldl counter (0, 0) $ R.corners game

counter :: (Integer, Integer) -> (Row, Col, Piece) -> (Integer, Integer)
counter (!x, !o) (_, _, PieceX) = (succ x, o)
counter (!x, !o) (_, _, PieceO) = (x, succ o)

