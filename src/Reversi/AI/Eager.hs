module Reversi.AI.Eager (eager) where

import Reversi (ValidMoves(SkipTurn, ValidMoves))

import qualified Reversi as R

import Reversi.AI.Common (AI)

-- | An AI that is so eager to pick a move, it just returns the first one
eager :: AI
eager _ game = head $ case R.validMoves game of
    SkipTurn -> error "This should never occur"
    ValidMoves moves -> moves

