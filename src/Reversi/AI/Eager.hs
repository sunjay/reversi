module Reversi.AI.Eager (eager) where

import qualified Reversi as R

import Reversi.AI.Common (AI)

-- | An AI that is so eager to pick a move, it just returns the first one
eager :: AI
eager _ = head . R.validMoves

