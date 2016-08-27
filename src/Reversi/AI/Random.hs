module Reversi.AI.Random (random) where

import qualified System.Random as Rand
import qualified Reversi as R

import Reversi.AI.Common (AI)

-- | A random AI
random :: AI
random gen game = valid !! index
    where index = fst $ Rand.randomR (0, length valid - 1) gen
          valid = R.validMoves game

