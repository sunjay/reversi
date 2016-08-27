module Reversi.AI.Common where

import System.Random (StdGen)
import Reversi (Reversi, Row, Col)

-- | Type representing a general AI function
-- | If there are no valid moves, AI should return bottom (error)
type AI = StdGen -> Reversi -> (Row, Col)

