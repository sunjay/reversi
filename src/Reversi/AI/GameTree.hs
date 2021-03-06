module Reversi.AI.GameTree where

import Reversi (Reversi, ValidMoves(SkipTurn, ValidMoves))
import qualified Reversi as R

-- | Represents a single node in a negamax tree
-- | Takes the move that led into this node and a list of children
-- | Each child represents a valid
data GameTreeNode = GameTreeNode {
    game :: Reversi,
    children :: [GameTreeNode]
}

-- | Generates the game tree starting from the children of the given game
-- | Each child is a valid path (move) to make from the node's game
-- | This will generate the tree to the end of its depth, so limiting
-- | depth must be done separately
-- | Thanks to Haskell's lazy evaluation, this doesn't actually generate
-- | any nodes that you don't use
gameTree :: Reversi -> GameTreeNode
gameTree game' = GameTreeNode {
    game = game',
    children = case R.validMoves game' of
        SkipTurn -> [gameTree $ R.skipTurn game']
        ValidMoves moves -> map (gameTree . (flip R.move) game') moves
}

formatGameTree :: Int -> GameTreeNode -> String
formatGameTree depth tree = formatGameTree' depth depth tree

formatGameTree' :: Int -> Int -> GameTreeNode -> String
formatGameTree' originalDepth depth tree =
        indent ++ currentLevel ++ "\n" ++ formattedChildren
    where currentLevel = applyIndent $ R.format game'
          game' = game tree
          applyIndent text = concatMap (\c -> if c == '\n' then '\n' : indent else [c]) text
          indent = concat $ replicate (originalDepth - depth) "|    "
          formattedChildren = if depth == 1 || (null $ children tree) then "" else concatMap (formatGameTree' originalDepth (pred depth)) $ children tree

