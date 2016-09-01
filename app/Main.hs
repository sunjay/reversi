{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Char (toUpper)
import Data.List (elemIndex, sort)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Concurrent (threadDelay)

import qualified System.Random as Rand

import qualified Reversi as R
import qualified Reversi.AI as AI

import Play (GetMove, play)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play getHumanMove (getAIMove $ AI.negamax 5) R.new

getAIMove :: AI.AI -> GetMove
getAIMove ai game = do
    putStr "AI is thinking..."
    gen <- Rand.newStdGen
    threadDelay 1000000 -- 1 second
    -- to complete the line before and an additional one for formatting
    let !move = ai gen game
    putStrLn "\n"
    return $ Just $ move

getHumanMove :: GetMove
getHumanMove _ = do
    !line <- prompt "Enter your move (e.g. A1): "
    putStrLn ""

    if length line /= 2 then do
        return Nothing
    else do
        -- normalize line
        let line' = reverse $ sort line
        -- Note that the row number is not the same as the row index!
        let maybeRow = readMaybe (drop 1 line') :: Maybe R.Row
            maybeCol = elemIndex (toUpper (head line')) validColumns

        if isNothing maybeRow || (isNothing maybeCol) then do
            return Nothing
        else do
            return $ Just (pred $ fromJust maybeRow, fromJust maybeCol)

prompt :: String -> IO String
prompt message = do
    putStr message
    getLine

validColumns :: [Char]
validColumns = take R.size ['A'..'Z']

