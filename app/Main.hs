{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (elemIndex)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import qualified System.Random as Rand

import qualified Reversi as R
import qualified Reversi.AI as AI

import Play (GetMove, play)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play getHumanMove getAIMove R.new

getAIMove :: GetMove
getAIMove game = do
    putStrLn "AI is thinking...\n"
    gen <- Rand.newStdGen
    return $ Just $ AI.random gen game

getHumanMove :: GetMove
getHumanMove _ = do
    !line <- prompt "Enter your move (e.g. A1): "
    putStrLn ""

    if length line /= 2 then do
        return Nothing
    else do
        -- Note that the row number is not the same as the row index!
        let maybeRow = readMaybe (drop 1 line) :: Maybe R.Row
            maybeCol = elemIndex (head line) validColumns

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

