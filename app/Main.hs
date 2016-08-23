{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (elemIndex)
import Data.Maybe (isNothing, fromJust)
import Text.Read (readMaybe)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering), isEOF)

import System.Console.ANSI (
    setSGR,
    SGR(SetColor),
    ConsoleLayer(Foreground),
    ColorIntensity(Vivid),
    Color(Red))

import Reversi (Reversi, Row, Col)
import qualified Reversi as R

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play R.new

play :: Reversi -> IO ()
play game = do
    putStrLn $ R.format game

    putStrLn $ "The current piece is: " ++ (show $ R.currentPiece game)
    maybeMove <- getMove
    eof <- isEOF
    if eof then do
        -- Put an extra line to make sure the terminal's prompt is well aligned
        putStrLn ""
        return ()
    else do
        game' <- makeMove game maybeMove
        play game'

getMove :: IO (Maybe (Row, Col))
getMove = do
    putStr "Enter your move (e.g. A1): "
    line <- getLine
    if length line /= 2 then do
        return Nothing
    else do
        -- Note that the row number is not the same as the row index!
        let maybeRow = readMaybe (take 1 line) :: Maybe Row
            maybeCol = elemIndex (last line) validColumns

        if isNothing maybeRow || (isNothing maybeCol) then do
            return Nothing
        else do
            return $ Just (pred $ fromJust maybeRow, fromJust maybeCol)

validColumns :: [Char]
validColumns = take R.size ['A'..'Z']

makeMove :: Reversi -> Maybe (Row, Col) -> IO Reversi
makeMove game Nothing = do
    putStrLn "Invalid move format (enter something like '1A')"
    return game
makeMove game (Just move) = do
    if elem move $ R.validMoves game then do
        return $ R.move move game
    else do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Invalid move. Your move must flip at least one tile."
        setSGR []
        return game

