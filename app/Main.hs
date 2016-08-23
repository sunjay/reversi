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

import Reversi (Reversi, Row, Col, Piece(PieceX, PieceO))
import qualified Reversi as R

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn $ R.format R.new
    play R.new

play :: Reversi -> IO ()
play game = do
    -- Print the current game status
    let (scoreX, scoreO) = R.score game
    putStrLn $ "Score: " ++ (show PieceX) ++ " " ++ (show scoreX) ++ " | " ++ (show PieceO) ++ " " ++ (show scoreO)
    putStrLn $ "The current piece is: " ++ (show $ R.currentPiece game)

    !maybeMove <- getMove
    eof <- isEOF
    if eof then do
        -- Put an extra line to make sure the terminal's prompt is well aligned
        putStrLn ""
        return ()
    else do
        case makeMove game maybeMove of
            Left error' -> do
                putStrLn $ R.format game

                setSGR [SetColor Foreground Vivid Red]
                putStrLn error'
                setSGR []
                play game

            Right game' -> do
                if null $! R.validMoves game' then do
                    putStrLn ""
                    return ()
                else do
                    play game'

getMove :: IO (Maybe (Row, Col))
getMove = do
    !line <- prompt "Enter your move (e.g. A1): "
    putStrLn ""

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

makeMove :: Reversi -> Maybe (Row, Col) -> Either String Reversi
makeMove _ Nothing = Left "Invalid move format. Enter something like '1A'."
makeMove game (Just move) =
    if elem move $ R.validMoves game then
        Right $ R.move move game
    else
        Left "Invalid move. Your move must flip at least one tile."

prompt :: String -> IO String
prompt message = do
    putStr message
    getLine
