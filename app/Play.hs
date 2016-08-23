{-# LANGUAGE BangPatterns #-}

module Play (GetMove, play) where

import System.Console.ANSI (
    setSGR,
    SGR(SetColor),
    ConsoleLayer(Foreground),
    ColorIntensity(Vivid),
    Color(Red))

import Reversi (Reversi, Row, Col, Piece(PieceX, PieceO))
import qualified Reversi as R

type GetMove = Reversi -> IO (Maybe (Row, Col))

play :: GetMove -> GetMove -> Reversi -> IO ()
play getXMove getOMove game = do
    putStrLn $ R.format game
    putStrLn ""
    loop getXMove getOMove game

loop :: GetMove -> GetMove -> Reversi -> IO ()
loop getXMove getOMove game = do
    -- Print the current game status
    let (scoreX, scoreO) = R.score game
    putStrLn $ "Score: " ++ (show PieceX) ++ " " ++ (show scoreX) ++ " | " ++ (show PieceO) ++ " " ++ (show scoreO)
    putStrLn $ "The current piece is: " ++ (show $ R.currentPiece game)

    if null $ R.validMoves game then do
        putStrLn ""
        return ()
    else do
        let getMove = if R.currentPiece game == PieceX then getXMove else getOMove
        !maybeMove <- getMove game
        case makeMove game maybeMove of
            Left error' -> do
                putStrLn $ R.format game

                setSGR [SetColor Foreground Vivid Red]
                putStrLn error'
                setSGR []
                loop getXMove getOMove game

            Right game' -> do
                putStrLn $ R.format game'
                -- This empty line is placed where the error line would go
                putStrLn ""
                loop getXMove getOMove game'

makeMove :: Reversi -> Maybe (Row, Col) -> Either String Reversi
makeMove _ Nothing = Left "Invalid move format. Enter something like 'A1'."
makeMove game (Just move) =
    if elem move $ R.validMoves game then
        Right $ R.move move game
    else
        Left "Invalid move. Your move must flip at least one tile."

