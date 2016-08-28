{-# LANGUAGE BangPatterns, MultiWayIf #-}

module Play (GetMove, play) where

import Data.Maybe (isJust, fromJust)

import System.Console.ANSI (
    setSGR,
    setSGRCode,
    SGR(SetColor),
    ConsoleLayer(Foreground),
    ColorIntensity(Vivid),
    Color(Red, Blue, Yellow))

import Reversi (Reversi, Row, Col, Piece(PieceX, PieceO))
import qualified Reversi as R

type GetMove = Reversi -> IO (Maybe (Row, Col))

play :: GetMove -> GetMove -> Reversi -> IO ()
play getXMove getOMove game = do
    putGame game
    putStrLn ""
    loop getXMove getOMove game

loop :: GetMove -> GetMove -> Reversi -> IO ()
loop getXMove getOMove game = do
    -- Print the current game status
    let (scoreX, scoreO) = R.scores game
    putStrLn $ "Score: " ++ pieceX ++ " " ++ (show scoreX) ++ " | " ++ pieceO ++ " " ++ (show scoreO)

    if isJust $ R.lastMove game then
        putStrLn $ "Last move: " ++ (formatMove $ fromJust $ R.lastMove game)
    else
        putStrLn "Let the game begin!"

    if null $ R.validMoves game then do
        putStr "The winner is: "
        case compare scoreX scoreO of
            LT -> putStrLn $ show PieceO
            GT -> putStrLn $ show PieceX
            EQ -> putStrLn $ "Tie"
        putStrLn ""
        return ()
    else do
        putStrLn $ "The current piece is: " ++ (if R.currentPiece game == PieceX then pieceX else pieceO)

        let getMove = if R.currentPiece game == PieceX then getXMove else getOMove
        !maybeMove <- getMove game
        case makeMove game maybeMove of
            Left error' -> do
                putGame game

                setSGR [SetColor Foreground Vivid Red]
                putStrLn error'
                setSGR []
                loop getXMove getOMove game

            Right game' -> do
                putGame game'
                -- This empty line is placed where the error line would go
                putStrLn ""
                loop getXMove getOMove game'

putGame :: Reversi -> IO ()
putGame game = putStrLn $ concatMap replacer $ R.format game
    where replacer :: Char -> String
          replacer c =
            if | c == (head $ show PieceX) -> pieceX
               | c == (head $ show PieceO) -> pieceO
               | c == (head $ R.validMoveMarker) -> _color Yellow "\x25CB"
               | otherwise -> [c]

_color :: Color -> [Char] -> [Char]
_color color text = setSGRCode [SetColor Foreground Vivid color] ++ text ++ (setSGRCode [])

pieceX :: String
pieceX = _color Red "\x25CF"
pieceO :: String
pieceO = _color Blue "\x25CF"

formatMove :: (Row, Col) -> String
formatMove (row, col) = (['A'..'Z'] !! col) : (show $ succ row)

makeMove :: Reversi -> Maybe (Row, Col) -> Either String Reversi
makeMove _ Nothing = Left "Invalid move format. Enter something like 'A1'."
makeMove game (Just move) =
    if elem move $ R.validMoves game then
        Right $ R.move move game
    else
        Left "Invalid move. Your move must flip at least one tile."

