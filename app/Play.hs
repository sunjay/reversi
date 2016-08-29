{-# LANGUAGE BangPatterns, MultiWayIf #-}

module Play (GetMove, play) where

import System.IO (stdout)
import System.Console.ANSI (
    hSupportsANSI,
    setSGRCode,
    SGR(SetColor),
    ConsoleLayer(Foreground),
    ColorIntensity(Vivid),
    Color(Red, Blue, Yellow))

import Reversi (Reversi, Row, Col, Piece(PieceX, PieceO), ValidMoves(SkipTurn, ValidMoves))
import qualified Reversi as R

type GetMove = Reversi -> IO (Maybe (Row, Col))

play :: GetMove -> GetMove -> Reversi -> IO ()
play getXMove getOMove game = do
    putGame game
    putStrLn ""
    loop getXMove getOMove game

--TODO: Refactor this to accept a parameter record with the two get move functions and supported
loop :: GetMove -> GetMove -> Reversi -> IO ()
loop getXMove getOMove game = do
    supported <- supportsANSI

    -- Print the current game status
    putScores (R.scores game) supported

    putLastMove $ R.lastMove game

    case R.validMoves game of
        SkipTurn -> skipTurn getXMove getOMove game supported
        ValidMoves moves -> maybeContinueGame getXMove getOMove game moves supported

skipTurn :: GetMove -> GetMove -> Reversi -> Bool -> IO ()
skipTurn getXMove getOMove game supported = do
    putCurrentPiece (R.currentPiece game) supported
    putStr "No valid moves, skipping turn. Press enter to continue... "
    !_ <- getLine

    let game' = R.skipTurn game
    putGame game'
    -- This empty line is placed where the error line would go
    putStrLn ""

    loop getXMove getOMove game'

maybeContinueGame :: GetMove -> GetMove -> Reversi -> [(Row, Col)] -> Bool -> IO ()
maybeContinueGame getXMove getOMove game validMoves supported =
    if null $ validMoves then do
        putWinner (R.scores game)
        return ()

    else do
        putCurrentPiece (R.currentPiece game) supported

        let getMove = case R.currentPiece game of
                PieceX -> getXMove
                PieceO -> getOMove

        !maybeMove <- getMove game
        attemptMove getXMove getOMove game maybeMove supported

attemptMove :: GetMove -> GetMove -> Reversi -> Maybe (Row, Col) -> Bool -> IO ()
attemptMove getXMove getOMove game maybeMove supported =
    case makeMove game maybeMove of
        Left error' -> do
            putGame game
            putError error' supported

            loop getXMove getOMove game

        Right game' -> do
            putGame game'
            -- This empty line is placed where the error line would go
            putStrLn ""

            loop getXMove getOMove game'

putError :: String -> Bool -> IO ()
putError error' supported =
    if supported then
        putStrLn $ _color Red error'
    else
        putStrLn error'


putCurrentPiece :: Piece -> Bool -> IO ()
putCurrentPiece piece supported = do
    putStr "The current piece is: "
    let formattedPieceX = if supported then pieceX else (show PieceX)
        formattedPieceO = if supported then pieceO else (show PieceO)

    putStrLn $ case piece of
        PieceX -> formattedPieceX
        PieceO -> formattedPieceO

putLastMove :: Maybe (Row, Col) -> IO ()
putLastMove lastMove =
    putStrLn $ case lastMove of
        Nothing -> "Let the game begin!"
        Just move -> "Last move: " ++ (formatMove move)

putWinner :: (Integer, Integer) -> IO ()
putWinner (scoreX, scoreO) = do
    putStr "The winner is: "
    case compare scoreX scoreO of
        LT -> putStrLn $ show PieceO
        GT -> putStrLn $ show PieceX
        EQ -> putStrLn $ "Tie"
    -- This line is where the user usually enters a move
    putStrLn ""

putScores :: (Integer, Integer) -> Bool -> IO ()
putScores (scoreX, scoreO) supported =
    if supported then
        putStrLn $ "Score: " ++ pieceX ++ " " ++ (show scoreX) ++ " | " ++ pieceO ++ " " ++ (show scoreO)
    else
        putStrLn $ "Score: " ++ (show PieceX) ++ " " ++ (show scoreX) ++ " | " ++ (show PieceO) ++ " " ++ (show scoreO)

putGame :: Reversi -> IO ()
putGame game = do
    supported <- supportsANSI
    if supported then do
        putStrLn $ concatMap replacer formatted
    else do
        putStrLn formatted
    where formatted = R.format game
          replacer :: Char -> String
          replacer c =
            if | c == (head $ show PieceX) -> pieceX
               | c == (head $ show PieceO) -> pieceO
               | c == (head $ R.validMoveMarker) -> _color Yellow "\x25CB"
               | otherwise -> [c]

_color :: Color -> String -> String
_color color text = setSGRCode [SetColor Foreground Vivid color] ++ text ++ (setSGRCode [])

supportsANSI :: IO Bool
supportsANSI = hSupportsANSI stdout

pieceX :: String
pieceX = _color Red "\x25CF"
pieceO :: String
pieceO = _color Blue "\x25CF"

formatMove :: (Row, Col) -> String
formatMove (row, col) = (['A'..'Z'] !! col) : (show $ succ row)

makeMove :: Reversi -> Maybe (Row, Col) -> Either String Reversi
makeMove _ Nothing = Left "Invalid move format. Enter something like 'A1'."
makeMove game (Just move) =
    if elem move valid then
        Right $ R.move move game
    else
        Left "Invalid move. Your move must flip at least one tile."
    where valid = case R.validMoves game of
              SkipTurn -> []
              ValidMoves moves -> moves

