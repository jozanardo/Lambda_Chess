module Main where
import Data.Maybe (isNothing)
import Data.Char (toLower)
import System.Random
import Text.Read (readMaybe)
import Data.Maybe (isJust)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving Show
data Color = White | Black deriving (Eq, Show)
data Position = Position Int Int deriving (Eq, Show)
data ChessPiece = ChessPiece { piece :: Piece, color :: Color } deriving Show
data GameResult = Ongoing | Checkmate Color | Stalemate | Draw deriving (Eq, Show)
type Board = [[Maybe ChessPiece]]

emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing)

placePiece :: Position -> Maybe ChessPiece -> Board -> Board
placePiece (Position x y) maybePiece board =
    take y board ++ [take x (board !! y) ++ maybePiece : drop (x + 1) (board !! y)] ++ drop (y + 1) board

movePiece :: Position -> Position -> Board -> Maybe Board
movePiece fromPos toPos board =
    let maybePiece = getPieceAtPosition fromPos board
        newBoard = placePiece fromPos Nothing board
    in case maybePiece of
        Just (ChessPiece King color) ->
            if isKingsideCastle fromPos toPos color board
                then Just (castleKingside color newBoard)
            else if isQueensideCastle fromPos toPos color board
                then Just (castleQueenside color newBoard)
            else if isValidKingMove fromPos toPos board
                then Just (placePiece toPos (Just (ChessPiece King color)) newBoard)
            else Nothing
        Just (ChessPiece Pawn color) ->
            if isValidPawnMove fromPos toPos color board
                then Just (placePiece toPos (Just (ChessPiece Pawn color)) newBoard)
            else Nothing
        Just (ChessPiece Knight _) ->
            if isValidKnightMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard)
            else Nothing
        Just (ChessPiece Bishop _) ->
            if isValidBishopMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard)
            else Nothing
        Just (ChessPiece Rook _) ->
            if isValidRookMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard)
            else Nothing
        Just (ChessPiece Queen _) ->
            if isValidQueenMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard)
            else Nothing
        Nothing -> Nothing


isKingsideCastle :: Position -> Position -> Color -> Board -> Bool
isKingsideCastle fromPos toPos color board =
    let kingPos = if color == White then Position 4 0 else Position 4 7
    in fromPos == kingPos && toPos == (if color == White then Position 6 0 else Position 6 7)

isQueensideCastle :: Position -> Position -> Color -> Board -> Bool
isQueensideCastle fromPos toPos color board =
    let kingPos = if color == White then Position 4 0 else Position 4 7
    in fromPos == kingPos && toPos == (if color == White then Position 2 0 else Position 2 7)

castleKingside :: Color -> Board -> Board
castleKingside color board =
    let kingPos = if color == White then Position 4 0 else Position 4 7
        newKingPos = if color == White then Position 6 0 else Position 6 7
        newRookPos = if color == White then Position 5 0 else Position 5 7
    in
        if canCastleKingside color board
            then placePiece kingPos Nothing
                 (placePiece newKingPos (Just (ChessPiece King color))
                 (placePiece newRookPos (Just (ChessPiece Rook color))
                 (placePiece (if color == White then Position 7 0 else Position 7 7) Nothing board)))
            else board

castleQueenside :: Color -> Board -> Board
castleQueenside color board =
    let kingPos = if color == White then Position 4 0 else Position 4 7
        newKingPos = if color == White then Position 2 0 else Position 2 7
        newRookPos = if color == White then Position 3 0 else Position 3 7
    in
        if canCastleQueenside color board
            then placePiece kingPos Nothing
                 (placePiece newKingPos (Just (ChessPiece King color))
                 (placePiece newRookPos (Just (ChessPiece Rook color))
                 (placePiece (if color == White then Position 0 0 else Position 0 7) Nothing board)))
            else board

canCastleKingside :: Color -> Board -> Bool
canCastleKingside color board =
    let row = if color == White then 0 else 7
    in
        case (board !! row) !! 7 of
            Just (ChessPiece Rook color') -> color' == color
            _ -> False

canCastleQueenside :: Color -> Board -> Bool
canCastleQueenside color board =
    let row = if color == White then 0 else 7
    in
        case (board !! row) !! 0 of
            Just (ChessPiece Rook color') -> color' == color
            _ -> False

getPieceAtPosition :: Position -> Board -> Maybe ChessPiece
getPieceAtPosition (Position x y) board =
    if x >= 0 && x < 8 && y >= 0 && y < 8
        then (board !! y) !! x
        else Nothing

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "    a   b   c   d   e   f   g   h"
    putStrLn " +----------------------------------+"
    mapM_ printRowWithNumbers (zip [1..8] board)
    putStrLn " +----------------------------------+"
  where
    printRowWithNumbers (rowNum, row) = do
        putStr $ show rowNum ++ " |"
        printRow row
        putStrLn ""

    printRow :: [Maybe ChessPiece] -> IO ()
    printRow [] = putStr "|"
    printRow (Nothing:xs) = putStr " . |" >> printRow xs
    printRow (Just piece:xs) = putStr (" " ++ showPiece piece ++ " |") >> printRow xs

    showPiece :: ChessPiece -> String
    showPiece (ChessPiece piece color) =
        let pieceSymbolWhite = case piece of
                Pawn -> "♙"
                Knight -> "♘"
                Bishop -> "♗"
                Rook -> "♖"
                Queen -> "♕"
                King -> "♔"
            pieceSymbolBlack = case piece of
                Pawn -> "♟"
                Knight -> "♞"
                Bishop -> "♝"
                Rook -> "♜"
                Queen -> "♛"
                King -> "♚"
        in if color == White then pieceSymbolWhite else pieceSymbolBlack

-- Função para obter a cor oposta
oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

-- Função para verificar o resultado do jogo
checkGameResult :: Board -> Color -> GameResult
checkGameResult board currentPlayer
    | isCheckmate board currentPlayer = Checkmate (oppositeColor currentPlayer)
    | isStalemate board currentPlayer = Stalemate
    | otherwise = Ongoing

-- Função para verificar se um jogador está em xeque-mate
isCheckmate :: Board -> Color -> Bool
isCheckmate board currentPlayer =
    isKingInCheck board currentPlayer &&
    not (any (\fromPos -> any (\toPos -> isValidMove fromPos toPos board) allPositions) (piecesOfColor currentPlayer board))

-- Função para verificar se um jogador está em empate por afogamento
isStalemate :: Board -> Color -> Bool
isStalemate board currentPlayer =
    not (isKingInCheck board currentPlayer) &&
    all (\fromPos -> all (\toPos -> not (isValidMove fromPos toPos board)) allPositions) (piecesOfColor currentPlayer board)

-- Função auxiliar para obter todas as posições das peças de uma determinada cor
piecesOfColor :: Color -> Board -> [Position]
piecesOfColor color board =
    [pos | pos <- allPositions, colorOfPieceAtPosition pos board == Just color]

-- Função para verificar se o rei de uma determinada cor está em xeque
isKingInCheck :: Board -> Color -> Bool
isKingInCheck board color =
    let kingPosition = findKingPosition board color
    in any (\position -> isAttackedByOpponent position color board) allPositions

-- Função para encontrar a posição do rei de uma determinada cor no tabuleiro
findKingPosition :: Board -> Color -> Position
findKingPosition board color =
    head [pos | pos <- allPositions, isKing pos]
  where
    isKing pos = case getPieceAtPosition pos board of
        Just (ChessPiece King pieceColor) -> pieceColor == color
        _ -> False

-- Função para verificar se uma posição está sendo atacada por um oponente
isAttackedByOpponent :: Position -> Color -> Board -> Bool
isAttackedByOpponent targetPos attackerColor board =
    any (\pos -> isSquareAttackedByPiece pos targetPos attackerColor board) allPositions

-- Função para verificar se um quadrado está sendo atacado por uma peça
isSquareAttackedByPiece :: Position -> Position -> Color -> Board -> Bool
isSquareAttackedByPiece piecePos targetPos color board =
    case getPieceAtPosition piecePos board of
        Just (ChessPiece pieceType pieceColor) ->
            pieceColor == color && isValidMove piecePos targetPos board
        _ -> False

-- Função principal para controlar o loop do jogo
playGame :: Color -> Board -> Bool -> IO ()
playGame currentPlayer board isIA = do
    putStrLn $ "Jogador atual: " ++ show currentPlayer
    printBoard board

    let gameResult = checkGameResult board currentPlayer
    case gameResult of
        Ongoing -> do
            if currentPlayer == Black && isIA then do
                newBoard <- makeAIMove Black board
                putStrLn "Movimento IA:"
                printBoard newBoard
                playGame White newBoard isIA
            else do
                putStrLn "Digite a posição referente a peça que você quer mover (ex., '2e'):"
                fromInput <- getLine
                let fromPos = parsePosition fromInput

                putStrLn "Digite a posição de destino para a peça (ex., '4e'):"
                toInput <- getLine
                let toPos = parsePosition toInput

                case (getPieceAtPosition fromPos board, colorOfPieceAtPosition fromPos board) of
                    (Just piece, Just pieceColor) | pieceColor == currentPlayer ->
                        case movePiece fromPos toPos board of
                            Just newBoard -> do
                                let newPlayer = if currentPlayer == White then Black else White
                                playGame newPlayer newBoard isIA
                            Nothing -> do
                                putStrLn "Movimento inválido! Tente novamente."
                                playGame currentPlayer board isIA
                    _ -> do
                        putStrLn "Movimento inválido! Tente novamente."
                        playGame currentPlayer board isIA
        Checkmate color -> putStrLn $ "Xeque-mate! O jogador " ++ show color ++ " venceu."
        Stalemate -> putStrLn "Afogamento! O jogo empatou."
        Draw -> putStrLn "Empate! O jogo empatou."

parsePosition :: String -> Position
parsePosition [row, col] = Position (colToIndex col) (read [row] - 1)

colToIndex :: Char -> Int
colToIndex col = fromEnum col - fromEnum 'a'

colorOfPieceAtPosition :: Position -> Board -> Maybe Color
colorOfPieceAtPosition pos board =
    case getPieceAtPosition pos board of
        Just piece -> Just (color piece)
        Nothing -> Nothing

initialBoard :: Board
initialBoard =
    [ [Just (ChessPiece Rook Black), Just (ChessPiece Knight Black), Just (ChessPiece Bishop Black), Just (ChessPiece Queen Black), Just (ChessPiece King Black), Just (ChessPiece Bishop Black), Just (ChessPiece Knight Black), Just (ChessPiece Rook Black)]
    , [Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black)]
    , replicate 8 Nothing
    , replicate 8 Nothing
    , replicate 8 Nothing
    , replicate 8 Nothing
    , [Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White)]
    , [Just (ChessPiece Rook White), Just (ChessPiece Knight White), Just (ChessPiece Bishop White), Just (ChessPiece Queen White), Just (ChessPiece King White), Just (ChessPiece Bishop White), Just (ChessPiece Knight White), Just (ChessPiece Rook White)]
    ]

isValidPawnMove :: Position -> Position -> Color -> Board -> Bool
isValidPawnMove (Position x1 y1) (Position x2 y2) color board =
    let deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
        pieceAtDest = getPieceAtPosition (Position x2 y2) board
    in case color of
        Black -> case pieceAtDest of
            Nothing -> deltaX == 0 && y2 - y1 == 1  -- Forward move
                     || (deltaX == 0 && y2 - y1 == 2 && y1 == 1 && isNothing (getPieceAtPosition (Position x1 (y1 + 1)) board))  -- Initial double move
                     || (deltaX == 1 && deltaY == 1 && isOpponentPiece (Position x2 y2) color board)  -- Diagonal capture
            Just _ -> deltaX == 1 && y1 - y2 == 1 && isOpponentPiece (Position x2 y2) color board
        White -> case pieceAtDest of
            Nothing -> deltaX == 0 && y1 - y2 == 1  -- Forward move
                     || (deltaX == 0 && y1 - y2 == 2 && y1 == 6 && isNothing (getPieceAtPosition (Position x1 (y1 - 1)) board))  -- Initial double move
                     || (deltaX == 1 && deltaY == 1 && isOpponentPiece (Position x2 y2) color board)  -- Diagonal capture
            Just _ -> deltaX == 1 && y1 - y2 == 1 && isOpponentPiece (Position x2 y2) color board

isValidKnightMove :: Position -> Position -> Board -> Bool
isValidKnightMove (Position x1 y1) (Position x2 y2) _ =
    let deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
    in (deltaX == 1 && deltaY == 2) || (deltaX == 2 && deltaY == 1)

isValidQueenMove :: Position -> Position -> Board -> Bool
isValidQueenMove fromPos toPos board =
    isValidRookMove fromPos toPos board || isValidBishopMove fromPos toPos board

isValidBishopMove :: Position -> Position -> Board -> Bool
isValidBishopMove fromPos toPos board =
    let deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
        positions = zip [min x1 x2 + 1 .. max x1 x2 - 1] [min y1 y2 + 1 .. max y1 y2 - 1]
        validDiagonal = deltaX == deltaY
        noObstructions = all (\(x, y) -> isNothing (getPieceAtPosition (Position x y) board)) positions
    in validDiagonal && noObstructions
  where
    Position x1 y1 = fromPos
    Position x2 y2 = toPos


isValidRookMove :: Position -> Position -> Board -> Bool
isValidRookMove fromPos toPos board =
    isValidHorizontalMove fromPos toPos board || isValidVerticalMove fromPos toPos board


isValidKingMove :: Position -> Position -> Board -> Bool
isValidKingMove fromPos@(Position x1 y1) toPos@(Position x2 y2) board
    | isKingsideCastle fromPos toPos White board = canCastleKingside White board
    | isQueensideCastle fromPos toPos White board = canCastleQueenside White board
    | isKingsideCastle fromPos toPos Black board = canCastleKingside Black board
    | isQueensideCastle fromPos toPos Black board = canCastleQueenside Black board
    | otherwise =
        let deltaX = abs (x2 - x1)
            deltaY = abs (y2 - y1)
        in deltaX <= 1 && deltaY <= 1

-- Helper functions

isValidDiagonalMove :: Position -> Position -> Board -> Bool
isValidDiagonalMove (Position x1 y1) (Position x2 y2) _ =
    let deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
    in deltaX == deltaY

isValidHorizontalMove :: Position -> Position -> Board -> Bool
isValidHorizontalMove (Position x1 y1) (Position x2 y2) board
    | y1 /= y2 = False
    | otherwise = all (\x -> isNothing (getPieceAtPosition (Position x y1) board)) positions
  where
    positions = [min x1 x2 + 1 .. max x1 x2 - 1]

isValidVerticalMove :: Position -> Position -> Board -> Bool
isValidVerticalMove (Position x1 y1) (Position x2 y2) board
    | x1 /= x2 = False
    | otherwise = all (\y -> isNothing (getPieceAtPosition (Position x1 y) board)) positions
  where
    positions = [min y1 y2 + 1 .. max y1 y2 - 1]


isOpponentPiece :: Position -> Color -> Board -> Bool
isOpponentPiece pos color board =
    case getPieceAtPosition pos board of
        Just (ChessPiece _ pieceColor) -> pieceColor /= color
        Nothing -> False

makeAIMove :: Color -> Board -> IO Board
makeAIMove aiColor board = do
    let aiPieces = filter (\pos -> colorOfPieceAtPosition pos board == Just aiColor) allPositions
    selectedPiece <- randomElement aiPieces
    let validMoves = getValidMoves selectedPiece board
    selectedMove <- randomElement validMoves
    case movePiece selectedPiece selectedMove board of
        Just newBoard -> return newBoard
        Nothing -> makeAIMove aiColor board

randomElement :: [a] -> IO a
randomElement xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

allPositions :: [Position]
allPositions = [Position x y | x <- [0..7], y <- [0..7]]

getValidMoves :: Position -> Board -> [Position]
getValidMoves fromPos board =
    [toPos | toPos <- allPositions, isValidMove fromPos toPos board]

isValidMove :: Position -> Position -> Board -> Bool
isValidMove fromPos toPos board =
    case getPieceAtPosition fromPos board of
        Just piece -> case movePiece fromPos toPos board of
            Just newBoard -> True
            Nothing -> False
        Nothing -> False


mainMenu :: IO ()
mainMenu = do
    putStrLn "Seja bem-vindo ao Lambda Chess!"
    putStrLn "Escolha uma opção:"
    putStrLn "1. Jogar contra a IA"
    putStrLn "2. Jogar contra outro jogador"
    putStrLn "3. Simular jogo entre duas IA's"
    putStrLn "4. Sair"

    choice <- getLine
    case choice of
        "1" -> playAgainstAI
        "2" -> playAgainstPlayer
        "3" -> simulateAIGame
        "4" -> putStrLn "Obrigado por jogar! Até logo."
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha uma opção válida."
            mainMenu

playAgainstAI :: IO ()
playAgainstAI = do
    putStrLn "Iniciando partida contra a IA..."
    playGame White initialBoard True

playAgainstPlayer :: IO ()
playAgainstPlayer = do
    putStrLn "Iniciando partida contra outro jogador..."
    playGame White initialBoard False


simulateAIGame :: IO ()
simulateAIGame = do
    putStrLn "Simulando jogo entre duas IA's..."
    putStrLn "Digite o número de jogadas"
    numMovesStr <- getLine
    case isInteger numMovesStr of
        True -> case readMaybe numMovesStr of
            Just numMoves -> playAIGame numMoves Black initialBoard
            Nothing -> do
                putStrLn "Erro na conversão do número de jogadas."
                simulateAIGame
        False -> do
            putStrLn "Opção inválida."
            simulateAIGame

isInteger :: String -> Bool
isInteger str = isJust (readMaybe str :: Maybe Integer)


playAIGame :: Int -> Color -> Board -> IO ()
playAIGame 0 _ board = do
    putStrLn "Limite de movimentos atingido. O jogo foi encerrado."
    printBoard board
playAIGame numMoves currentPlayer board = do
    putStrLn $ "Jogador atual: " ++ show currentPlayer
    printBoard board

    let gameResult = checkGameResult board currentPlayer
    case gameResult of
        Ongoing -> do
            newBoard <- makeAIMove currentPlayer board
            putStrLn "Movimento IA:"
            printBoard newBoard
            let newPlayer = if currentPlayer == White then Black else White
            playAIGame (numMoves - 1) newPlayer newBoard
        Checkmate color -> putStrLn $ "Xeque-mate! O jogador " ++ show color ++ " venceu."
        Stalemate -> putStrLn "Afogamento! O jogo empatou."
        Draw -> putStrLn "Empate! O jogo empatou."

main :: IO ()
main = mainMenu



