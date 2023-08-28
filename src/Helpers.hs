module Helpers where

import Data.Maybe (isNothing) 
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import System.Random
import Data

-- Função para mover uma peça no tabuleiro
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

-- Função para colocar uma peça em uma posição no tabuleiro
placePiece :: Position -> Maybe ChessPiece -> Board -> Board
placePiece (Position x y) maybePiece board =
    take y board ++ [take x (board !! y) ++ maybePiece : drop (x + 1) (board !! y)] ++ drop (y + 1) board

-- Função para realizar o roque no lado do rei
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

-- Função para realizar o roque no lado da rainha
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

-- Função para analisar a posição de uma peça no formato de string e retornar sua posição na estrutura Position
parsePosition :: String -> Position
parsePosition [row, col] = Position (colToIndex col) (read [row] - 1)

-- Função para converter a coluna (letra) para um índice (inteiro)
colToIndex :: Char -> Int
colToIndex col = fromEnum col - fromEnum 'a'

-- Função para determinar a cor de uma peça em uma posição no tabuleiro
colorOfPieceAtPosition :: Position -> Board -> Maybe Color
colorOfPieceAtPosition pos board =
    case getPieceAtPosition pos board of
        Just piece -> Just (color piece)
        Nothing -> Nothing

-- Função para obter a peça em uma determinada posição do tabuleiro
getPieceAtPosition :: Position -> Board -> Maybe ChessPiece
getPieceAtPosition (Position x y) board =
    if x >= 0 && x < 8 && y >= 0 && y < 8
        then (board !! y) !! x
        else Nothing

-- Função para verificar se o movimento é um roque do lado do rei
isKingsideCastle :: Position -> Position -> Color -> Board -> Bool
isKingsideCastle fromPos toPos color board =
    let kingPos = if color == White then Position 4 0 else Position 4 7
    in fromPos == kingPos && toPos == (if color == White then Position 6 0 else Position 6 7)

-- Função para verificar se o movimento é um roque do lado da rainha
isQueensideCastle :: Position -> Position -> Color -> Board -> Bool
isQueensideCastle fromPos toPos color board =
    let kingPos = if color == White then Position 4 0 else Position 4 7
    in fromPos == kingPos && toPos == (if color == White then Position 2 0 else Position 2 7)

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

-- Função para verificar se o movimento de um peão é válido
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

-- Função para verificar se o movimento de um cavalo é válido
isValidKnightMove :: Position -> Position -> Board -> Bool
isValidKnightMove (Position x1 y1) (Position x2 y2) _ =
    let deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
    in (deltaX == 1 && deltaY == 2) || (deltaX == 2 && deltaY == 1)

-- Função para verificar se o movimento de uma rainha é válido
isValidQueenMove :: Position -> Position -> Board -> Bool
isValidQueenMove fromPos toPos board =
    isValidRookMove fromPos toPos board || isValidBishopMove fromPos toPos board

-- Função para verificar se o movimento de um bispo é válido
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

-- Função para verificar se o movimento de uma torre é válido na horizontal ou vertical
isValidRookMove :: Position -> Position -> Board -> Bool
isValidRookMove fromPos toPos board =
    isValidHorizontalMove fromPos toPos board || isValidVerticalMove fromPos toPos board

-- Função para verificar se o movimento de um rei é válido
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

-- Função para verificar se o movimento é válido na diagonal
isValidDiagonalMove :: Position -> Position -> Board -> Bool
isValidDiagonalMove (Position x1 y1) (Position x2 y2) _ =
    let deltaX = abs (x2 - x1)
        deltaY = abs (y2 - y1)
    in deltaX == deltaY

-- Função para verificar se o movimento é válido na horizontal
isValidHorizontalMove :: Position -> Position -> Board -> Bool
isValidHorizontalMove (Position x1 y1) (Position x2 y2) board
    | y1 /= y2 = False
    | otherwise = all (\x -> isNothing (getPieceAtPosition (Position x y1) board)) positions
  where
    positions = [min x1 x2 + 1 .. max x1 x2 - 1]

-- Função para verificar se o movimento é válido na vertical
isValidVerticalMove :: Position -> Position -> Board -> Bool
isValidVerticalMove (Position x1 y1) (Position x2 y2) board
    | x1 /= x2 = False
    | otherwise = all (\y -> isNothing (getPieceAtPosition (Position x1 y) board)) positions
  where
    positions = [min y1 y2 + 1 .. max y1 y2 - 1]

-- Função para verificar se o rei pode fazer roque para o lado do rei
canCastleKingside :: Color -> Board -> Bool
canCastleKingside color board =
    let row = if color == White then 0 else 7
    in
        case (board !! row) !! 7 of
            Just (ChessPiece Rook color') -> color' == color
            _ -> False

-- Função para verificar se o rei pode fazer roque para o lado da rainha
canCastleQueenside :: Color -> Board -> Bool
canCastleQueenside color board =
    let row = if color == White then 0 else 7
    in
        case (board !! row) !! 0 of
            Just (ChessPiece Rook color') -> color' == color
            _ -> False

-- Função para verificar se uma peça em uma determinada posição pertence ao oponente
isOpponentPiece :: Position -> Color -> Board -> Bool
isOpponentPiece pos color board =
    case getPieceAtPosition pos board of
        Just (ChessPiece _ pieceColor) -> pieceColor /= color
        Nothing -> False

-- Função para verificar se uma string representa um número inteiro
isInteger :: String -> Bool
isInteger str = isJust (readMaybe str :: Maybe Integer)

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

-- Função para encontrar a posição do rei de uma determinada cor no tabuleiro
findKingPosition :: Board -> Color -> Position
findKingPosition board color =
    head [pos | pos <- allPositions, isKing pos]
  where
    isKing pos = case getPieceAtPosition pos board of
        Just (ChessPiece King pieceColor) -> pieceColor == color
        _ -> False

-- Lista de todas as posições do tabuleiro
allPositions :: [Position]
allPositions = [Position x y | x <- [0..7], y <- [0..7]]

-- Função para obter todos os movimentos válidos a partir de uma posição
getValidMoves :: Position -> Board -> [Position]
getValidMoves fromPos board =
    [toPos | toPos <- allPositions, isValidMove fromPos toPos board]

-- Função para verificar se um movimento de uma posição para outra é válido
isValidMove :: Position -> Position -> Board -> Bool
isValidMove fromPos toPos board =
    case getPieceAtPosition fromPos board of
        Just piece -> case movePiece fromPos toPos board of
            Just newBoard -> True
            Nothing -> False
        Nothing -> False

-- Função para selecionar aleatoriamente um elemento de uma lista
randomElement :: [a] -> IO a
randomElement xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)