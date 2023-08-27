-- Módulos necessários para funcionalidades do jogo
module Main where
import Data.Maybe ( isNothing, isJust )
import Data.Char (toLower)
import System.Random ( randomRIO )
import Text.Read (readMaybe)

-- Definição dos tipos de peças, cores e posições
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving Show
data Color = White | Black deriving (Eq, Show)
data Position = Position Int Int deriving (Eq, Show)
data ChessPiece = ChessPiece { piece :: Piece, color :: Color } deriving Show
data GameResult = Ongoing | Checkmate Color | Stalemate | Draw deriving (Eq, Show)
type Board = [[Maybe ChessPiece]]

-- Função para criar um tabuleiro vazio
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing)

-- Função para colocar uma peça no tabuleiro
placePiece :: Position -> Maybe ChessPiece -> Board -> Board
placePiece (Position x y) maybePiece board =
    take y board ++ [take x (board !! y) ++ maybePiece : drop (x + 1) (board !! y)] ++ drop (y + 1) board

-- Função para mover uma peça no tabuleiro
movePiece :: Position -> Position -> Board -> Maybe Board
movePiece fromPos toPos board =
    -- Obtém a peça da posição de origem
    let maybePiece = getPieceAtPosition fromPos board
        -- Remove a peça da posição de origem do tabuleiro
        newBoard = placePiece fromPos Nothing board
    in case maybePiece of
        Just (ChessPiece King color) ->
            -- Verifica se é um movimento de roque
            if isKingsideCastle fromPos toPos color board
                then Just (castleKingside color newBoard)
            else if isQueensideCastle fromPos toPos color board
                then Just (castleQueenside color newBoard)
            else if isValidKingMove fromPos toPos board
                then Just (placePiece toPos (Just (ChessPiece King color)) newBoard) -- Move o rei
            else Nothing -- Movimento inválido para o rei
        Just (ChessPiece Pawn color) ->
            -- Verifica se é um movimento válido para o peão
            if isValidPawnMove fromPos toPos color board
                then Just (placePiece toPos (Just (ChessPiece Pawn color)) newBoard) -- Move o peão
            else Nothing -- Movimento inválido para o peão
        Just (ChessPiece Knight _) ->
            -- Verifica se é um movimento válido para o cavalo
            if isValidKnightMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard) -- Move o cavalo
            else Nothing -- Movimento inválido para o cavalo
        Just (ChessPiece Bishop _) ->
            -- Verifica se é um movimento válido para o bispo
            if isValidBishopMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard) -- Move o bispo
            else Nothing -- Movimento inválido para o bispo
        Just (ChessPiece Rook _) ->
            -- Verifica se é um movimento válido para a torre
            if isValidRookMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard) -- Move a torre
            else Nothing -- Movimento inválido para a torre
        Just (ChessPiece Queen _) ->
            -- Verifica se é um movimento válido para a rainha
            if isValidQueenMove fromPos toPos board
                then Just (placePiece toPos maybePiece newBoard) -- Move a rainha
            else Nothing -- Movimento inválido para a rainha
        Nothing -> Nothing -- Não há peça na posição de origem


-- Roque

-- Verifica se é um movimento de roque do lado do rei
isKingsideCastle :: Position -> Position -> Color -> Board -> Bool
isKingsideCastle fromPos toPos color board =
    -- Define a posição atual do rei da cor correspondente
    let kingPos = if color == White then Position 4 0 else Position 4 7
    -- Verifica se a posição de origem é a posição do rei e a posição de destino é a posição do roque do lado do rei
    in fromPos == kingPos && toPos == (if color == White then Position 6 0 else Position 6 7)

-- Verifica se é um movimento de roque do lado da rainha
isQueensideCastle :: Position -> Position -> Color -> Board -> Bool
isQueensideCastle fromPos toPos color board =
    -- Define a posição atual do rei da cor correspondente
    let kingPos = if color == White then Position 4 0 else Position 4 7
    -- Verifica se a posição de origem é a posição do rei e a posição de destino é a posição do roque do lado da rainha
    in fromPos == kingPos && toPos == (if color == White then Position 2 0 else Position 2 7)

-- Realiza um movimento de roque do lado do rei, atualizando o tabuleiro
castleKingside :: Color -> Board -> Board
castleKingside color board =
    -- Define as posições atuais do rei, do novo rei após o roque e da nova torre após o roque
    let kingPos = if color == White then Position 4 0 else Position 4 7
        newKingPos = if color == White then Position 6 0 else Position 6 7
        newRookPos = if color == White then Position 5 0 else Position 5 7
    in
        -- Verifica se o roque do lado do rei é possível
        if canCastleKingside color board
            then placePiece kingPos Nothing  -- Remove o rei da sua posição atual
                 (placePiece newKingPos (Just (ChessPiece King color))  -- Coloca o novo rei na posição após o roque
                 (placePiece newRookPos (Just (ChessPiece Rook color))  -- Coloca a nova torre na posição após o roque
                 (placePiece (if color == White then Position 7 0 else Position 7 7) Nothing board)))  -- Remove a torre da sua posição original
            else board  -- Caso o roque do lado do rei não seja possível, o tabuleiro permanece inalterado

-- Realiza um movimento de roque do lado da rainha, atualizando o tabuleiro
castleQueenside :: Color -> Board -> Board
castleQueenside color board =
    -- Define as posições atuais do rei, do novo rei após o roque e da nova torre após o roque
    let kingPos = if color == White then Position 4 0 else Position 4 7
        newKingPos = if color == White then Position 2 0 else Position 2 7
        newRookPos = if color == White then Position 3 0 else Position 3 7
    in
        -- Verifica se o roque do lado da rainha é possível
        if canCastleQueenside color board
            then placePiece kingPos Nothing  -- Remove o rei da sua posição atual
                 (placePiece newKingPos (Just (ChessPiece King color))  -- Coloca o novo rei na posição após o roque
                 (placePiece newRookPos (Just (ChessPiece Rook color))  -- Coloca a nova torre na posição após o roque
                 (placePiece (if color == White then Position 0 0 else Position 0 7) Nothing board)))  -- Remove a torre da sua posição original
            else board  -- Caso o roque do lado da rainha não seja possível, o tabuleiro permanece inalterado

-- Verifica se o roque do lado do rei é possível para a cor especificada
canCastleKingside :: Color -> Board -> Bool
canCastleKingside color board =
    let row = if color == White then 0 else 7
    in
        case (board !! row) !! 7 of
            Just (ChessPiece Rook color') -> color' == color  -- Verifica se a torre está na posição correta e tem a mesma cor do rei
            _ -> False  -- Retorna False caso não seja possível realizar o roque do lado do rei

-- Verifica se o roque do lado da rainha é possível para a cor especificada
canCastleQueenside :: Color -> Board -> Bool
canCastleQueenside color board =
    let row = if color == White then 0 else 7
    in
        case (board !! row) !! 0 of
            Just (ChessPiece Rook color') -> color' == color  -- Verifica se a torre está na posição correta e tem a mesma cor do rei
            _ -> False  -- Retorna False caso não seja possível realizar o roque do lado da rainha


-- Obtém a peça de xadrez em uma determinada posição do tabuleiro, se estiver dentro dos limites do tabuleiro
getPieceAtPosition :: Position -> Board -> Maybe ChessPiece
getPieceAtPosition (Position x y) board =
    if x >= 0 && x < 8 && y >= 0 && y < 8  -- Verifica se as coordenadas estão dentro dos limites do tabuleiro
        then (board !! y) !! x  -- Retorna a peça na posição (x, y) do tabuleiro
        else Nothing  -- Retorna Nothing se as coordenadas estiverem fora dos limites do tabuleiro


-- Imprime o tabuleiro de xadrez no console com suas peças e coordenadas
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "    a   b   c   d   e   f   g   h"  -- Imprime a linha superior das coordenadas das colunas
    putStrLn " +----------------------------------+"  -- Imprime a linha superior do tabuleiro
    mapM_ printRowWithNumbers (zip [1..8] board)  -- Imprime cada linha do tabuleiro com seus números de linha
    putStrLn " +----------------------------------+"  -- Imprime a linha inferior do tabuleiro
  where
    -- Imprime uma linha do tabuleiro com números de linha e peças
    printRowWithNumbers (rowNum, row) = do
        putStr $ show rowNum ++ " |"  -- Imprime o número da linha seguido por um espaço e um pipe "|"
        printRow row
        putStrLn ""  -- Imprime uma nova linha após imprimir uma linha do tabuleiro

    -- Imprime uma linha do tabuleiro com as peças
    printRow :: [Maybe ChessPiece] -> IO ()
    printRow [] = putStr "|"  -- Se não houver peças na linha, imprime apenas um pipe "|"
    printRow (Nothing:xs) = putStr " . |" >> printRow xs  -- Imprime um espaço seguido de um pipe "|", representando uma célula vazia, e continua a impressão da próxima célula
    printRow (Just piece:xs) = putStr (" " ++ showPiece piece ++ " |") >> printRow xs  -- Imprime a representação da peça e um pipe "|", e continua a impressão da próxima célula

    -- Retorna a representação visual da peça de xadrez
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
        in if color == White then pieceSymbolWhite else pieceSymbolBlack  -- Retorna o símbolo da peça na cor correspondente

-- Função para obter a cor oposta
oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

-- Verifica o resultado do jogo com base no estado atual do tabuleiro e do jogador atual
checkGameResult :: Board -> Color -> GameResult
checkGameResult board currentPlayer
    | isCheckmate board currentPlayer = Checkmate (oppositeColor currentPlayer)  -- Se o jogador estiver em xeque-mate, retorna o resultado de xeque-mate para o oponente
    | isStalemate board currentPlayer = Stalemate  -- Se o jogador estiver em empate por afogamento, retorna o resultado de empate por afogamento
    | otherwise = Ongoing  -- Caso contrário, o jogo ainda está em andamento

-- Verifica se um jogador está em xeque-mate
isCheckmate :: Board -> Color -> Bool
isCheckmate board currentPlayer =
    isKingInCheck board currentPlayer &&
    not (any (\fromPos -> any (\toPos -> isValidMove fromPos toPos board) allPositions) (piecesOfColor currentPlayer board))

-- Verifica se um jogador está em empate por afogamento
isStalemate :: Board -> Color -> Bool
isStalemate board currentPlayer =
    not (isKingInCheck board currentPlayer) &&
    all (\fromPos -> all (\toPos -> not (isValidMove fromPos toPos board)) allPositions) (piecesOfColor currentPlayer board)

-- Obtém todas as posições das peças de uma determinada cor
piecesOfColor :: Color -> Board -> [Position]
piecesOfColor color board =
    [pos | pos <- allPositions, colorOfPieceAtPosition pos board == Just color]

-- Verifica se o rei de uma determinada cor está em xeque
isKingInCheck :: Board -> Color -> Bool
isKingInCheck board color =
    let kingPosition = findKingPosition board color
    in any (\position -> isAttackedByOpponent position color board) allPositions

-- Encontra a posição do rei de uma determinada cor no tabuleiro
findKingPosition :: Board -> Color -> Position
findKingPosition board color =
    head [pos | pos <- allPositions, isKing pos]
  where
    -- Verifica se uma peça na posição dada é um rei da cor especificada
    isKing pos = case getPieceAtPosition pos board of
        Just (ChessPiece King pieceColor) -> pieceColor == color
        _ -> False

-- Função para verificar se uma posição está sendo atacada por um oponente
isAttackedByOpponent :: Position -> Color -> Board -> Bool
isAttackedByOpponent targetPos attackerColor board =
    any (\pos -> isSquareAttackedByPiece pos targetPos attackerColor board) allPositions

-- Verifica se um quadrado está sendo atacado por uma peça
isSquareAttackedByPiece :: Position -> Position -> Color -> Board -> Bool
isSquareAttackedByPiece piecePos targetPos color board =
    case getPieceAtPosition piecePos board of
        -- Verifica se a peça na posição de origem é da cor especificada
        Just (ChessPiece pieceType pieceColor) ->
            pieceColor == color && isValidMove piecePos targetPos board
        -- Se não houver peça na posição de origem, retorna False
        _ -> False

-- Função principal para controlar o loop do jogo
playGame :: Color -> Board -> Bool -> IO ()
playGame currentPlayer board isIA = do
    putStrLn $ "Jogador atual: " ++ show currentPlayer
    printBoard board

    -- Verifica o resultado atual do jogo
    let gameResult = checkGameResult board currentPlayer
    case gameResult of
        Ongoing -> do
            if currentPlayer == Black && isIA then do
                -- Se for a vez do jogador IA, realiza um movimento IA
                newBoard <- makeAIMove Black board
                putStrLn "Movimento IA:"
                printBoard newBoard
                playGame White newBoard isIA
            else do
                -- Solicita a entrada do jogador humano para movimentos
                putStrLn "Digite a posição referente a peça que você quer mover (ex., '2e'):"
                fromInput <- getLine
                let fromPos = parsePosition fromInput

                putStrLn "Digite a posição de destino para a peça (ex., '4e'):"
                toInput <- getLine
                let toPos = parsePosition toInput

                -- Verifica a peça e a cor na posição de origem
                case (getPieceAtPosition fromPos board, colorOfPieceAtPosition fromPos board) of
                    (Just piece, Just pieceColor) | pieceColor == currentPlayer ->
                        -- Tenta mover a peça e atualizar o tabuleiro
                        case movePiece fromPos toPos board of
                            Just newBoard -> do
                                -- Passa a vez para o próximo jogador e continua o jogo
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

-- Analisar uma entrada de posição no formato de string e criar uma posição
parsePosition :: String -> Position
parsePosition [row, col] =
    -- Converte o caractere da coluna em um índice e o caractere da linha em um valor de posição
    Position (colToIndex col) (read [row] - 1)

-- Converter um caractere de coluna em um índice
colToIndex :: Char -> Int
colToIndex col =
    -- Calcula a diferença entre o valor ASCII do caractere col e 'a' para determinar o índice da coluna
    fromEnum col - fromEnum 'a'

-- Função para obter a cor de uma peça em uma determinada posição
colorOfPieceAtPosition :: Position -> Board -> Maybe Color
colorOfPieceAtPosition pos board =
    -- Obtém a peça na posição especificada
    case getPieceAtPosition pos board of
        Just piece ->
            -- Retorna a cor da peça, se existir
            Just (color piece)
        Nothing ->
            -- Se não houver peça na posição, retorna Nothing
            Nothing


-- Função que cria e retorna o tabuleiro de xadrez inicial, onde:
-- - As peças pretas (Black) são representadas pelos seguintes símbolos: Rook (♜), Knight (♞), Bishop (♝), Queen (♛), King (♚), Pawn (♟).
-- - As peças brancas (White) são representadas pelos seguintes símbolos: Rook (♖), Knight (♘), Bishop (♗), Queen (♕), King (♔), Pawn (♙).
initialBoard :: Board
initialBoard =
    [ -- Linha 8 (Black)
      [Just (ChessPiece Rook Black), Just (ChessPiece Knight Black), Just (ChessPiece Bishop Black), Just (ChessPiece Queen Black), Just (ChessPiece King Black), Just (ChessPiece Bishop Black), Just (ChessPiece Knight Black), Just (ChessPiece Rook Black)]
    , -- Linha 7 (Black)
      [Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black), Just (ChessPiece Pawn Black)]
    , -- Linhas 6 a 3 (Vazias)
      replicate 8 Nothing
    , -- Linhas 2 e 1 (White)
      [Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White), Just (ChessPiece Pawn White)]
    , -- Linha 0 (White)
      [Just (ChessPiece Rook White), Just (ChessPiece Knight White), Just (ChessPiece Bishop White), Just (ChessPiece Queen White), Just (ChessPiece King White), Just (ChessPiece Bishop White), Just (ChessPiece Knight White), Just (ChessPiece Rook White)]
    ]

-- Validando Movimentos

-- Função que verifica se um movimento de peão é válido, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino, a cor do peão e o tabuleiro.
isValidPawnMove :: Position -> Position -> Color -> Board -> Bool
isValidPawnMove (Position x1 y1) (Position x2 y2) color board =
    let deltaX = abs (x2 - x1)  -- Calcula a diferença absoluta nas coordenadas X entre as posições inicial e de destino
        deltaY = abs (y2 - y1)  -- Calcula a diferença absoluta nas coordenadas Y entre as posições inicial e de destino
        pieceAtDest = getPieceAtPosition (Position x2 y2) board  -- Obtém a peça na posição de destino, se houver
    in case color of
        Black -> case pieceAtDest of
            Nothing ->
                -- Movimento para frente de 1 casa ou movimento inicial de 2 casas para peões pretos
                deltaX == 0 && y2 - y1 == 1
                || (deltaX == 0 && y2 - y1 == 2 && y1 == 1 && isNothing (getPieceAtPosition (Position x1 (y1 + 1)) board))
                -- Captura diagonal de peça oponente
                || (deltaX == 1 && deltaY == 1 && isOpponentPiece (Position x2 y2) color board)
            Just _ ->
                -- Captura diagonal de peça oponente
                deltaX == 1 && y1 - y2 == 1 && isOpponentPiece (Position x2 y2) color board
        White -> case pieceAtDest of
            Nothing ->
                -- Movimento para frente de 1 casa ou movimento inicial de 2 casas para peões brancos
                deltaX == 0 && y1 - y2 == 1
                || (deltaX == 0 && y1 - y2 == 2 && y1 == 6 && isNothing (getPieceAtPosition (Position x1 (y1 - 1)) board))
                -- Captura diagonal de peça oponente
                || (deltaX == 1 && deltaY == 1 && isOpponentPiece (Position x2 y2) color board)
            Just _ ->
                -- Captura diagonal de peça oponente
                deltaX == 1 && y1 - y2 == 1 && isOpponentPiece (Position x2 y2) color board


-- Função que verifica se um movimento de cavalo é válido, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidKnightMove :: Position -> Position -> Board -> Bool
isValidKnightMove (Position x1 y1) (Position x2 y2) _ =
    let deltaX = abs (x2 - x1)  -- Calcula a diferença absoluta nas coordenadas X entre as posições inicial e de destino
        deltaY = abs (y2 - y1)  -- Calcula a diferença absoluta nas coordenadas Y entre as posições inicial e de destino
    in (deltaX == 1 && deltaY == 2) || (deltaX == 2 && deltaY == 1)

-- Função que verifica se um movimento de rainha é válido, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidQueenMove :: Position -> Position -> Board -> Bool
isValidQueenMove fromPos toPos board =
    isValidRookMove fromPos toPos board || isValidBishopMove fromPos toPos board

-- Função que verifica se um movimento de bispo é válido, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidBishopMove :: Position -> Position -> Board -> Bool
isValidBishopMove fromPos toPos board =
    let deltaX = abs (x2 - x1)  -- Calcula a diferença absoluta nas coordenadas X entre as posições inicial e de destino
        deltaY = abs (y2 - y1)  -- Calcula a diferença absoluta nas coordenadas Y entre as posições inicial e de destino
        positions = zip [min x1 x2 + 1 .. max x1 x2 - 1] [min y1 y2 + 1 .. max y1 y2 - 1]  -- Gera posições intermediárias entre as coordenadas X e Y
        validDiagonal = deltaX == deltaY  -- Verifica se o movimento é na diagonal
        noObstructions = all (\(x, y) -> isNothing (getPieceAtPosition (Position x y) board)) positions  -- Verifica se não há obstruções no caminho
    in validDiagonal && noObstructions
  where
    Position x1 y1 = fromPos
    Position x2 y2 = toPos

-- Função que verifica se um movimento de torre é válido, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidRookMove :: Position -> Position -> Board -> Bool
isValidRookMove fromPos toPos board =
    isValidHorizontalMove fromPos toPos board || isValidVerticalMove fromPos toPos board

-- Função que verifica se um movimento de rei é válido, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino, e o tabuleiro.
isValidKingMove :: Position -> Position -> Board -> Bool
isValidKingMove fromPos@(Position x1 y1) toPos@(Position x2 y2) board
    | isKingsideCastle fromPos toPos White board = canCastleKingside White board
    | isQueensideCastle fromPos toPos White board = canCastleQueenside White board
    | isKingsideCastle fromPos toPos Black board = canCastleKingside Black board
    | isQueensideCastle fromPos toPos Black board = canCastleQueenside Black board
    | otherwise =
        let deltaX = abs (x2 - x1)  -- Calcula a diferença absoluta nas coordenadas X entre as posições inicial e de destino
            deltaY = abs (y2 - y1)  -- Calcula a diferença absoluta nas coordenadas Y entre as posições inicial e de destino
        in deltaX <= 1 && deltaY <= 1  -- Verifica se a diferença nas coordenadas é no máximo 1, indicando um movimento válido de rei

-- Helper functions

-- Função que verifica se um movimento é válido na diagonal, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidDiagonalMove :: Position -> Position -> Board -> Bool
isValidDiagonalMove (Position x1 y1) (Position x2 y2) _ =
    let deltaX = abs (x2 - x1)  -- Calcula a diferença absoluta nas coordenadas X entre as posições inicial e de destino
        deltaY = abs (y2 - y1)  -- Calcula a diferença absoluta nas coordenadas Y entre as posições inicial e de destino
    in deltaX == deltaY  -- Verifica se a diferença nas coordenadas é a mesma em ambas as direções, indicando um movimento válido na diagonal

-- Função que verifica se um movimento é válido na horizontal, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidHorizontalMove :: Position -> Position -> Board -> Bool
isValidHorizontalMove (Position x1 y1) (Position x2 y2) board
    | y1 /= y2 = False  -- Se as coordenadas Y não forem iguais, o movimento não é horizontal
    | otherwise = all (\x -> isNothing (getPieceAtPosition (Position x y1) board)) positions
  where
    positions = [min x1 x2 + 1 .. max x1 x2 - 1]  -- Gera uma lista de posições intermediárias na direção horizontal

-- Função que verifica se um movimento é válido na vertical, levando em consideração as regras do xadrez.
-- Recebe a posição inicial, a posição de destino e o tabuleiro.
isValidVerticalMove :: Position -> Position -> Board -> Bool
isValidVerticalMove (Position x1 y1) (Position x2 y2) board
    | x1 /= x2 = False  -- Se as coordenadas X não forem iguais, o movimento não é vertical
    | otherwise = all (\y -> isNothing (getPieceAtPosition (Position x1 y) board)) positions
  where
    positions = [min y1 y2 + 1 .. max y1 y2 - 1]  -- Gera uma lista de posições intermediárias na direção vertical

-- Função que verifica se a peça em uma determinada posição é uma peça oponente, com base na cor especificada.
-- Recebe a posição da peça, a cor do jogador atual e o tabuleiro.
isOpponentPiece :: Position -> Color -> Board -> Bool
isOpponentPiece pos color board =
    case getPieceAtPosition pos board of
        Just (ChessPiece _ pieceColor) -> pieceColor /= color  -- Verifica se a cor da peça na posição não é igual à cor do jogador atual
        Nothing -> False  -- Se não houver peça na posição, não é uma peça oponente


-- Função que gera um movimento feito pela inteligência artificial (IA).
-- Recebe a cor da IA e o tabuleiro atual como entrada e retorna o novo tabuleiro após o movimento da IA.
makeAIMove :: Color -> Board -> IO Board
makeAIMove aiColor board = do
    -- Filtra as posições das peças da IA
    let aiPieces = filter (\pos -> colorOfPieceAtPosition pos board == Just aiColor) allPositions

    -- Seleciona aleatoriamente uma das peças da IA
    selectedPiece <- randomElement aiPieces

    -- Obtém todos os movimentos válidos para a peça selecionada
    let validMoves = getValidMoves selectedPiece board

    -- Seleciona aleatoriamente um dos movimentos válidos
    selectedMove <- randomElement validMoves

    -- Tenta realizar o movimento selecionado
    case movePiece selectedPiece selectedMove board of
        Just newBoard -> return newBoard  -- Retorna o novo tabuleiro se o movimento for válido
        Nothing -> makeAIMove aiColor board  -- Se o movimento for inválido, a IA tenta novamente fazer um movimento válido


-- Função que seleciona aleatoriamente um elemento de uma lista.
randomElement :: [a] -> IO a
randomElement xs = do
    -- Gera um índice aleatório dentro dos limites da lista e retorna o elemento correspondente.
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

-- Lista de todas as posições possíveis em um tabuleiro de xadrez.
allPositions :: [Position]
allPositions = [Position x y | x <- [0..7], y <- [0..7]]

-- Função que obtém todos os movimentos válidos a partir de uma posição dada no tabuleiro.
getValidMoves :: Position -> Board -> [Position]
getValidMoves fromPos board =
    -- Cria uma lista de posições de destino para as quais um movimento a partir da posição inicial é válido.
    [toPos | toPos <- allPositions, isValidMove fromPos toPos board]

-- Função que verifica se um movimento de uma posição de origem para uma posição de destino é válido no tabuleiro.
isValidMove :: Position -> Position -> Board -> Bool
isValidMove fromPos toPos board =
    case getPieceAtPosition fromPos board of
        Just piece -> case movePiece fromPos toPos board of
            Just newBoard -> True  -- O movimento é válido se resultar em um novo tabuleiro não nulo.
            Nothing -> False  -- O movimento não é válido se resultar em um tabuleiro nulo.
        Nothing -> False  -- Não há peça na posição de origem, portanto, o movimento não é válido.

-- Menu Principal
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

-- Função que inicia uma partida contra a IA.
playAgainstAI :: IO ()
playAgainstAI = do
    putStrLn "Iniciando partida contra a IA..."
    -- Inicia o jogo com o jogador branco jogando contra a IA, utilizando o tabuleiro inicial.
    playGame White initialBoard True

-- Função que inicia uma partida contra outro jogador humano.
playAgainstPlayer :: IO ()
playAgainstPlayer = do
    putStrLn "Iniciando partida contra outro jogador..."
    -- Inicia o jogo com o jogador branco jogando contra outro jogador humano, utilizando o tabuleiro inicial.
    playGame White initialBoard False


-- Função que inicia a simulação de um jogo entre duas IA's.
simulateAIGame :: IO ()
simulateAIGame = do
    putStrLn "Simulando jogo entre duas IA's..."
    putStrLn "Digite o número de jogadas"
    numMovesStr <- getLine
    -- Verifica se a entrada é um número inteiro
    if isInteger numMovesStr then
        case readMaybe numMovesStr of
            Just numMoves -> playAIGame numMoves Black initialBoard
            Nothing -> do
                putStrLn "Erro na conversão do número de jogadas."
                simulateAIGame
    else do
        putStrLn "Opção inválida."
        simulateAIGame

-- Função auxiliar que verifica se uma string representa um número inteiro.
isInteger :: String -> Bool
isInteger str = isJust (readMaybe str :: Maybe Integer)

-- Função para simular um jogo entre duas IA's.
-- Recebe o número máximo de movimentos, a cor do jogador atual e o tabuleiro atual.
playAIGame :: Int -> Color -> Board -> IO ()
-- Condição de parada: atingiu o limite de movimentos.
playAIGame 0 _ board = do
    putStrLn "Limite de movimentos atingido. O jogo foi encerrado."
    printBoard board
-- Caso geral: continua o jogo com o número atual de movimentos.
playAIGame numMoves currentPlayer board = do
    putStrLn $ "Jogador atual: " ++ show currentPlayer
    printBoard board

    let gameResult = checkGameResult board currentPlayer
    case gameResult of
        Ongoing -> do
            -- A IA faz um movimento e obtém um novo tabuleiro.
            newBoard <- makeAIMove currentPlayer board
            putStrLn "Movimento IA:"
            printBoard newBoard
            -- Alterna o jogador e continua o jogo.
            let newPlayer = if currentPlayer == White then Black else White
            playAIGame (numMoves - 1) newPlayer newBoard
        -- Exibe os resultados finais.
        Checkmate color -> putStrLn $ "Xeque-mate! O jogador " ++ show color ++ " venceu."
        Stalemate -> putStrLn "Afogamento! O jogo empatou."
        Draw -> putStrLn "Empate! O jogo empatou."

main :: IO ()
main = mainMenu



