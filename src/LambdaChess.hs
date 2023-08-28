module LambdaChess where

import Text.Read (readMaybe)
import Data
import Helpers

-- Função para criar um tabuleiro vazio, sem peças.
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing)

-- Tabuleiro inicial do jogo de xadrez
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

-- Função para imprimir o tabuleiro de xadrez
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

-- Função para jogar contra a IA
playAgainstAI :: IO ()
playAgainstAI = do
    putStrLn "Iniciando partida contra a IA..."
    playGame White initialBoard True

-- Função para jogar contra outro jogador
playAgainstPlayer :: IO ()
playAgainstPlayer = do
    putStrLn "Iniciando partida contra outro jogador..."
    playGame White initialBoard False

-- Função para simular um jogo entre duas IA's
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

-- Função para a IA fazer um movimento
makeAIMove :: Color -> Board -> IO Board
makeAIMove aiColor board = do
    let aiPieces = filter (\pos -> colorOfPieceAtPosition pos board == Just aiColor) allPositions
    selectedPiece <- randomElement aiPieces
    let validMoves = getValidMoves selectedPiece board
    selectedMove <- randomElement validMoves
    case movePiece selectedPiece selectedMove board of
        Just newBoard -> return newBoard
        Nothing -> makeAIMove aiColor board

-- Função para jogar um jogo entre uma IA e um jogador
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

-- Função para exibir o menu principal
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