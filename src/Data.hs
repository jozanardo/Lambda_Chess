module Data where

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving Show
data Color = White | Black deriving (Eq, Show)
data Position = Position Int Int deriving (Eq, Show)
data ChessPiece = ChessPiece { piece :: Piece, color :: Color } deriving Show
data GameResult = Ongoing | Checkmate Color | Stalemate | Draw deriving (Eq, Show)
type Board = [[Maybe ChessPiece]]