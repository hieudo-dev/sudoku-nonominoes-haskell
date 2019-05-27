module Types where
	
	data Nonomino = Nonomino [(Int, Int, Int)] deriving (Show, Eq)

	data Sudoku = Sudoku [((Int, Int), Nonomino)] deriving (Show, Eq)