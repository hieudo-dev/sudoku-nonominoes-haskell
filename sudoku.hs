module Sudoku where
	import Data.List

	------------------------------- TIPOS ------------------------------

	data Nonomino = Nonomino [(Int, Int, Int)] deriving (Show, Eq)

	---------------------------- F AUXILIARES --------------------------

	-- Devuelve la lista de tuplas (x, xs) donde x es un elemento de la lista dada
	-- y xs es el resto
	selections :: [a] -> [(a, [a])]
	selections [] = []
	selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]


	-- Devuelve la lista de todas las posibles permutaciones de 'list'
	permutations :: [a] -> [[a]]
	permutations [] = [[]]
	permutations list = [x:ys | (x, xs) <- selections list, ys <- permutations xs]

	------------------------- GENERAR SUDOKU ---------------------------

	-- Devuelve la lista de las tableros sudokus validos que se pueden generar a partir 
	-- de la lista de nonominos dada
	buildSudokus :: [Nonomino] -> [[((Int, Int), Nonomino)]]
	buildSudokus nonos = 
		let f order = validMatch order nonos []
		in [ f order | order <- permutations [0..8], (f order) /= []]
	

	-- Devuelve si la representacion del sudoku que se genera al unir los nonominos de izq a der 
	-- en el orden dado es valida, si no se devuelve [] 
	validMatch :: [Int] -> [Nonomino] -> [((Int, Int), Nonomino)] -> [((Int, Int), Nonomino)]
	validMatch [] nonos board = board
	validMatch (h:t) nonos board
		| newBoard == [] = []
		| otherwise = validMatch t nonos newBoard
		where
			newBoard = placeNonomino (nonos !! h) board

			-----------------------------------------------

	-- Devuelve la nueva representacion del sudoku que se genera al colocar el nonomino, 
	-- en un sudoku dado, si no es posible devuelve [] 
	placeNonomino :: Nonomino -> [((Int, Int), Nonomino)] -> [((Int, Int), Nonomino)]
	placeNonomino (Nonomino tiles) list
		| (overlapTiles /= []) || (not $ isInside (Nonomino tiles) firstEmpty) = []
		| otherwise = list ++ [(firstEmpty, Nonomino tiles)]
			where
				firstEmpty = head $ getEmptyTiles list		-- Primera casilla vacia
				a = fst firstEmpty
				b = snd firstEmpty
				overlapTiles = [ (i+a, j+b) | (i, j, _) <- tiles, isUsed (i+a, j+b) list]
				isInside (Nonomino tiles) (a, b) = 
					let outsideTile = find (\(x,y,_) -> (x+a>8) || (y+b>8) || (y+b<0)) tiles
					in outsideTile == Nothing 


	-- Dado la posicion de una casilla y una representacion del sudoku devuelve si 
	-- esta casilla esta ocupada en esa representacion 
	isUsed :: (Int, Int) -> [((Int, Int), Nonomino)] -> Bool
	isUsed (x, y) list = elem (x, y) $ getUsedTiles list


	-- Devuelve los pares ordenados (i, j) que estan ocupados por algun Nonomino
	getUsedTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
	getUsedTiles list = [ (a+i, b+j) | ((a, b), Nonomino tiles) <- list, (i,j,_) <- tiles]


	-- Devuelve los pares ordenados (i, j) que esten desocupados
	getEmptyTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
	getEmptyTiles list = [ (i, j) | i <- [0..8], j <- [0..8], not (elem (i, j) $ getUsedTiles list)]

	----------------------- SOLUCION SUDOKU -----------------------------

	getRow :: Int -> [((Int, Int), Nonomino)] -> [Int]
	getRow x sudoku = 
		let unOrderedRow = [ (j+b, n) | ((a,b), Nonomino tiles) <- sudoku, (i, j, n) <- tiles, a + i == x]
		in map (\(x,y) -> y) $ sort unOrderedRow

		
	getColumn :: Int -> [((Int, Int), Nonomino)] -> [Int]
	getColumn x sudoku = 
		let unOrderedRow = [ (i+a, n) | ((a,b), Nonomino tiles) <- sudoku, (i, j, n) <- tiles, b + j == x]
		in map (\(x,y) -> y) $ sort unOrderedRow