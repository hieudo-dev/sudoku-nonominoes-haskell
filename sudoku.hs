module Sudoku where
	import Data.List

	------------------------------- Tipos ------------------------------

	data Nonomino = Nonomino [(Int, Int)] deriving (Show, Eq)

	data GeoSudoku = GeoSudoku [((Int, Int), Nonomino)]

	---------------------- Funciones Auxiliares ------------------------

	-- Devuelve la lista de tuplas (x, xs) donde x es un elemento de la lista dada
	-- y xs es el resto
	selections :: [a] -> [(a, [a])]
	selections [] = []
	selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]


	-- Devuelve la lista de todas las posibles permutaciones de 'list'
	permutations :: [a] -> [[a]]
	permutations [] = [[]]
	permutations list = [x:ys | (x, xs) <- selections list, ys <- permutations xs]

	------------------------------------------

	-- Devuelve la lista de las permutaciones de los nonominos que representan
	-- tableros validos de sudoku
	buildSudokus :: [Nonomino] -> [[((Int, Int), Nonomino)]]
	buildSudokus nonos = 
		let f order = validMatch order nonos []
		in [ f order | order <- permutations [0..8], (f order) /= []]
	

	-- Devuelve si la representacion del sudoku que se genera al unir los nonominos de izq a der 
	-- en el orden dado, si no es posible se devuelve [] 
	validMatch :: [Int] -> [Nonomino] -> [((Int, Int), Nonomino)] -> [((Int, Int), Nonomino)]
	validMatch [] nonos board = board
	validMatch (h:t) nonos board
		| newBoard == [] = []
		| otherwise = validMatch t nonos newBoard
		where
			newBoard = placeNonomino (nonos !! h) board
	

	-- Devuelve la nueva representacion del sudoku que se genera al colocar el nonomino, 
	-- si no posible devuelve [] 
	placeNonomino :: Nonomino -> [((Int, Int), Nonomino)] -> [((Int, Int), Nonomino)]
	placeNonomino (Nonomino points) list
		| overlapTiles /= [] = []
		| otherwise = list ++ [(firstEmpty, Nonomino points)]
			where
				firstEmpty = head $ getEmptyTiles list		-- Primera casilla vacia
				a = fst firstEmpty
				b = snd firstEmpty
				overlapTiles = [ (i+a, j+b) | (i, j) <- points, isUsed (i+a, j+b) list]


	-- Dado la posicion de una casilla y una representacion del sudoku devuelve si 
	-- esta casilla esta ocupada en esa representacion 
	isUsed :: (Int, Int) -> [((Int, Int), Nonomino)] -> Bool
	isUsed (x, y) list =
		let
			overlaps ((u, v), Nonomino points) =
				find (==True) (map (\(i, j) -> (i+u, j+v) == (x, y)) points) == Just (True)
			usedTiles =  find (==True) (map overlaps list)
		in
			usedTiles /= Nothing


	-- Devuelve los pares ordenados (i, j) que estan ocupados por algun Nonomino
	getUsedTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
	getUsedTiles list = [ (a+i, b+j) | ((a, b), Nonomino points) <- list, (i,j) <- points]


	-- Devuelve los pares ordenados (i, j) que esten desocupados
	getEmptyTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
	getEmptyTiles list = [ (i, j) | i <- [0..8],
										  j <- [0..8],
										  not (elem (i, j) $ getUsedTiles list)]