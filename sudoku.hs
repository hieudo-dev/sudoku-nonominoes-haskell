module Sudoku where
	import List

	------------------------------- Tipos ------------------------------

	data Nonomino = Nonomino [(Int, Int)]

	data GeoSudoku = GeoSudoku [((Int, Int), Nonomino)]

	---------------------- Funciones Auxiliares ------------------------

	find :: (a -> Bool) -> [a] -> Maybe a
	find f list = let candidates = filter f list in case candidates of
		[] -> Nothing
		_ -> Just (head candidates)

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
	buildSudokus :: [Nonomino] -> [[Int]]
	buildSudokus nonos = [ list | list <- permutations [0..8],
											validOrder list nonos]


	validOrder :: [Int] -> [Nonomino] -> Bool
	validOrder perm nonos = True


	-- Devuelve los pares ordenados (i, j) que estan ocupados por algun Nonomino
	getUsedTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
	getUsedTiles list = [ (a+i, b+j) | ((a, b), Nonomino points) <- list,
												 (i,j) <- points]


	-- Devuelve los pares ordenados (i, j) que esten desocupados
	getEmptyTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
	getEmptyTiles list = [ (i, j) | i <- [0..8],
										  j <- [0..8],
										  not (elem (i, j) $ getUsedTiles list)]