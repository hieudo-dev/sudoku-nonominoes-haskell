module Matching where
	import Data.List
	import Types

	---------------------------- F AUXILIARES -------------------------------------------------------------

	-- Devuelve la lista de tuplas (x, xs) donde x es un elemento de la lista dada
	-- y xs es el resto
	selections :: [a] -> [(a, [a])]
	selections [] = []
	selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]


	-- Devuelve la lista de todas las posibles permutaciones de 'list'
	permutations :: [a] -> [[a]]
	permutations [] = [[]]
	permutations list = [x:ys | (x, xs) <- selections list, ys <- permutations xs]

	------------------------- GENERAR SUDOKU --------------------------------------------------------------

	-- Devuelve la lista de las tableros sudokus validos que se pueden generar a partir
	-- de la lista de nonominos dada
	buildSudokus :: [Nonomino] -> [Sudoku]
	buildSudokus nonos =
		let f order = validMatch order nonos (Sudoku []); recursives = [ f order | order <- permutations [0..8]]
		in [ x | x <- recursives, x /= Sudoku []]


	-- Devuelve si la representacion del sudoku que se genera al unir los nonominos de izq a der
	-- en el orden dado es valida, si no es valido se devuelve un sudoku vacio: Sudoku []
	validMatch :: [Int] -> [Nonomino] -> Sudoku -> Sudoku
	validMatch [] nonos sudoku = sudoku
	validMatch (h:t) nonos sudoku
		| newSudoku == Sudoku [] = Sudoku []
		| otherwise = validMatch t nonos newSudoku
		where
			newSudoku = placeNonomino (nonos !! h) sudoku

			----------------------------------------------------------------------------

	-- Devuelve la nueva representacion del sudoku que se genera al colocar el nonomino,
	-- en un sudoku dado, si no es posible devuelve []
	placeNonomino :: Nonomino -> Sudoku -> Sudoku
	placeNonomino (Nonomino tiles) sudoku
		| (overlapTiles /= []) || (not $ isInside (Nonomino tiles) firstEmpty) = Sudoku []
		| otherwise = Sudoku (list ++ [(firstEmpty, Nonomino tiles)])
			where
				(Sudoku list) = sudoku
				firstEmpty = head $ getEmptyTiles sudoku		-- Primera casilla vacia
				a = fst firstEmpty
				b = snd firstEmpty
				overlapTiles = [ (i+a, j+b) | (i, j, _) <- tiles, isUsed (i+a, j+b) sudoku]
				isInside (Nonomino tiles) (a, b) =
					let outsideTile = find (\(x,y,_) -> (x+a>8) || (y+b>8) || (y+b<0)) tiles
					in outsideTile == Nothing


	-- Dado la posicion de una casilla y una representacion del sudoku devuelve si
	-- esta casilla esta ocupada en esa representacion
	isUsed :: (Int, Int) -> Sudoku -> Bool
	isUsed (x, y) sudoku = elem (x, y) $ getUsedTiles sudoku


	-- Devuelve los pares ordenados (i, j) que estan ocupados por algun Nonomino
	getUsedTiles :: Sudoku -> [(Int, Int)]
	getUsedTiles (Sudoku nonos) = [ (a+i, b+j) | ((a, b), Nonomino tiles) <- nonos, (i,j,_) <- tiles]


	-- Devuelve los pares ordenados (i, j) que esten desocupados
	getEmptyTiles :: Sudoku -> [(Int, Int)]
	getEmptyTiles sudoku = [ (i, j) | i <- [0..8], j <- [0..8], not (elem (i, j) $ getUsedTiles sudoku)]
