module Sudoku where
	import Data.List
	import Debug.Trace

	------------------------------- TIPOS -----------------------------------------------------------------

	data Nonomino = Nonomino [(Int, Int, Int)] deriving (Show, Eq)

	data Sudoku = Sudoku [((Int, Int), Nonomino)] deriving (Show, Eq)

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

	----------------------- SOLUCION SUDOKU -----------------------------------------------------------

	-- Recibe un sudoku y lo devuelve rellenado si es posible, si no devuelve un Sudoku vacio
	solutionSudoku :: Sudoku -> Sudoku
	solutionSudoku sudoku =
		let
			sortChoices ((i,j), list, id) ((a,b), list2, id2)
				| length list < length list2 = LT
				| otherwise = GT
			solution = solve (sortBy sortChoices $ choices sudoku) (usedsRows sudoku) (usedsCols sudoku) (usedsNonos sudoku) []
			f ((i,j), Nonomino points)	=
				let
					isHint (a,b) = find (\((x,y), number) -> (a+i,b+j) == (x,y)) solution == Nothing
					unwrap (Just (x)) = let (_,b) = x in b
					newPoints = [ if isHint (a,b) then (a,b,n)
										else (a,b, unwrap $ find (\((x,y), number) -> (a+i,b+j) == (x,y)) solution)
										| (a,b,n) <- points]
				in
					((i,j), Nonomino newPoints)
			Sudoku nonos = sudoku
		in
			if solution /= [] then Sudoku (map f nonos) else Sudoku []

	
	-- Recibe la lista de casillas no rellenas con las posibles opciones y el id del nonomino, y ademas 
	-- mantiene 3 listas para consultar los numeros utilizados por cada Fila, Columna, Nonomino (+ eficiencia).
	-- Devuelve la lista de estas casillas junto al numero escogido para la solucion
	-- (La solucion se va construyendo con 'sol' y se devuelve cuando se rellenan todas las casillas)
	solve :: [((Int, Int), [Int], Int)] -> [[Int]] -> [[Int]] -> [[Int]] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
	solve [] _ _ _ sol = sol
	solve (h:t) usedR usedC usedNono sol
		| options == [] = []
		| result == Nothing = []
		| otherwise = let Just (x,y,n, solution) = result in solution
		where
			((i, j), options, nonoId) = h
			update n used x = [ if it /= x then used !! it else union [n] (used !! it) | it <- [0..8]]
			choices = [ (i, j, n, update n usedR i, update n usedC j, update n usedNono nonoId) | n <- options,
						(not(elem n $ usedR !! i) && not (elem n $ usedC !! j) && not (elem n $ usedNono !! nonoId))]
			choseResult [] = Nothing
			choseResult (choice:rs) =
				let
					(i, j, n, newR, newC, newNono) = choice
					result = solve t newR newC newNono (((i,j),n):sol)
				in
					if result == [] then choseResult rs else Just (i,j,n,result)
			result = choseResult choices


	-- Devuelve la lista de todas las casillas junto a la lista de los numeros que se pueden
	-- colocar en esta (casilla, [Opciones de #s])
	choices :: Sudoku -> [((Int, Int),[Int], Int)]
	choices sudoku =
		let getIndex (Just (x)) = x; Sudoku nonos = sudoku in
			[ ((a+i, b+j), unusedNbers (a+i, b+j) ((i, j), Nonomino points) sudoku, getIndex $ elemIndex ((i, j), Nonomino points) nonos) |
							((i, j), Nonomino points) <- nonos, (a, b, _) <- points,
							length (unusedNbers (a+i, b+j) ((i, j), Nonomino points) sudoku) /= 1]


	-- Estos metodos devuelven las listas de numeros usados por cada Fila, Columna, o Nonomino
	usedsRows :: Sudoku -> [[Int]]
	usedsRows sudoku =
		[ [ n | n <- [1..9], elem n $ getRow i sudoku]
			| i <- [0..8]]
	usedsCols :: Sudoku -> [[Int]]
	usedsCols sudoku =
		[ [ n | n <- [1..9], elem n $ getCol i sudoku]
			| i <- [0..8]]
	usedsNonos :: Sudoku -> [[Int]]
	usedsNonos (Sudoku nonos) =
		[ [n | (_,_, n) <- points, n /= 0]
			| ((_,_), Nonomino points) <- nonos]


	-- Devuelve la lista de numeros que NO pueden ser colocados en la casilla (x, y) (Recibe ademas el nonomino
	-- que contiene a la casilla (x,y))
	usedNbers :: (Int, Int) -> ((Int, Int), Nonomino) -> Sudoku -> [Int]
	usedNbers (x, y) ((i, j), Nonomino points) sudoku
		| hintNumber /= 0 = [ a | a <- [1..9], a /= hintNumber]
		| otherwise = let
			nonomino = [n | (a, b, n) <- points, (a+i, b+j) == (x, y)]
			row = [ n | n <- getRow x sudoku, n /= 0]
			col = [ n | n <- getCol y sudoku, n /= 0]
			in union (union row col) nonomino
			where
				hintTile = find (\(a, b, n) -> (x, y) == (i+a, j+b)) points
				Just (_,_,hintNumber) = hintTile


	-- Devuelve la lista de numeros que pueden ser colocados en la casilla (x, y)
	unusedNbers :: (Int, Int) -> ((Int, Int), Nonomino) -> Sudoku -> [Int]
	unusedNbers (x, y) ((i, j), Nonomino points) sudoku =
		[ n | n <- [1..9],
				not (elem n $ usedNbers (x, y) ((i, j), Nonomino points) sudoku)]


	getRow :: Int -> Sudoku -> [Int]
	getRow x (Sudoku nonos) =
		let unOrderedRow = [ (j+b, n) | ((a,b), Nonomino tiles) <- nonos, (i, j, n) <- tiles, a + i == x]
		in map (\(x,y) -> y) $ sort unOrderedRow

	getCol :: Int -> Sudoku -> [Int]
	getCol x (Sudoku nonos) =
		let unOrderedRow = [ (i+a, n) | ((a,b), Nonomino tiles) <- nonos, (i, j, n) <- tiles, b + j == x]
		in map (\(x,y) -> y) $ sort unOrderedRow

