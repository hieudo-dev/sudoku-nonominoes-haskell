module Sudoku where

	------------------------------- Tipos ------------------------------

	data Nonomino = Nonomino [(Int, Int)]

	------------------------------ Funciones ---------------------------

	origen (Nonomino (h:t)) = h

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
	buildSudokus nonos = [ list | list <- permutations [1..9], 
											validOrder list nonos]

	validOrder :: [Int] -> [Nonomino] -> Bool
	validOrder perm nonos = True

	getOccupied list = [(a+i, b+j) | ((a, b), Nonomino points) <- list,
												(i,j) <- points]