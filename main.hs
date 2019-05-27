module Main where
	import System.Environment
	import Data.List
	import Sudoku
	
	-- Imprime los elementos de una lista separados linea por linea
	printline [] = do putStrLn "------------------"
	printline (h:t) = do 
		putStrLn (show h)
		printline t

	------------------------------------- SUDOKU PRINTING ------------------------------------------

	sameNonomino (i, j) (u, v) (Sudoku nonos) = 
		let s = [ Nonomino p | ((x,y), Nonomino p) <- nonos, 
										find (\(a,b,n) -> (a+x,b+y) == (i,j)) p /= Nothing &&
										find (\(a,b,n) -> (a+x,b+y) == (u,v)) p /= Nothing ] 
		in s /= [] 

	sudokuToString sudoku = 
		let
			rows = [ getRow i sudoku | i <- [0..8]] 
			unfold sudoku 9  = []
			unfold sudoku n = rowToString n (rows !! n) sudoku ++ unfold sudoku (n+1)
		in "|--------------------------|": unfold sudoku 0

	rowToString n row sudoku = 
		let
			rowStr 9 = ""
			rowStr j = 
				if sameNonomino (n,j) (n,j+1) sudoku 
				then " " ++ show (row !! j) ++ " " ++ rowStr (j+1)
				else " " ++ show (row !! j) ++ "|" ++ rowStr (j+1)
			separator 9 = ""
			separator j = 
				if sameNonomino (n,j) (n+1,j) sudoku 
				then if j /= 8 then "   " ++ separator (j+1) else "  |" ++ separator (j+1)
				else if j /= 8 then "---" ++ separator (j+1) else "--|" ++ separator (j+1)
		in
			["|" ++ rowStr 0, "|" ++ separator 0]

------------------------------------------ MAIN -----------------------------------------------------

	
	main = 
		let 
			sudoku = head $ buildSudokus testNonos
			sol = solutionSudoku sudoku
			board = [ getRow i sol | i <- [0..8]]
		in 
			do putStrLn $ unlines $ sudokuToString sudoku
	
			
	testNonos = [  
		Nonomino [(0,0, 0), (0,1, 0), (0,2, 0), (0,3, 1), (0,4, 0), (1,1, 0), (1,2, 0), (1,3, 0), (2,2, 0)],
		Nonomino [(0,0, 2), (0,1, 0), (0,2, 3), (0,3, 0), (1,-1, 0), (1,0, 0), (1,1, 0), (1,2, 9), (1,3, 0)],
		Nonomino [(0,0, 6), (1,0, 0), (1,1, 0), (2,0, 8), (2,1, 0), (2,2, 0), (2,3, 0), (1,3, 0), (3,2, 4)],
		Nonomino [(0,0, 0), (0,1, 0), (0,2, 0), (0,3, 8), (0,4, 0), (1,3, 0), (1,4, 0), (2,3, 0), (2,4, 0)],
		Nonomino [(0,0, 7), (1,0, 0), (1,-1, 5), (2,-1, 0), (2,-2, 2), (3,-2, 9), (4,-2, 1), (5,-2, 0), (5,-3, 0)],
		Nonomino [(0,0, 4), (0,1, 0), (1,0, 6), (1,1, 0), (2,0, 0), (2,1, 0), (2,-1, 0), (3,1, 0), (4,1, 0)],
		Nonomino [(0,0, 0), (0,1, 0), (1,0, 0), (1,1, 0), (2,0, 3), (2,1, 0), (3,0, 4), (3,1, 0), (4,0, 0)],
		Nonomino [(0,0, 0), (0,1, 0), (1,0, 0), (1,1, 0), (2,0, 0), (2,1, 0), (3,0, 7), (3,1, 0), (3,-1, 0)],
		Nonomino [(0,0, 8), (0,1, 0), (0,2, 5), (1,0, 7), (1,1, 0), (1,2, 9), (2,0, 3), (2,1, 6), (2,2, 0)]
		]

						