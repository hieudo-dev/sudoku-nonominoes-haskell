module Main where
	
	import System.Environment
	import Sudoku
	
	-- Imprime los elementos de una lista separados linea por linea
	printline [] = do print("-----------------------")
	printline (h:t) = do 
		putStrLn (show h)
		printline t
	
	main = do
		printline  board
		printline  test2
		

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

						
	test = head $ buildSudokus testNonos
	board = [ getRow i (head $ buildSudokus testNonos) | i <- [0..8]]
	test2 = choices test
