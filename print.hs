module Print where
	import Data.List
	import Types
	import Solution

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
				then
					if j /= 8 then
						if sameNonomino (n,j) (n,j+1) sudoku then "   " ++ separator (j+1)
						else "  |" ++ separator (j+1)
					else "  |" ++ separator (j+1)
				else
					if j /= 8 then "---" ++ separator (j+1)
					else "--|" ++ separator (j+1)
		in
			["|" ++ rowStr 0, "|" ++ separator 0]