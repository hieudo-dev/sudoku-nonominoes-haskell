module Main where
	import System.Environment
	import Data.List

	import Types
	import Matching
	import Solution
	import Print

	import Tests

   --------------------------------------- MAIN -----------------------------------------------------

	main =
		let
			sudoku = head $ buildSudokus testNonominos
			sol = solutionSudoku sudoku
		in
			do 
				putStrLn " ++ SUDOKU ARMADO ++"
				putStrLn $ unlines $ sudokuToString sudoku
				putStrLn " ++ SUDOKU SOLUCIONADO ++"
				putStrLn $ unlines $ sudokuToString sol
