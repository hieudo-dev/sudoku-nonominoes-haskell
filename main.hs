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
			board = [ getRow i sol | i <- [0..8]]
		in
			do putStrLn $ unlines $ sudokuToString sudoku
