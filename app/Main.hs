module Main (main) where

import Lib
import System.Environment
import System.IO

selectedColor :: Color
selectedColor = Color {r = 200, g = 103, b = 255}

options :: [String]
options = ["cool option 1", "lame option 2", "option 3", "none of the above", "aaaaaaa", "aaaaaaaaaaaaaaaaaaaaa", "no", "why"]

moveSelectionDown :: Int -> IO ()
moveSelectionDown selected = do
  redrawLine $ options !! selected

  clearLine
  putStr $ makeRgb selectedColor
  putStr $ options !! (selected + 1)
  putStrLn $ moveUp 1

moveSelectionUp :: Int -> IO ()
moveSelectionUp selected = do
  redrawLine $ options !! selected
  putStrLn $ moveUp 3

  clearLine
  putStr $ makeRgb selectedColor
  putStr $ options !! (selected - 1)
  putStrLn $ moveUp 1

printOptions :: [String] -> IO ()
printOptions [] = return ()
printOptions (x : xs) = do
  putStrLn x
  printOptions xs

editSelection :: Int -> Char -> IO Int
editSelection i c
  | c == 'j' && i < length options - 1 = do
      moveSelectionDown i
      input <- getChar
      editSelection (i + 1) input
  | c == 'k' && i > 0 = do
      moveSelectionUp i
      input <- getChar
      editSelection (i - 1) input
  | c == '\n' = return i
  | otherwise = do
      clearLine
      putStr $ makeRgb selectedColor
      putStr $ options !! i
      putStrLn $ moveUp 1

      input <- getChar
      editSelection i input

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  putStrLn "\ESC[?25l" -- Make cursor invisible
  printOptions options

  putStr $ moveUp $ length options
  clearLine
  putStr $ makeRgb selectedColor
  putStr $ head options
  putStrLn $ moveUp 1

  input <- getChar
  ans <- editSelection 0 input

  putStrLn "\ESC[?25h"
  print ans
