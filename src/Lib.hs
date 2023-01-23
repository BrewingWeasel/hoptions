module Lib
  ( moveDown,
    moveUp,
    Color (..),
    makeRgb,
    goTo,
    clearLine,
    resetStyle,
    redrawLine,
  )
where

data Color = Color
  { r :: Int,
    g :: Int,
    b :: Int
  }

goTo :: Int -> Int -> String
goTo x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "f"

makeRgb :: Color -> String
makeRgb Color {r = rVal, g = gVal, b = bVal} = "\ESC[38;2;" ++ show rVal ++ ";" ++ show gVal ++ ";" ++ show bVal ++ "m"

moveUp :: Int -> String
moveUp n = "\ESC[" ++ show n ++ "F"

moveDown :: Int -> String
moveDown n = "\ESC[" ++ show n ++ "E"

clearLine :: IO ()
clearLine = do
  putStr "\ESC[2K"
  putStr "\ESC[0G"

resetStyle :: IO ()
resetStyle = do
  putStr "\ESC[0m"

redrawLine :: String -> IO ()
redrawLine str = do
  clearLine
  resetStyle
  putStrLn str
