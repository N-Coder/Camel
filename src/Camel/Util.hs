module Camel.Util where

import           System.Random

diceSides :: [Int]
diceSides = [1, 2, 3]

extract :: Int -> [a] -> ([a], a)
extract idx xs = (lft ++ rgt, elem)
  where
    (lft, elem:rgt) = splitAt idx xs

-- import Data.Random.Extras (choice) without all the dependencies
choice :: [a] -> IO a
choice l = do
  i <- randomRIO (0, length l - 1)
  return $ l !! i

choiceExtract :: [a] -> IO ([a], a)
choiceExtract l = do
  i <- randomRIO (0, length l - 1)
  return $ extract i l
