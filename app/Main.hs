module Main where

import           Camel.Lib
import           Control.Monad.Trans.State (execStateT)

actions = do
  moveRandom
  moveRandom
  moveRandom
  moveRandom
  moveRandom
  restartRound

main :: IO ()
main = do
  g <- newGame 15
  print g
  g <- execStateT actions g
  print g
