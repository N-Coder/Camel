module Camel.Lib
  ( GameState
  , newGame
  , moveCamelBy
  , moveRandom
  , restartRound
  ) where

import           Camel.Data
import           Camel.Move
import           Camel.Predict
import           Camel.Util
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
import           Data.List                 (delete)
import           Lens.Micro

type GameState = StateT Game IO ()

placeCamel :: Board -> Camel -> IO Board
placeCamel b c = do
  i <- choice diceSides
  putStrLn $ "place " ++ show c ++ " at " ++ show i
  return $ moveTowerTo b addOnTop (Stack c None) (i - 1) ^. board

newGame :: Int -> IO Game
newGame n = do
  board <- return $ replicate n None
  board <- foldM placeCamel board camels
  return $ Game (MoveResult board Nothing) camels

moveCamelBy :: Camel -> Int -> GameState
moveCamelBy c i = do
  lift $ putStrLn $ "move " ++ show c ++ " by " ++ show i
  g <- get
  -- TODO check state
  modify $ lastMove .~ moveCamelsBy (g ^. gameBoard) addOnTop c i
  modify $ remainingCamels .~ delete c (g ^. remainingCamels)

moveRandom :: GameState
moveRandom = do
  g <- get
  c <- lift $ choice (g ^. remainingCamels)
  i <- lift $ choice diceSides
  moveCamelBy c i

restartRound :: GameState
restartRound = do
  g <- get
  -- TODO check state
  lift $ putStrLn $ "restart round, current leaderboard is " ++ show (reverse $ currentOrder g)
  modify $ remainingCamels .~ camels
