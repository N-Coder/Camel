module Camel.Predict where

import           Camel.Data
import           Camel.Move
import           Camel.Util
import           Data.List  (delete)
import           Data.Map   (fromListWith, toList)
import           Lens.Micro ((^.))

predictOneMove :: Game -> [Game]
predictOneMove (Game (MoveResult b Nothing) []) = [Game (MoveResult b Nothing) []]
predictOneMove (Game (MoveResult b Nothing) cs) = concatMap predictCamel cs
  where
    predictCamel c = map (makeGameState c . moveCamelsBy b addOnTop c) diceSides
    makeGameState c mr = Game mr (delete c cs)

currentOrder :: Game -> [Camel]
currentOrder gs = boardToReverseList (gs ^. gameBoard)

positions :: [Game] -> [(Camel, Int)]
positions = concatMap ((\cs -> zip (reverse cs) [1 ..]) . currentOrder)

occurences :: [Game] -> [((Camel, Int), Int)]
occurences oc = toList (fromListWith (+) (zip (positions oc) (repeat 1)))

probabilities :: Fractional c => [Game] -> [(Camel, Int, c)]
probabilities oc = map stat (occurences oc)
  where
    stat ((camel, place), count) = (camel, place, fromIntegral count / fromIntegral (length oc))
