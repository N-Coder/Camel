module Camel.Move where

import           Camel.Data
import           Camel.Util
import           Lens.Micro (over)

splitTower :: CamelTower -> Camel -> (CamelTower, Maybe CamelTower)
splitTower None _ = (None, Nothing)
splitTower (Stack b t) c
  | b == c = (None, Just (Stack c t))
  | otherwise = (Stack b bot, top)
  where
    (bot, top) = splitTower t c

popTower :: Board -> Camel -> (Board, Maybe (CamelTower, Int))
popTower [] _ = ([], Nothing)
popTower (h:t) c =
  let (bot, top) = splitTower h c
   in case top of
        Nothing -> (h : resB, Just (resT, resI + 1))
          where (resB, Just (resT, resI)) = popTower t c
        Just x -> (bot : t, Just (x, 0))

addOnTop :: Pusher
addOnTop None ts           = ts
addOnTop (Stack b None) ts = Stack b ts
addOnTop (Stack b t) ts    = Stack b (addOnTop t ts)

addBelow :: Pusher
addBelow top bot = addOnTop bot top

moveTowerTo :: Board -> Pusher -> CamelTower -> Int -> MoveResult
moveTowerTo (h:t) op ct i
  | i > 0 = over board (h :) $ moveTowerTo t op ct (i - 1)
  | otherwise = MoveResult (op h ct : t) Nothing
moveTowerTo [] op ct i = MoveResult [] (Just (ct, i))

moveCamelsTo :: Board -> Pusher -> Camel -> Int -> MoveResult
moveCamelsTo board op camel i =
  let (b, Just (t, _)) = popTower board camel
   in moveTowerTo b op t i

moveCamelsBy :: Board -> Pusher -> Camel -> Int -> MoveResult
moveCamelsBy board op camel i =
  let (b, Just (t, p)) = popTower board camel
   in moveTowerTo b op t (p + i)
