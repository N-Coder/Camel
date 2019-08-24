{-# LANGUAGE TemplateHaskell #-}

module Camel.Data where

import           Lens.Micro    (Lens', lens)
import           Lens.Micro.TH (makeLenses)

data Camel
  = Blue
  | Green
  | Orange
  | Yellow
  | White
  deriving (Enum, Eq, Show, Bounded, Ord)

camels :: [Camel]
camels = [minBound .. maxBound]

data CamelTower
  = Stack
      { bottom :: Camel
      , top    :: CamelTower
      }
  | None
  deriving (Eq)

type Board = [CamelTower]

instance Show CamelTower where
  show ct = "[" ++ show_ ct ++ "]"
    where
      show_ (Stack b None) = show b
      show_ (Stack b t)    = show b ++ " < " ++ show_ t
      show_ None           = ""
  showList [] s = "|<--->|" ++ s
  showList l s = "|<-----\n" ++ showList_ l ++ "----->|" ++ s
    where
      showList_ []    = ""
      showList_ (h:t) = show h ++ "\n" ++ showList_ t

towerToReverseList :: CamelTower -> [Camel]
towerToReverseList (Stack b t) = b : towerToReverseList t
towerToReverseList None        = []

boardToReverseList :: Board -> [Camel]
boardToReverseList = concatMap towerToReverseList

type Pusher = CamelTower -> CamelTower -> CamelTower

data MoveResult =
  MoveResult
    { _board    :: Board
    , _finisher :: Maybe (CamelTower, Int)
    }
  deriving (Show)

makeLenses ''MoveResult

winner :: Lens' MoveResult (Maybe CamelTower)
winner = lens getter setter
  where
    getter MoveResult {_finisher = Just (ct, _)} = Just ct
    getter MoveResult {_finisher = Nothing}      = Nothing
    setter mr (Just ct) = mr {_finisher = Just (ct, 0)}
    setter mr Nothing   = mr {_finisher = Nothing}

data Game =
  Game
    { _lastMove        :: MoveResult
    , _remainingCamels :: [Camel]
    }
  deriving (Show)

makeLenses ''Game

gameBoard :: Lens' Game Board
gameBoard = lastMove . board

gameWinner :: Lens' Game (Maybe CamelTower)
gameWinner = lastMove . winner
