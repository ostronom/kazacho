module Kazacho (kazacho) where

import Prelude
import Data.List
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Random (RANDOM, randomInt, random)
import Partial.Unsafe (unsafePartial)
import Data.Maybe


data Chain = Node (List String) (Number -> Chain)
           | Stop

geo = "шанхайский" : "прибалтийский" : "дальневосточный" : "филиппинский" :
     "лунный" : "калифорнийский" : "теночтитланский" : "антарктический" :
     "заполярный" : Nil

adj = "краснознамённый" : "имени Сербского" : "объединённый" : "сводный" :
     "ставропигиальный" : "отдельный" : "святоотеческий" : "трубопрокладочный" :
     "имени Сталина" : "девичий" : Nil

kaz = "казачий" : "староказачий" : Nil

squad = "батальон" : "отряд" : "круг" : Nil

geoChain = Node geo t
  where
    t x | x < 0.25 = geoChain
    t x | x < 0.5  = adjChain
    t _ = kazChain

adjChain = Node adj t
  where
    t x | x < 0.5 = adjChain
    t _ = kazChain

kazChain = Node kaz (\_ -> squadChain)

squadChain = Node squad (\_ -> Stop)

chooseWord :: forall eff. List String -> Eff (random :: RANDOM | eff) String
chooseWord words = do
  n <- randomInt 0 ((length words) - 1)
  pure $ unsafePartial $ fromJust $ index words n

joinList :: List String -> String
joinList = intercalate " "

genKazacho :: forall eff. List String -> Chain -> Eff (random :: RANDOM | eff) (List String)
genKazacho acc Stop = pure acc
genKazacho acc (Node ws t) = do
  word <- chooseWord ws
  next <- random
  genKazacho (snoc acc word) (t next)

kazacho' :: forall eff. Eff (random :: RANDOM | eff) (List String)
kazacho' = do
  res <- nub <$> genKazacho Nil geoChain
  if length res > 3 then pure res else kazacho'

kazacho :: forall eff. Eff (random :: RANDOM | eff) String
kazacho = kazacho' >>= pure <<< joinList
