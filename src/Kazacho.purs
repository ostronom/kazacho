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

p1 = "шанхайский" : "прибалтийский" : "дальневосточный" : "филиппинский" :
     "лунный" : "калифорнийский" : "теночтитланский" : "антарктический" : Nil

p2 = "краснознамённый" : "имени Сербского" : "объединённый" : "сводный" :
     "ставропигиальный" : "отдельный" : "святоотеческий" : "трубопрокладочный" :
     "имени Сталина" : Nil

p3 = "казачий" : Nil

p4 = "батальон" : "отряд" : "круг" : Nil

chain1 = Node p1 t
  where
    t x | x < 0.25 = chain1
    t x | x < 0.5  = chain2
    t _ = chain3

chain2 = Node p2 t
  where
    t x | x < 0.5 = chain2
    t _ = chain3

chain3 = Node p3 (\_ -> chain4)

chain4 = Node p4 (\_ -> Stop)

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
  res <- nub <$> genKazacho Nil chain1
  if length res > 3 then pure res else kazacho'

kazacho :: forall eff. Eff (random :: RANDOM | eff) String
kazacho = kazacho' >>= pure <<< joinList
