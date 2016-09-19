module Main where

import Prelude
import Data.List
import Data.Maybe
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt, random)
import Partial.Unsafe (unsafePartial)

data Part = RepeatableParts (List String)
          | UniqueParts (List String)

parts :: List Part
parts = fromFoldable [
  UniqueParts (fromFoldable ["Шанхайский",  "Прибалтийский", "Дальневосточный", "Филиппинский", "Лунный", "Калифорнийский", "Теночтитланский", "Антарктический", "Ботсванский"]),
  RepeatableParts (fromFoldable ["краснознамённый", "имени Сербского", "объединённый", "сводный", "ставропигиальный", "отдельный"]),
  UniqueParts (fromFoldable ["казачий"]),
  UniqueParts (fromFoldable ["батальон", "отряд", "круг"])
]

chooseWord :: forall eff. List String -> Eff (random :: RANDOM | eff) String
chooseWord words = do
  n <- randomInt 0 ((length words) - 1)
  pure $ unsafePartial $ fromJust $ index words n

kazacho :: forall eff. Eff (random :: RANDOM | eff) String
kazacho = go Nil parts >>= pure <<< joinList
  where
    joinList lst = intercalate " " lst
    go :: forall eff. List String -> List Part -> Eff (random :: RANDOM | eff) (List String)
    go acc Nil = pure acc
    go acc (part : xs) = case part of
      RepeatableParts Nil -> go acc xs
      RepeatableParts words -> do
        word <- chooseWord words
        let ws   = RepeatableParts $ filter (\w -> w /= word) words
            acc' = snoc acc word
        n <- random
        if n >= 0.75 then go acc' (ws : xs) else go acc' xs
      UniqueParts words -> chooseWord words >>= \w -> go (snoc acc w) xs

main = kazacho >>= logShow
