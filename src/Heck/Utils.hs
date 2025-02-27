module Heck.Utils where

import Control.Monad (zipWithM, zipWithM_)
import System.Exit (exitFailure)

errorExit :: String -> IO a
errorExit msg = do
  putStrLn msg
  exitFailure

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f = zipWith f [0 ..]

mapIndexedM :: (Monad m) => (Int -> a -> m b) -> [a] -> m [b]
mapIndexedM f = zipWithM f [0 ..]

mapIndexedM_ :: (Monad m) => (Int -> a -> m b) -> [a] -> m ()
mapIndexedM_ f = zipWithM_ f [0 ..]

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = concat <$> mapM f as

replace :: (Eq a) => a -> a -> [a] -> [a]
replace from to =
  let replacer el = if el == from then to else el
   in map replacer
