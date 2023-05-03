module Main where

import HaskellSay (haskellSay)
import Data.List
import Control.Monad.CSP

solveVergleich :: (Enum a, Eq a, Num a) => [[Maybe (a -> a -> Bool)]] -> [[Maybe (a -> a -> Bool)]] -> [[a]] -> [[a]]
solveVergleich rowCtrs colCtrs values = oneCSPSolution $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1..4] else [a])) values
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  sequence_ [assertSquareConstraints dvs x y | x <- [0,2], y <- [0,2]]
  sequence_ [assertComparativeConstraints (rowCtrs !! i) (dvs !! i) | i <- [0..3]]
  sequence_ [assertComparativeConstraints ((transpose colCtrs) !! i) ((transpose dvs) !! i) | i <- [0..3]]
  return dvs
    where assertRowConstraints = mapAllPairsM_ (constraint2 (/=))
          assertSquareConstraints dvs i j =
            mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+1], y <- [j..j+1]]
          assertComparativeConstraints ctrs row = mapAllNeighborsM_ (fmap constraint2 ctrs) row

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l)  = mapM_ (f a) l >> mapAllPairsM_ f l

mapAllNeighborsM_ :: Monad m => [Maybe (a -> a -> m b)] -> [a] -> m ()
mapAllNeighborsM_ f []     = return()
mapAllNeighborsM_ f (_:[]) = return ()
mapAllNeighborsM_ (f:r) (a:b:l)  = f <*> Just a <*> Just b >> mapAllNeighborsM_ r (b:l)


rowCtrs = [
              [Just (<), Nothing, Just (>), Nothing],
              [Just (>), Nothing, Just (<), Nothing],
              [Just (>), Nothing, Just (<), Nothing],
	      [Just (>), Nothing, Just (>), Nothing]]

colCtrs = [
              [Just (>), Just (>), Just (<), Just (<)],
              [Nothing , Nothing, Nothing, Nothing],
	      [Just (<), Just (>), Just (>), Just (<)],
              [Nothing , Nothing, Nothing, Nothing]]

values = [[0,0,0,0],
          [0,0,0,0],
          [0,0,0,0],
          [0,0,0,0]]

main :: IO ()
main = do
    haskellSay "Ola mundo!"
    putStrLn $ show $ solveVergleich rowCtrs colCtrs values
