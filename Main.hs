module Main where

import HaskellSay (haskellSay)
import Data.List
import Control.Monad.CSP

solveKojun :: [[Int]] -> [[Int]] -> Result [[DV [[Int]] Int]]
solveKojun values areas = oneCSPSolution $ do
  dvs <- mapM (mapM (mkDV)) (map (map (getPossibilities)) [[[values !! x !! y, areas !! x !! y] | y <- [0..5]] | x <- [0..5]])
  mapM_ applyNotEqConstraint [[dvs !! x !! y | x <- [0..5], y <- [0..5], areas !! x !! y == z] | z <- [1..(maximum (concat areas))]]
  mapM_ (mapM_ applyNotEqConstraint) [[[dvs !! z !! h, dvs !! x !! y] | x <- [z-1..z+1], y <- [h-1..h+1], (xor (x == z) (y == h)), x >= 0, y >= 0, x < 6, y < 6] | z <- [0..5], h <- [0..5]]
  mapM_ applyGreaterThanConstraint [[dvs !! x !! y, dvs !! (x+1) !! y] | x <- [0..4], y <- [0..5], (areas !! x !! y) == (areas !! (x+1) !! y)]
  return dvs
    where applyNotEqConstraint = mapAllPairsM_ (constraint2 (/=))
          applyGreaterThanConstraint [a, b] = constraint2 (>) a b

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l)  = mapM_ (f a) l >> mapAllPairsM_ f l

areaSize :: (Eq a, Num a) => a -> [[a]] -> Int
areaSize area areaz = length $ filter (== area) $ concat areaz

getPossibilities ::  [Int] -> [Int]
getPossibilities [] = []
getPossibilities [value, area] = if value == 0 then [1..(areaSize area areas)] else [value]

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

values = [[0, 0, 4, 0, 2, 0],
          [0, 0, 3, 0, 0, 0],
          [1, 4, 0, 4, 0, 0],
          [0, 5, 0, 0, 0, 2],
          [0, 0, 0, 0, 3, 0],
          [6, 2, 0, 2, 0, 5]]

areas = [[1, 2, 2, 2, 3, 4],
         [1, 5, 2, 3, 3, 3],
         [1, 1, 6, 3, 7, 7],
         [8, 9, 6,10,10, 7],
         [8, 9, 9,11,11, 7],
         [9, 9, 9,11,11,11]]

main :: IO ()
main = do
    haskellSay "Ola mundo!"
    putStrLn $ show $ solveKojun values areas
