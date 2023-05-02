module Main where

import HaskellSay (haskellSay)
import Data.List
import Control.Monad.CSP

solveVergleich :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
solveVergleich values = oneCSPSolution $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1..4] else [a])) values
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  sequence_ [assertSquareConstraints dvs x y | x <- [0,2], y <- [0,2]]
  return dvs
    where assertRowConstraints = mapAllPairsM_ (constraint2 (/=))
          assertSquareConstraints dvs i j =
            mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+1], y <- [j..j+1]]

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l)  = mapM_ (f a) l >> mapAllPairsM_ f l

--constrts = [[[() ,(<)],[() ,() ],[() ,(>)],[() ,() ]],
--            [[(>),(>)],[(>),() ],[(<),(<)],[(<),() ]],
--	          [[() ,(>)],[() ,() ],[() ,(<)],[() ,() ]],
--	          [[(<),(>)],[(>),() ],[(>),(>)],[(<),() ]]]

values = [[0,0,0,0],
          [0,0,0,0],
	        [0,0,0,0],
	        [0,0,0,0]]

main :: IO ()
main = do
    haskellSay "Ola mundo!"
    putStrLn $ show $ solveVergleich $ values
