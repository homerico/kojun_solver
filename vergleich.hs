import Data.List
import Control.Monad.CSP

solveVergleich :: (Enum a, Eq a, Num a) => [[[(a -> a -> Bool), (a -> a -> Bool)]]] -> [[a]] -> [[a]]
solveVergleich contrts values = oneCSPSolution $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1..4] else [a])) values
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  

constrts = [[[() ,(<)],[() ,() ],[() ,(<)],[() ,() ]],
            [[(<),(<)],[(>),() ],[(>),(>)],[(<),() ]],
	          [[() ,(>)],[() ,() ],[() ,(<)],[() ,() ]],
	          [[(<),(>)],[(>),() ],[(>),(>)],[(<),() ]]]

values = [[0,0,0,0],
          [0,0,0,0],
	        [0,0,0,0],
	        [0,0,0,0]]
