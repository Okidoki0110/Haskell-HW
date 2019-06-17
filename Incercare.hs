module Incercare where

changeNth :: Int->(a->a)->[a]->[a]
changeNth n change (x:xs)
     | n == 0 = (change x):xs
     | otherwise = x:changeNth (n-1) change xs
   
modifyXY :: Int -> Int -> (a->a) -> [[a]]->[[a]]
modifyXY x y f nList = changeNth y (changeNth x f) nList

