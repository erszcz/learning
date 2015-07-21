import Data.List as L

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort $ take l xs) (mergesort $ drop l xs)
    where l = length xs `quot` 2

merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : (merge xs (y:ys))
                    | x >  y = y : (merge (x:xs) ys)
