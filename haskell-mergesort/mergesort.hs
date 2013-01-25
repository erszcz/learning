import Data.List as L

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort ys) (mergesort zs)
    where (ys, zs) = L.splitAt (quot (L.length xs) 2) xs

merge [] [] = []
merge ys [] = ys
merge [] zs = zs
merge (y:ys) (z:zs) | y <= z = [y] ++ merge ys (z:zs)
                    | y >  z = [z] ++ merge (y:ys) zs
