import Data.List as L

qsort [] = []
qsort [x] = [x]
qsort (x:xs) = (qsort lesser) ++ (x:(qsort greater))
    where (lesser, greater) = L.partition (<x) xs
