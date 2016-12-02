import Data.List(sortBy)

sortList :: [[a]] -> [[a]]
sortList = sortBy cmpLength where
    cmpLength x y = if lx == ly then EQ
                    else
                        if lx < ly then LT
                        else GT
                    where lx = length x
                          ly = length y