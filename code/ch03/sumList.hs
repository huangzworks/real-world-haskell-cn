-- file:: ch03/sumList.hs
sumList (x:xs) = x + sumList xs
sumList []  = 0
