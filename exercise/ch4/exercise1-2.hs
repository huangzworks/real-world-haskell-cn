splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith = iter [] []
    where iter ans cur _ []     = 
            ans ++ (if null cur then [] else [reverse cur])
          iter ans cur f (x:xs) =
            if f x then
                if null cur
                then iter ans [] f xs
                else iter (ans++[reverse cur]) [] f xs
            else
                iter ans (x:cur) f xs
