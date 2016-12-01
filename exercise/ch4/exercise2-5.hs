groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f s = uncurry (:) ans
    where
      ans = foldr step ([], []) s
      step x ([],  acc) = ([x], acc)
      step x (cur@(h:hs), acc) = if f x h
                                 then (x:cur, acc)
                                 else ([x], cur:acc)