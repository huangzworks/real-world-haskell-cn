-- file: ch04/identity.hs

identity :: [a] -> [a]
identity xs = foldr (:) [] xs
