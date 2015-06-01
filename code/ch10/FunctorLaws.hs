-- file: ch10/FunctorLaws.hs
fmap id       ==  id
fmap (f . g)  ==  fmap f . fmap g