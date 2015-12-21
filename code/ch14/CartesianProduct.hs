-- file: ch14/CartesianProduct.hs
comprehensive xs ys = [(x,y) | x <- xs, y <- ys]

-- file: ch14/CartesianProduct.hs
monadic xs ys = do { x <- xs; y <- ys; return (x,y) }

-- file: ch14/CartesianProduct.hs
blockyDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- file: ch14/CartesianProduct.hs
blockyPlain xs ys =
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)

blockyPlain_reloaded xs ys =
    concat (map (\x ->
                 concat (map (\y ->
                              return (x, y))
                         ys))
            xs)

-- file: ch14/CartesianProduct.hs
wordCount = print . length . words =<< getContents
