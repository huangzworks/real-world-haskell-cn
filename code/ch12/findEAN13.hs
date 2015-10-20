-- file: ch12/Barcode.hs
findEAN13 :: Pixmap -> Maybe [Digit]
findEAN13 pixmap = withRow center pixmap (fmap head . findMatch)
  where (_, (maxX, _)) = bounds pixmap
        center = (maxX + 1) `div` 2