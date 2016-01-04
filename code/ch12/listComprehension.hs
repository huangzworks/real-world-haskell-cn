-- file: ch12/Barcode.hs
-- our original
zip [distance d (scaleToOne ps) | d <- srl] digits

-- the same expression, expressed without a list comprehension
zip (map (flip distance (scaleToOne ps)) srl) digits

-- the same expression, written entirely as a list comprehension
[(distance d (scaleToOne ps), n) | d <- srl, n <- digits]