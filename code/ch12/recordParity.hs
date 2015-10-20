-- file: ch12/Barcode.hs
data AltParity a = AltEven {fromAltParity :: a}
             	| AltOdd  {fromAltParity :: a}
             	| AltNone {fromAltParity :: a}
               	deriving (Show)