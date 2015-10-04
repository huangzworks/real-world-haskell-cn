-- file: ch12/recordParity.hs
data AltParity a = AltEven {fromAltParity :: a}
             	| AltOdd  {fromAltParity :: a}
             	| AltNone {fromAltParity :: a}
               	deriving (Show)