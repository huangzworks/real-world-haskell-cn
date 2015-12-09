-- file: ch06/AutomaticDerivation.hs
data CannotShow = CannotShow
--                  deriving (Show) --原书例子这行是多余的,不然和下面自己的注释打架了.

-- will not compile, since CannotShow is not an instance of Show
data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)