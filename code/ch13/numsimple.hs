-- file: ch13/numsimple.hs
-- 我们支持的操作符
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

{- 核心符号操作类型（core symbolic manipulation type） -}
data SymbolicManip a =
          Number a           -- Simple number, such as 5
        | Arith Op (SymbolicManip a) (SymbolicManip a)
          deriving (Eq, Show)

{- SymbolicManip 是 Num 的实例。定义 SymbolicManip 实现 Num 的函数。如(+)等。 -}
instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b
    a * b = Arith Mul a b
    negate a = Arith Mul (Number (-1)) a
    abs a = error "abs is unimplemented"
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)
