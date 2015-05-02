-- file: ch06/NewtypeDiff.hs
-- 可以：任意数量的构造器和字段
data TwoFields = TwoFields Int Int

-- 可以：一个字段
newtype Okay = ExactlyOne Int

-- 可以：可以使用类型变量
newtype Param a b = Param (Either a b)

-- 可以：可以使用记录语法
newtype Record = Record {
        getInt :: Int
    }

-- 不可以：没有字段
newtype TooFew = TooFew

-- 不可以：多于一个字段
newtype TooManyFields = Fields Int Int

-- 不可以：多于一个构造器
newtype TooManyCtors = Bad Int
                     | Worse Int