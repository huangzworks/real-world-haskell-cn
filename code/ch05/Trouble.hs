-- file: ch05/Trouble.hs

import Data.Char (toUpper)

upcaseFirst (c:cs) = toUpper c  -- 这里忘记了 ":cs"

camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
