-- file: ch06/listToJValues.hs
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue