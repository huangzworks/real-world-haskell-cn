-- file: ch04/safeHead.hs

safeHead (x:_) = Just x
safeHead [] = Nothing
