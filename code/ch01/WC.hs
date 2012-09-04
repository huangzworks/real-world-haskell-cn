-- file: ch01/WC.hs
-- 以 "--" 开头的都是注释

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
