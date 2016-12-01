graham :: [Point] -> [Point]
graham graph = foldl makeStack [head . tail $ pList, p0] (tail . tail $ pList)
    where
        p0 = head . sortBy yco $ graph
        yco p1 p2 = if y1 == y2 then
                        if x1 < x2 then LT
                        else GT
                    else 
                        if y1 < y2 then LT
                        else GT
            where
                x1 = px p1
                x2 = px p2
                y1 = py p1
                y2 = py p2
        pList = sortBy angle $ graph
            where
                angle p1 p2 = if dir == TurnLeft || dir == Straight then LT else GT
                    where dir = calcTurn p0 p1 p2
        makeStack [pa] pn = pn : [pa]
        makeStack acc@(pa:pb:ps) pn@(Point x y) = if dir == TurnLeft || dir == Straight
                                                  then pn:acc
                                                  else makeStack (tail acc) pn
                                                  where
                                                        dir = calcTurn pb pa pn