-- file: ch14/Do.hs
doNotation1 =
    do act

-- file: ch14/Do.hs
translated1 =
    act

-- file: ch14/Do.hs
doNotation2 =
    do act1
       act2
       {- ... etc. -}
       actN

-- file: ch14/Do.hs
translated2 =
    act1 >>
    do act2
       {- ... etc. -}
       actN

finalTranslation2 =
    act1 >>
    act2 >>
    {- ... etc. -}
    actN

-- file: ch14/Do.hs
doNotation3 =
    do pattern <- act1
       act2
       {- ... etc. -}
       actN

-- file: ch14/Do.hs
translated3 =
    let f pattern = do act2
                       {- ... etc. -}
                       actN
        f _     = fail "..."
    in act1 >>= f

-- file: ch14/Do.hs
robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x

-- file: ch14/Do.hs
doNotation4 =
    do let val1 = expr1
           val2 = expr2
           {- ... etc. -}
           valN = exprN
       act1
       act2
       {- ... etc. -}
       actN

-- file: ch14/Do.hs
translated4 =
    let val1 = expr1
        val2 = expr2
        valN = exprN
    in do act1
          act2
          {- ... etc. -}
          actN

-- file: ch14/Do.hs
semicolon = do
  {
    act1;
    val1 <- act2;
    let { val2 = expr1 };
    actN;
  }

-- file: ch14/Do.hs
semicolonTranslated =
    act1 >>
    let f val1 = let val2 = expr1
                 in actN
        f _ = fail "..."
    in act2 >>= f
