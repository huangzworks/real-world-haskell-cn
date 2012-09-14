-- file: ch06/roleEq.hs

data Role = Boss | Manager | Employee

roleEq :: Role -> Role -> Bool
roleEq Employee Employee = True
roleEq Manager  Manager  = True
roleEq Boss     Boss     = True
roleEq _        _        = False
