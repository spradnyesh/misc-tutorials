doubleSmallNumber x = if x > 100
                         then x
                         else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2
doubleUs x y = doubleMe x + doubleMe y
