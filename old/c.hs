




isPal (a:[]) = True
isPal (a:b:[]) = a == b
isPal a = ((head a) == (last a)) && (isPal (init (tail a)))

con (d,c) 
  |c == 'm' = ((d*2),'y')
  |c == 'y' = (((/) d 2), 'm') 
