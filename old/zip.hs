


zipt :: [a]->[b]->[(a,b)]
zipt a [] = []
zipt [] b = []
zipt (a:ax) (b:bx) = (a,b):(zipt ax bx)





