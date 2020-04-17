
 
lengthCompare :: String -> String -> Ordering  
lengthCompare a b = 
 let la = length a 
     lb = length b
     lcompare = compare la lb
 in if lcompare == EQ
    then compare a b
    else lcompare         


lengthCompare' :: String -> String -> Ordering
lengthCompare' a b = (compare (length a) (length b)) <> (compare a b)


lengthCompare'' :: String -> String -> Ordering
lengthCompare'' a b = (compare (length a) (length b)) <> (compare (vowels a) (vowels b)) <> (compare a b)
 where vowels = length . filter (`elem` "aeiou")

instance Monoid a => Semigroup (Maybe a) where
 (Just a) <> (Just b) = Just (a <> b) 
 Nothing <> b = b
 x <> Nothing = Nothing <> x

instance Monoid a => Monoid (Maybe a) where  
 mempty = Nothing















 
