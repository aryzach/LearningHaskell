


data Parser a = P (String -> Maybe (a, String))


runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = case runParser p s of
 Just (a, "") -> Just a
 _ -> Nothing


noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser a = P (\x -> Just (a, x))

instance Functor Parser where
 fmap f p = P p'
  where 
   p' input = case runParser p input of
    Nothing -> Nothing
    Just (a, str) -> Just (f a, str)
{-
instance Applicative Parser where
 pure = pureParser
 P f <*> P x = P fx
  where
   fx input = case f input of 
    Nothing -> Nothing
    Just (a, fstr) -> case x fstr of
     Nothing -> Nothing
     Just (b, xstr) -> Just (a b, xstr)  
-}
instance Applicative Parser where
 pure = pureParser
 f <*> x = P $ \input -> do
  (f1, fstr) <- runParser f input
  (x1, xstr) <- runParser x fstr
  return (f1 x1, xstr)

instance Monad Parser where
    return = pureParser
    fa >>= k = P $ \input -> do
     (fa1, fastr) <- runParser fa input
     runParser (k fa1) fastr

anyChar :: Parser Char
anyChar = P $ \x -> case x of
 [] -> Nothing
 (c:cs) -> Just (c, cs)

char :: Char -> Parser ()
char c = do
 c' <- anyChar
 if c == c' 
 then return ()
 else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
 c' <- anyChar
 if c' /= c 
 then return c'
 else noParser

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P $ \input 
 -> case runParser p1 input of
     Just x -> Just x
     Nothing -> runParser p2 input

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy pa p = many $ orElse pa p








