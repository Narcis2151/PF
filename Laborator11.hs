data List a = Nil
            | Cons a (List a) deriving (Eq, Show)
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)
instance Functor List where
    fmap _ Nil = Nil
    fmap fu (Cons a b) = Cons (fu a) (fmap fu b) 
instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons a b) <*> x = (fmap a x) `append` (b <*> x)
        
f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)

test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

--data Maybe x = Nothing | Just x
noEmpty :: String -> Maybe String
noEmpty x = if x == "" then Nothing
            else Just x

noNegative :: Int -> Maybe Int
noNegative x = if x > 0 then Just x
             else Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w = if noEmpty n == Just n && noNegative a == Just a && noNegative w == Just w then Just Cow{name = n, age = a, weight = w}
                      else Nothing

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 n a w = if Just n == (++) <$> pure "" <*> noEmpty n && Just a == (+) <$> pure 0 <*> noNegative a && Just w == (+) <$> pure 0 <*> noNegative w then 
                        Just Cow{name = n, age = a, weight = w}
                        else Nothing

                        