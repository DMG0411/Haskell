import Data.List
import Distribution.Compiler (AbiTag(NoAbiTag))
--Lab 1

myid x = x

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

myMax :: Int -> Int -> Int
myMax x y = if x <= y then y else x

intMax :: Int -> Int -> Int -> Int
intMax x y z
    | x > y && x > z = x
    | y > x && y > z = y
    | otherwise = z

mySum :: Int -> Int
mySum x = if x <= 0 then 0 else x + mySum (x - 1)

fibonacci x
    | x == 1 = 1
    | x == 0 = 0
    | otherwise = (+) (fibonacci ((-)x 1))(fibonacci((-)x 2))

cmmdc a b
    | (||)((==) a 0)((==) b 0) = 0
    | (==) a b = a
    | (>) a b = cmmdc ((-) a b) b
    | otherwise = cmmdc a ((-) b a)


--------------------------------------------------------------------
-- Lab 2

-- 1
andPrim :: Bool -> Bool -> Bool
andPrim False _ = False
andPrim _ False = False
andPrim _ _ = True

orPrim :: Bool -> Bool -> Bool
orPrim False _ = True
orPrim _ False = True
orPrim _ _ = True

negPrim :: Bool -> Bool
negPrim False = True
negPrim _ = False

nandPrim :: Bool -> Bool -> Bool
nandPrim False _ = True
nandPrim _ False = True
nandPrim _ _ = False

norPrim :: Bool -> Bool -> Bool
norPrim False _ = False
norPrim _ False = False
norPrim _ _ = False

implication :: Bool -> Bool -> Bool
implication p q = not p || q

doubleImplication :: Bool -> Bool -> Bool
doubleImplication p q = (p == q) && (q == p)

-- 2
hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | n `mod` a == 0 = True
hasDivisors n a b | n `mod` b == 0 = True

isPrime :: Integer -> Bool
isPrime n = hasDivisors n 1 n

-- 3
minusCmmdc a b
    | (||)((==) a 0)((==) b 0) = 0
    | (==) a b = a
    | (>) a b = minusCmmdc ((-) a b) b
    | otherwise = minusCmmdc a ((-) b a)

divCmmdc a b
    | (==) b 0 = a
    | otherwise = divCmmdc b (a `mod` b)

binaryCmmdc a b
    | (==) a 0 = b
    | (==) b 0 = a
    | even a && even b = 2*binaryCmmdc (a`div`2) (b`div`2)
    | even a && odd b = binaryCmmdc (a`div`2) b
    | odd a && even b = binaryCmmdc a (b`div`2)
    | odd a && odd b = binaryCmmdc (abs(a - b)) (min a b)

-- 5
fibo :: Integer -> Integer
fibo x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = (+) (fibo (x - 1))(fibo(x - 2))

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n-1) (b-a) a
-- a si b sunt doua numere Fibonacci consecutive

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

-- Implementati varianta care functioneaza ın timp O(log(n)).

-- Folositi un rationament ecuational pentru a arata ca fibo si fibo’ sunt echivalente
-- functional

---------------------------------------------------------------------------------------

-- Lab 3
-- 1

data Colors = Red
            | Blue
            | Green
            deriving (Show)

data MobileDevice = Smartphone Colors
                  | Laptop Colors
                  | Tablet Colors
                  deriving (Show)

descriere :: MobileDevice -> Colors
descriere (Laptop color) = color
descriere (Tablet color) = color
descriere (Smartphone color) = color

main = do
    let smartphone = Smartphone Blue
    let laptop = Laptop Red
    let tablet = Tablet Green
    putStrLn $ "Smartphone color is " ++ show(descriere smartphone)
    putStrLn $ "The color of tablet is " ++ show(descriere tablet)
    putStrLn $ "The laptop is " ++ show(descriere laptop)

--2
data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)

isBST :: Arb -> Bool
isBST Frunza = True
isBST (Nod val left right) = isBST' left (minim(Nod val left right)) val && isBST' right val (maxim(Nod val left right))
  where
    isBST' Frunza _ _ = True
    isBST' (Nod val left right) minVal maxVal =
      val > minVal && val < maxVal && isBST' left minVal val && isBST' right val maxVal

search :: Arb -> Integer -> Bool
search Frunza _ = False
search (Nod val left right) x
    | x == val = True
    | x < val = search left x
    | x > val = search right x

insert :: Arb -> Integer -> Arb
insert Frunza x = Nod x Frunza Frunza
insert (Nod val left right) x
    | x < val = Nod val (insert left x) right
    | x > val = Nod val left (insert right x)

maxim :: Arb -> Integer
maxim (Nod val Frunza Frunza) = val
maxim (Nod val left right) = max val (max (maxim left) (maxim right))


minim :: Arb -> Integer
minim (Nod val Frunza Frunza) = val
minim (Nod val left right) = min val (min (minim left) (minim right))

removeMax :: Arb -> Arb
removeMax Frunza = Frunza
removeMax (Nod val left Frunza) = left
removeMax (Nod val left right) = Nod val left (removeMax right)


--Lab 4

-- 1
    -- addThree :: (Int, Int, Int) -> Int
    -- addThree (x,y,z) = x + y + z

    addThree :: Int -> Int -> Int -> Int
    addThree x y z = x + y + z

-- 2

    add1 :: Int -> Int
    add1 x = x + 1

    betweenTwo :: (Int -> Int) -> Int -> Int -> Int
    betweenTwo f a b
        | b > a = a + betweenTwo f a b
        | b == a = b

    compunere :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
    compunere f g = \x -> f (g x)

    compunereN :: (Int -> Int) -> [Int] -> [Int]
    compunereN _ [] = []
    compunereN f (hd:tl) = f hd : compunereN f tl



    sumOfList :: [Int] -> Int
    sumOfList [] = 0
    sumOfList (x:xs) = x + sumOfList xs



    funOnList :: (Int -> Int) -> [Int] -> [Int]
    funOnList f [] = []
    funOnList f (x:xs) = (f x) : (funOnList f xs)




    filterList :: (a -> Bool) -> [a] -> [a]
    filterList f xs = filter f xs

    isEven :: Int -> Bool
    isEven x = x `mod` 2 == 0

    evenNumbers :: [Int] -> [Int]
    evenNumbers (x:xs) = filterList isEven (x:xs)

    myfoldl = foldl :: (b -> a -> b) -> b -> [a] -> b
    myfoldr = foldr:: (a -> b -> b) -> b -> [a] -> b 

--Lab 5

-- 3.
-- Varianta 1:
data Nat = Cons [Bool]
-- Din baza 2 in Nat
fromIntegerToNat :: Integer -> Nat
fromIntegerToNat n = Cons (reverse (toBinaryList n))
    where
    toBinaryList 0 = []
    toBinaryList x = (x `mod` 2 == 1) : toBinaryList (x `div` 2)
-- Din Nat in baza 2
fromNatToInteger :: Nat -> Integer
fromNatToInteger (Cons []) = 0
fromNatToInteger (Cons (x:xs)) = (if x then 1 else 0) * 2^length xs + fromNatToInteger (Cons xs)

--Varianta 2:
data Nat1 = Zero | Double Nat1 | DoubleAddOne Nat1
-- Din baza 2 in Nat
fromIntegerToNat1 :: Integer -> Nat1
fromIntegerToNat1 0 = Zero
fromIntegerToNat1 n = if even n then Double(fromIntegerToNat1 (n `div` 2))
                      else DoubleAddOne (fromIntegerToNat1 (n `div`2))
--Din Nat in baza 2
fromNat1ToInteger :: Nat1 -> Integer
fromNat1ToInteger Zero = 0
fromNat1ToInteger (Double n) = 2 * fromNat1ToInteger n
fromNat1ToInteger (DoubleAddOne n) = 2 * fromNat1ToInteger n + 1

--4
instance Eq Nat1 where
Zero == Zero = True
(Double x) == (Double y) = x == y
(DoubleAddOne x) == (DoubleAddOne y) = x == y
_ == _ = False

instance Ord Nat1 where
Zero <= _ = True
(Double _) <= Zero = False
(Double x) <= (Double y) = x <= y
(DoubleAddOne _) <= Zero = False
(DoubleAddOne x) <= (DoubleAddOne y) = x <= y
compare = cmp
    where
        cmp Zero Zero = EQ
        cmp Zero _ = LT
        cmp _ Zero = GT
        cmp (Double x) (Double y) = cmp x y
        cmp (DoubleAddOne x) (DoubleAddOne y) = cmp x y

--5
data Complex a = Complex a a

instance (Num a) => Num (Complex a) where
  (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
  (Complex r1 i1) * (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)
  negate (Complex r i) = Complex (negate r) (negate i)
  abs (Complex r i) = Complex (sqrt (r*r + i*i)) 0
  
--6
class MyOrd a where
  (<) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  min :: a -> a -> a
  max :: a -> a -> a
--Int
instance MyOrd Int where
  x < y = x < y
  x > y = x > y
  min x y = if x < y then x else y
  max x y = if x > y then x else y
--a
instance MyOrd a => MyOrd [a] where
  [] < [] = False
  [] < _ = True
  _ < [] = False
  (x:xs) < (y:ys)
    | x == y = xs < ys
    | x < y = True
    | otherwise = False
  x > y = y < x
  min x y = if x < y then x else y
  max x y = if x > y then x else y
--7
data Nat2 = Zero1 | Succ Nat2
  --Show transforma nr intr-un sir de caractere
instance Show Nat2 where
  show Zero = "0"
  show (Succ n) = "Succ " ++ show n
instance Eq Nat2 where
  Zero == Zero = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False
instance Ord Nat2 where
  Zero <= _ = True
  (Succ n) <= (Succ m) = n <= m
  _ <= _ = False

--8
instance Show Nat2 where
  show Zero = "o"
  show (Succ n) = "s" ++ show n
instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero _ = LT
  compare _ Zero = GT
  compare (Succ n) (Succ m) = compare n m

--12
data Nat3 = Zero2 | Succ1 Nat3
instance Eq Nat3 where
    Zero2 == Zero2 = True
    (Succ1 x) == (Succ1 y) = x == y
    _ == _ = False

--

--Lab 6

randomList :: Int -> [Int]
randomList n = take n $ map ((\i -> (1337 + i * 29) `mod` 97) . (+1)) [1..]

myList = randomList 100000

-- 1

minList :: Ord a => [a] -> a
minList = minimum

-- 2

-- :set +s

--3

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
  where
    insert x [] = [x]
    insert x (y:ys) = if x <= y then x:y:ys else y:(insert x ys)
  
selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort xs = let min' = minimum xs 
                    rest = delete min' xs in min' : selectSort rest

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = split xs
    split xs = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = if x <= y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <=x] ++ [x] ++ quickSort [y | y <- xs, y > x]

--4
maxList :: Ord a => [a] -> a
maxList xs = last (mergeSort xs)

--5

fibonacci :: [Integer]
fibonacci = 0 : 1 : rest
  where rest = next fibonacci
        next (a:b:xs) = (a+b) : next (b:xs)

-- 6

primeNums :: [Bool]
primeNums = map isPrimeBool[2..]

isPrimeBool :: Integer -> Bool
isPrimeBool n = all(\x -> n `mod` x /= 0) [2..sqrt' n]
  where sqrt' = floor.sqrt.fromIntegral

-- 7

allPrimeNums :: [Integer]
allPrimeNums = isPrime [2..]

isPrime :: [Integer] -> [Integer]
isPrime (p:xs) = p : isPrime [x | x <- xs, x `mod` p /= 0]

 -- Lab 11

type Id = String

data Term = Var Id
 | App Term Term
 | Lambda Id Term deriving (Show, Eq)

1
term = Lambda "x" (Lambda "y" (Var "x"))

-- 2
subst :: Id -> Term -> Term -> Term
subst id term (Var id') 
 | id == id' = term
 | otherwise = Var id'
subst id term (App t1 t2) = App (subst id term t1) (subst id term t2)
subst id term (Lambda id' t) 
 | id == id' = Lambda id' t
 | otherwise = Lambda id' (subst id term t)
 
 -- Exemple:
 -- 1. x[x/y] = y || return y // subst "x" (Var "y") (Var "x")
 -- 2. x[y/z] = x || return x // subst "y" (Var "z") (Var "x")
 -- 3. (x y)[y/z] = x z || return x z // subst "y" (Var "z") (App (Var "x") (Var "y"))
 -- 4. (y x)[y/z] = y x || return z x // subst "y" (Var "z") (App (Var "y") (Var "x"))
 -- 5. (λx.(y x))[x/(λz.z)] = λx.(y x) || return Lambda x (y x) // subst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x")))
 -- 6. (λy.(y x))[x/(λz.z)] = λy.(y (λz.z)) || return Lambda y(y (Lambda z.z)) // subst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x")))

 --3
remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) 
 | id == hd = remove id tl
 | otherwise = hd : remove id tl

-- Ex: remove "x" ["x", "x", "y", "z"]

--4
free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

--5 
vars :: Term -> [Id]
vars term = nub (vars' term)
 where
  vars' (Var id) = [id]
  vars' (App term1 term2) = vars' term1 ++ vars' term2
  vars' (Lambda id term) = id : vars' term

--6
fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ (show index)) `elem` ids then fresh' ids (index + 1)
                   else "n" ++ (show index)
fresh :: [Id] -> Id
fresh ids = fresh' ids 1

--7 
casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') avoid
  | id == id' = term
  | otherwise = Var id'
casubst id term (App term1 term2) avoid =
  let term1' = casubst id term term1 avoid
      term2' = casubst id term term2 avoid
  in App term1' term2'
casubst id term (Lambda id' term') avoid 
  | id == id' = Lambda id' term'
  | id' `elem` free term =
    let id'' = fresh avoid in
      Lambda id'' (casubst id term (subst id' (Var id'') term') avoid)
      | otherwise = Lambda id' (casubst' id term term' avoid)
        where casubst' id term term' avoid' = casubst id term term' (id':avoid')

--8
reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid) -- beta-reducerea propriu-zisa
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
  Nothing -> case reduce1' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
  Nothing -> Nothing
  Just term' -> Just (Lambda id term')

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)

x = Var"x"
y = Var"y"
z = Var"z"

term1 = Lambda "x" x -- Nothing
term2 = App term1 term1 -- Just (Lambda "x" (Var "x"))
term3 = Lambda "y" (Lambda "x" term2) -- Just (Lambda "y" (Lambda "x" (Lambda "x" (Var "x"))))
term4 = App term3 term1 -- Just (Lambda "x" (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))))

--9 
reduce :: Term -> Term
reduce term = case reduce1 term of
  Nothing -> term
  Just term' -> reduce term'

term5 = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "z")

--10 
{- exemplu de termen bucla (λx.x x) (λx.x x) -}
reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term = case reduce1 term of
  Nothing -> term
  Just term' -> reduceFor (n-1) term'

--11
tTRUE = Lambda "x" (Lambda "y" x)
tFALSE = Lambda "x" (Lambda "y" y)
tAND = Lambda "x" (Lambda "y" App((App x y) tFALSE))

--Lab 13

--1

type Id = String

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) 
 | id == hd = remove id tl
 | otherwise = hd : remove id tl

fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ (show index)) `elem` ids then fresh' ids (index + 1)
                   else "n" ++ (show index)
fresh :: [Id] -> Id
fresh ids = fresh' ids 1

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

subst :: Id -> Term -> Term -> Term
subst id term (Var id') 
 | id == id' = term
 | otherwise = Var id'
subst id term (App t1 t2) = App (subst id term t1) (subst id term t2)
subst id term (Lambda id' t) 
 | id == id' = Lambda id' t
 | otherwise = Lambda id' (subst id term t)

vars :: Term -> [Id]
vars term = nub (vars' term)
 where
  vars' (Var id) = [id]
  vars' (App term1 term2) = vars' term1 ++ vars' term2
  vars' (Lambda id term) = id : vars' term

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') avoid
  | id == id' = term
  | otherwise = Var id'
casubst id term (App term1 term2) avoid =
  let term1' = casubst id term term1 avoid
      term2' = casubst id term term2 avoid
  in App term1' term2'
casubst id term (Lambda id' term') avoid 
  | id == id' = Lambda id' term'
  | id' `elem` free term =
    let id'' = fresh avoid in
      Lambda id'' (casubst id term (subst id' (Var id'') term') avoid)
      | otherwise = Lambda id' (casubst' id term term' avoid)
        where casubst' id term term' avoid' = casubst id term term' (id':avoid')

data Term = Var Id
 | App Term Term
 | Lambda Id Term deriving (Show, Eq)
reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
  Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
  Nothing -> case reduce1' term2 avoid of
  Nothing -> Nothing
  Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
  Nothing -> Nothing
  Just term' -> Just (Lambda id term')
reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)

--2
reduce2' :: Term -> [Id] -> Maybe Term
reduce2' (Var id') _ = Nothing
reduce2' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
reduce2' (App term1 term2) avoid = case reduce1' term1 avoid of
  Nothing -> case reduce1' term2 avoid of
    Nothing -> Nothing
    Just term2' -> Just (App term1 term2')
  Just term1' -> Just (App term1' term2)
reduce2' (Lambda id term) _ = Just (Lambda id term)

reduce2 :: Term -> Maybe Term
reduce2 t = reduce2' t (vars t)
--amana reducerile pentru a evita calculele inutile

--3 F B-R
strategy1' :: Term -> [Id] -> [Term]
strategy1' (Var _) _ = []
strategy1' (App (Lambda id term) term') avoid = [casubst id term' term avoid] ++
  let all = strategy1' term avoid in
  let all' = strategy1' term' avoid in
  [ App (Lambda id successorTerm) term' | successorTerm <- all ] ++
  [ App (Lambda id term) successorTerm' | successorTerm' <- all']
strategy1' (App term1 term2) avoid =
  let all1 = strategy1' term1 avoid in
  let all2 = strategy1' term2 avoid in
  [ App sterm1 term2 | sterm1 <- all1 ] ++
  [ App term1 sterm2 | sterm2 <- all2 ]
strategy1' (Lambda id term) avoid =
  let all = strategy1' term avoid in
  [ Lambda id sterm | sterm <- all ]
strategy1 :: Term -> [Term]
strategy1 term = strategy1' term (vars term)
strategy :: Term -> [Term]
strategy term = let all = strategy1 term in case all of
  [] -> [term]
  _ -> concat (map strategy all)

--4
reduce3' :: Term -> [Id] -> Maybe Term
reduce3' (Var id') _ = Nothing
reduce3' (App (Lambda id term) term') avoid =
  case reduce3' term avoid of
    Just term'' -> Just (App (Lambda id term'') term')
    Nothing -> Just (casubst id term' term avoid)
reduce3' (App term1 term2) avoid =
  case reduce1' term1 avoid of
    Just term1' -> Just (App term1' term2)
    Nothing -> case reduce3' term2 avoid of
      Just term2' -> Just (App term1 term2')
      Nothing -> Nothing
reduce3' (Lambda id term) avoid =
  case reduce1' term avoid of
    Just term' -> Just (Lambda id term')
    Nothing -> Nothing

reduce3 :: Term -> Maybe Term
reduce3 t = reduce3' t (vars t)

--5

--1. Exemplul: (λx1.x1)(λx2.x2)(λz.(λx3.x3) z))
-- CBV
-- -- Output: Just (App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (App (Lambda "z" (Var "z")) (Var "z"))))
-- main = do
--   let term = App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (App (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))) (Var "z")))
--   print(reduce4 term)
-- CBN 
-- Output: Just (App (Lambda "x2" (Var "x2")) (App (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))) (Var "z")))
-- main = do
--   let term = App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (App (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))) (Var "z")))
--   print(reduce2 term)


--2. Exemplul: (λx1.λx2.x2) (λx.x) (λy.y)
-- CBV
-- Output: Just (Lambda "x2" (Var "x2"))
--         Nothing
-- main = do
--  let term1 = App (Lambda "x1" (Lambda "x2" (Var "x2"))) (Lambda "x" (Var "x"))
--  let term2 = Lambda "y" (Var "y")
--  print (reduce4 term1)
--  print (reduce4 term2)
-- CBN
-- Output: Just (Lambda "x2" (Var "x2"))
--         Just (Lambda "y" (Var "y"))
-- main = do
--   let term1 = App (Lambda "x1" (Lambda "x2" (Var "x2"))) (Lambda "x" (Var "x"))
--   let term2 = Lambda "y" (Var "y")
--   print(reduce2 term1)
--   print(reduce2 term2)

--6
reduce4' :: Term -> [Id] -> Maybe Term
reduce4' (Var id') _ = Nothing
reduce4' (App (Lambda id term) term') avoid =
  case reduce4' term avoid of
    Just term'' -> Just (App (Lambda id term'') term')
    Nothing -> case reduce4' term' avoid of
      Just term''' -> Just (App (Lambda id term) term''')
      Nothing -> Just (casubst id term' term avoid)
reduce4' (App term1 term2) avoid =
  case reduce4' term1 avoid of
    Just term1' -> Just (App term1' term2)
    Nothing -> case reduce4' term2 avoid of
      Just term2' -> Just (App term1 term2')
      Nothing -> Nothing
reduce4' (Lambda id term) avoid =
  case reduce4' term avoid of
    Just term' -> Just (Lambda id term')
    Nothing -> Nothing

reduce4 :: Term -> Maybe Term
reduce4 t = reduce4' t (vars t)


import Data.Char (ord)

char2Integer :: Char -> Integer
char2Integer c = fromIntegral (ord c) - 48

string2Integer :: String -> Integer
string2Integer = convert 0
  where
    convert :: Integer -> String -> Integer
    convert acc [] = acc
    convert acc (c:cs) = convert (10 * acc + char2Integer c) cs


countCapitals :: String -> Int
countCapitals = count 0
  where
    count :: Int -> String -> Int
    count acc [] = acc
    count acc (c:cs)
      | isCapitalLetter c = count (acc + 1) cs
      | otherwise = count acc cs
    
    isCapitalLetter :: Char -> Bool
    isCapitalLetter c = c >= 'A' && c <= 'Z'

data Vehicle = Car String String Int
             | Ship String Int
             | Bicycle String String
             | Truck String String Int

vehicleBrand :: Vehicle -> String
vehicleBrand vehicle = case vehicle of
  Car brand _ _ -> brand
  Ship brand _ -> brand
  Bicycle brand _ -> brand
  Truck brand _ _ -> brand


-- EXAM PREP//

data Expr
  = Const Int
  | Var Char
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

exampleExpr :: Expr
exampleExpr = Sub (Add (Mul (Var 'x') (Const 7)) (Const 10)) (Const 23)


--

countDigits :: String -> Int
countDigits str = countDigitsAux str 0
  where
    countDigitsAux :: String -> Int -> Int
    countDigitsAux [] count = count
    countDigitsAux (c:cs) count
      | c >= '0' && c <= '9' = countDigitsAux cs (count + 1)
      | otherwise = countDigitsAux cs count

import Data.Char(ord);
import Language.Haskell.TH (Exp)

char2Integer :: Char -> Integer
char2Integer c = (fromIntegral (ord c)) - 48

string2Integer :: String -> Integer
string2Integer = convert 0
  where
    convert :: Integer -> String -> Integer
    convert acc [] = acc
    convert acc (c:cs) = convert (10*acc + char2Integer(c)) cs


countCapitals :: String -> Integer
countCapitals = count 0
  where
    count :: Integer -> String -> Integer
    count acc [] = acc
    count acc (c:cs)
      | isCapital c = count (acc+1) cs
      | otherwise = count acc cs

    isCapital :: Char -> Bool
    isCapital c = c >= 'A' && c <= 'Z' 

data Vehicle = Car String String Integer
              | Ship String Integer
              | Bicycle String String
              | Truck String String Integer

vehicleBrand :: Vehicle -> String
vehicleBrand vehicle = case vehicle of
  Car brand _ _ -> brand
  Ship brand _ -> brand
  Bicycle brand _ -> brand
  Truck brand _ _ -> brand

data Expr = Const Int
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr

exampleExpr :: Expr
exampleExpr =    Sub  (Add (Mul (Var 'x') (Const 7)) (Const 10)) (Const 23)
