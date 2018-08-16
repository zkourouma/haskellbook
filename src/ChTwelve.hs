module ChTwelve where

-- String Processing
replaceThe :: String -> String
replaceThe sentence =
  let w:ws = words sentence
      a = fromMaybe ("a" :: String) (notThe w)
   in case ws of
        [] -> a
        _  -> a ++ " " ++ replaceThe (unwords ws)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence =
  let w:ws = words sentence
      c = theBeforeVowel w ws
   in case ws of
        [] -> c
        _  -> c + countTheBeforeVowel (unwords ws)

theBeforeVowel :: String -> [String] -> Integer
theBeforeVowel _ [] = 0
theBeforeVowel "the" (l:_) =
  if isVowel (head l)
    then 1
    else 0
theBeforeVowel _ _ = 0

countVowels :: String -> Integer
countVowels = foldl countVowel 0

countVowel :: Integer -> Char -> Integer
countVowel i c =
  if isVowel c
    then i + 1
    else i

isVowel :: Char -> Bool
isVowel c = c `elem` "AEIOUYaeiouy"

-- Validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s =
  let (consts, vols) = foldl mkWord' (0, 0) s
   in if vols > consts
        then Nothing
        else Just (Word' s)

mkWord' :: (Integer, Integer) -> Char -> (Integer, Integer)
mkWord' (cs, vs) c =
  if c `elem` vowels
    then (cs, vs + 1)
    else (cs + 1, vs)

-- It's only Natural
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise =
    case integerToNat (i - 1) of
      Nothing  -> Nothing
      Just nat -> Just (Succ nat)

-- Small library for Maybe
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (a:as) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr catMaybes' []

catMaybes' :: Maybe a -> [a] -> [a]
catMaybes' Nothing as  = as
catMaybes' (Just a) as = a : as

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr flipMaybe' (Just [])

flipMaybe' :: Maybe a -> Maybe [a] -> Maybe [a]
flipMaybe' Nothing _          = Nothing
flipMaybe' _ Nothing          = Nothing
flipMaybe' (Just a) (Just as) = Just (a : as)

-- Small library for Either
lefts' :: [Either a b] -> [a]
lefts' = foldr (\(Left a) as -> a : as) []

rights' :: [Either a b] -> [b]
rights' = foldr (\(Right b) bs -> b : bs) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (as, bs)
  where
    as = lefts' es
    bs = rights' es

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Unfolds
myIterate :: (a -> a) -> a -> [a]
myIterate f a = go [a]
  where
    go (x:xs) = x : go (f x : x : xs)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f = go
  where
    go n =
      case f n of
        Nothing      -> []
        Just (a, b') -> a : go b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing        -> Leaf
    Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeBuild' (takeN n [0 ..])

treeBuild' :: [Integer] -> Maybe ([Integer], Integer, [Integer])
treeBuild' []     = Nothing
treeBuild' (x:xs) = Just (xs, x, xs)

takeN :: Integer -> [a] -> [a]
takeN n = take (fromIntegral n)
