module Stately where

import           Control.Monad
import           Control.Monad.Trans.State as ST
import qualified Data.DList                as DL

-- "L’Etat, c’est moi."
newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

-- fmap :: (a -> b) -> Moi s a -> Moi s b
instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ f' . g
    where
      f' (a, s) = (f a, s)

-- pure :: a -> Moi s a
-- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (a, s') = g s
          h = fst $ f s
          b = h a
       in (b, s')

-- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g =
    Moi $ \s ->
      let a = fst $ f s
          ms = runMoi $ g a
       in ms s

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo x y =
  let dlist = execState (mapM_ addResult [x .. y]) DL.empty
   in DL.apply dlist []

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- ST.get
  let result = fizzBuzz n
  ST.put (DL.snoc xs result)

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec m s = snd $ runMoi m s

eval :: Moi s a -> s -> a
eval m s = fst $ runMoi m s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
