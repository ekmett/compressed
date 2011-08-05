{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeOperators, FlexibleInstances, FlexibleContexts, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Compressed.RunLengthEncoding
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Compression algorithms are all about exploiting redundancy. When applying
-- an expensive 'Reducer' to a redundant source, it may be better to 
-- extract the structural redundancy that is present. Run length encoding
-- can do so for long runs of identical inputs.
-----------------------------------------------------------------------------

module Data.Compressed.RunLengthEncoding
    ( RLE(..)
    , Run
    , runLength
    , decode
    , encode
    , recode
    , toRuns
    , fromRuns
    ) where

import Data.Foldable
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Semigroup.Foldable
import Data.Monoid
import Data.Hashable
import Data.Function (on)
import Data.Functor.Bind
import Control.Comonad
import Data.FingerTree (FingerTree,(|>),(<|),ViewL(..),ViewR(..),(><),viewl,viewr, Measured(..), split)
import qualified Data.FingerTree as F
import Data.Generator
import Data.Pointed
import Data.Key
import Control.Applicative

-- | A single run with a strict length
data Run a = Run {-# UNPACK #-} !Int a deriving (Eq,Show)

runLength :: Run a -> Int
runLength (Run n _) = n

-- lexicographical order
instance Ord a => Ord (Run a) where
  compare (Run n a) (Run m b) = case compare a b of
    LT -> LT
    GT -> GT
    EQ -> compare n m

instance Extend Run where
  duplicate r@(Run i _) = Run i r
  extend f r@(Run i _) = Run i (f r)

instance Comonad Run where
  extract (Run _ a) = a 

instance Functor Run where
  fmap f (Run n a) = Run n (f a)
  a <$ Run n _ = Run n a

instance Pointed Run where
  point = Run 1

instance Apply Run where
  Run n f <.> Run m a = Run (n * m) (f a)
  Run n _  .> Run m a = Run (n * m) a
  Run n a <.  Run m _ = Run (n * m) a

instance Applicative Run where
  pure = Run 1
  Run n f <*> Run m a = Run (n * m) (f a)
  Run n _  *> Run m a = Run (n * m) a
  Run n a <*  Run m _ = Run (n * m) a

instance Bind Run where
  Run n a >>- f = case f a of
    Run m b -> Run (n * m) b
 
instance Monad Run where
  return = Run 1
  Run n _ >> Run m b = Run (n * m) b
  Run n a >>= f = case f a of
    Run m b -> Run (n * m) b

instance Foldable Run where
  foldMap k (Run y0 x0) = f (k x0) y0 where
    f x y
      | even y = f (x `mappend` x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x `mappend` x) ((y - 1) `quot` 2) x
    g x y z
      | even y = g (x `mappend` x) (y `quot` 2) z
      | y == 1 = x `mappend` z
      | otherwise = g (x `mappend` x) ((y - 1) `quot` 2) (x `mappend` z)
  {-# INLINE foldMap #-}

instance Foldable1 Run where
  foldMap1 k (Run y0 x0) = f (k x0) y0 where
    f x y
      | even y = f (x <> x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x <> x) ((y - 1) `quot` 2) x
    g x y z
      | even y = g (x <> x) (y `quot` 2) z
      | y == 1 = x <> z
      | otherwise = g (x <> x) ((y - 1) `quot` 2) (x <> z)
  {-# INLINE foldMap1 #-}

instance Measured Count (Run a) where
  measure (Run n _) = Count n

-- | A 'Generator' which supports efficient 'mapReduce' operations over run-length encoded data.
newtype RLE a = RLE { getRLE :: FingerTree Count (Run a) } 

toRuns :: RLE a -> [Run a]
toRuns = toList . getRLE

fromRuns :: [Run a] -> RLE a
fromRuns = RLE . F.fromList 

instance Eq a => Semigroup (RLE a) where
  RLE l <> RLE r = go (viewr l) (viewl r) where
    go EmptyR  _ = RLE r
    go _  EmptyL = RLE l
    go (l' :> Run m a) (Run n b :< r')
      | a == b     = RLE ((l' |> Run (m+n) a) >< r')
      | otherwise  = RLE (l >< r)

instance Functor RLE where
  fmap f = RLE . F.fmap' (fmap f) . getRLE

instance Pointed RLE where
  point = RLE . F.singleton . pure

instance Apply RLE where
  (<.>) = (<*>) 
  (<. ) = (<* )
  ( .>) = ( *>)

instance Applicative RLE where
  pure = RLE . F.singleton . pure
  RLE fs <*> RLE as = RLE $ F.fromList 
    [ Run (n * m) (f a)
    | Run n f <- toList fs
    , Run m a <- toList as
    ]
  RLE as <* RLE bs = RLE $ F.fmap' (\(Run n a) -> Run (n * m) a) as where
    m = reduceWith getCount bs
  RLE as *> RLE bs = RLE $ mconcat $ replicate (reduceWith getCount as) bs

instance Bind RLE where
  (>>-) = (>>=)

instance Monad RLE where
  return = RLE . F.singleton . pure 
  (>>) = (*>)
  RLE xs >>= f = RLE $ mconcat [ mconcat $ replicate n (getRLE (f a)) | Run n a <- toList xs ]
 
instance Eq a => Reducer a (RLE a) where
  unit = pure
  cons a (RLE r) = case viewl r of
    EmptyL -> pure a
    Run n b :< r' 
      | a == b    -> RLE (Run (n+1) a <| r')
      | otherwise -> RLE (Run 1     a <| r )
  snoc (RLE l) a = case viewr l of
    EmptyR -> pure a
    l' :> Run n b 
      | a == b    -> RLE (l' |> Run (n+1) b)
      | otherwise -> RLE (l  |> Run 1 a   )

instance Eq a => Monoid (RLE a) where
  mempty = RLE mempty
  mappend = (<>)

instance Foldable RLE where
  foldMap f = foldMap (foldMap f) . getRLE

instance Generator (RLE a) where
  type Elem (RLE a) = a
  mapReduce f = foldMap (unit . f)

instance Hashable a => Hashable (RLE a) where
  hash = hash . toList
  hashWithSalt n = hashWithSalt n . toList

instance Eq a => Eq (RLE a) where
  (==) = (==) `on` toList  -- todo stride through aligning

instance Zip RLE where
  zipWith f (RLE xs0) (RLE ys0) = RLE $ case toList xs0 of
    [] -> mempty
    (Run n0 a0:as0) -> case toList ys0 of 
      [] -> mempty
      (Run m0 b0:bs0) -> go n0 a0 as0 m0 b0 bs0 
    where
      go !n !a !as !m !b !bs = case compare n m of 
        LT -> Run n (f a b) <| case as of
          [] -> mempty
          (Run n' a':as') -> go n' a' as' (m - n) b bs
        EQ -> Run n (f a b) <| case as of
          [] -> mempty
          (Run n' a':as') -> case bs of
             [] -> mempty
             (Run m' b':bs') -> go n' a' as' m' b' bs'
        GT -> Run m (f a b) <| case bs of
          [] -> mempty
          (Run m' b':bs') -> go (n - m) a as m' b' bs'
          
type instance Key RLE = Int

instance Lookup RLE where
  lookup i (RLE xs) 
    | i < 0 = Nothing
    | otherwise = case viewl r of
      Run _ a :< _ -> Just a
      EmptyL       -> Nothing 
      where (l,r) = split (\n -> getCount n > i) xs

instance Adjustable RLE where
  adjust f i (RLE xs) = RLE $ case viewl r of
    EmptyL -> xs
    Run n a :< r' -> 
      let 
        k = i - getCount (measure l)
        infixr 4 <?
        Run 0 _ <? xs = xs
        Run n a <? xs = Run n a <| xs
     in l >< (Run k a <? Run 1 (f a) <? Run (n - k - 1) a <? r')
    where 
      (l,r) = split (\n -> getCount n > i) xs


encode :: (Generator c, Eq (Elem c)) => c -> RLE (Elem c)
encode = reduce
{-# RULES "encode/recode"     encode = recode #-}
{-# RULES "encode/encodeList" encode = encodeList #-}

decode :: RLE a -> [a]
decode = reduce

recode :: Eq a => RLE a -> RLE a
recode (RLE xs0) = case toList xs0 of 
  [] -> RLE mempty
  (Run n0 a0:as0) -> RLE $ go n0 a0 as0
  where
    go n a [] = F.singleton (Run n a)
    go n a (Run m b:bs)
      | a == b = go (n + m) a bs
      | otherwise = Run n a <| go m b bs

encodeList :: Eq a => [a] -> RLE a
encodeList []       = RLE mempty
encodeList (a0:as0) = RLE $ go 1 a0 as0
  where
    go n a [] = F.singleton (Run n a)
    go n a (b:bs) 
      | a == b    = go (n + 1) a bs
      | otherwise = Run n a <| go 1 b bs
