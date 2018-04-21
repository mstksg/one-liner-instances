{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : System.Random.OneLiner
-- Description : Derived methods for Random.
-- Copyright   : (c) Justin Le 2018
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for 'Random', using "Generics.OneLiner" and
-- "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') made with a single
-- constructor, where every field is an instance of 'Random'.
--
-- Also includes a newtype wrapper that imbues any such data type with
-- instant 'Random' instances, which can one day be used with /DerivingVia/
-- syntax to derive instances automatically.
--

module System.Random.OneLiner (
  -- * Newtype wrapper
    GRandom(..)
  -- * Generics-derived methods
  -- ** Random
  , gRandomR
  , gRandom
  , gRandomRs
  , gRandoms
  , gRandomRIO
  , gRandomIO
  ) where

import           Data.Coerce
import           Data.Data
import           GHC.Exts          (build)
import           GHC.Generics
import           Generics.OneLiner
import           System.Random

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Semigroup', then @'GMonoid' a@ has a 'Semigroup' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Monoid', then @'GMonoid' a@ has a 'Monoid' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
newtype GRandom a = GRandom { getGRandom :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADTRecord a
         , Constraints a Random
         )
      => Random (GRandom a) where

    randomR :: forall g. RandomGen g => (GRandom a, GRandom a) -> g -> (GRandom a, g)
    randomR = coerce (gRandomR @a @g)
    {-# INLINE randomR #-}

    random :: forall g. RandomGen g => g -> (GRandom a, g)
    random = coerce (gRandom @a @g)
    {-# INLINE random #-}

    randomRs :: forall g. RandomGen g => (GRandom a, GRandom a) -> g -> [GRandom a]
    randomRs = coerce (gRandomRs @a @g)
    {-# INLINE randomRs #-}

    randoms :: forall g. RandomGen g => g -> [GRandom a]
    randoms = coerce (gRandoms @a @g)
    {-# INLINE randoms #-}

    randomRIO :: (GRandom a, GRandom a) -> IO (GRandom a)
    randomRIO = coerce (gRandomRIO @a)
    {-# INLINE randomRIO #-}

    randomIO :: IO (GRandom a)
    randomIO = coerce (gRandomIO @a)
    {-# INLINE randomIO #-}

-- | 'randomR' implemented by sequencing 'randomR' between all components
gRandomR
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => (a, a) -> g -> (a, g)
gRandomR (l, u) = runState $
    dialgebra @Random
      (State . randomR . dePair)
      (Pair l u)
{-# INLINE gRandomR #-}

-- | 'random' implemented by sequencing 'random' for all components.
gRandom
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => g -> (a, g)
gRandom = runState $ createA' @Random (State random)
{-# INLINE gRandom #-}

-- | 'randomRs' implemented by repeatedly calling 'gRandomR'.
gRandomRs
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => (a, a) -> g -> [a]
gRandomRs ival g = build (\cons _nil -> buildRandoms cons (gRandomR ival) g)
{-# INLINE gRandomRs #-}

-- | 'randoms' implemented by repeatedly calling 'gRandoms'.
gRandoms
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => g -> [a]
gRandoms g = build (\cons _nil -> buildRandoms cons gRandom g)
{-# INLINE gRandoms #-}

-- | 'randomRIO' implemented by calling 'gRandomR' on the global seed.
gRandomRIO
    :: forall a. (ADTRecord a, Constraints a Random)
    => (a, a) -> IO a
gRandomRIO range = getStdRandom (gRandomR range)
{-# INLINE gRandomRIO #-}

-- | 'randomIO' implemented by calling 'gRandom' on the global seed.
gRandomIO
    :: forall a. (ADTRecord a, Constraints a Random)
    => IO a
gRandomIO = getStdRandom gRandom
{-# INLINE gRandomIO #-}

buildRandoms :: RandomGen g
             => (a -> as -> as)  -- ^ E.g. '(:)' but subject to fusion
             -> (g -> (a,g))     -- ^ E.g. 'random'
             -> g                -- ^ A 'RandomGen' instance
             -> as
buildRandoms cons rand = go
  where
    -- The seq fixes part of #4218 and also makes fused Core simpler.
    go g = x `seq` (x `cons` go g') where (x,g') = rand g
{-# INLINE buildRandoms #-}

data Pair a = Pair !a !a
    deriving Functor

dePair :: Pair a -> (a, a)
dePair (Pair x y) = (x, y)
{-# INLINE dePair #-}

newtype State s a = State { runState :: s -> (a, s) }
    deriving Functor

instance Applicative (State s) where
    pure x = State (x,)
    {-# INLINE pure #-}
    sf <*> sx = State $ \s0 ->
        let (f, !s1) = runState sf s0
            (x, !s2) = runState sx s1
        in  (f x, s2)
    {-# INLINE (<*>) #-}
