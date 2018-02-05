{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Numeric.OneLiner
-- Description : Derived methods for numeric typeclasses
-- Copyright   : (c) Justin Le 2018
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for numeric typeclasses, using "Generics.OneLiner" and
-- "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') made with a single
-- constructor, where every field is an instance of 'Num' (or 'Fractional'
-- or 'Floating', depending on the function).
--
-- Also includes a newtype wrapper that imbues any such data type with an
-- instant 'Num' (and 'Fractional' and 'Floating') instance, which can one
-- day be used with /DerivingVia/ syntax to derive instances automatically.
--

module Numeric.OneLiner (
  -- * Newtype wrapper
    GNum(..)
  -- * Generics-derived methods
  -- $num
  -- ** Num
  , gPlus
  , gMinus
  , gTimes
  , gNegate
  , gAbs
  , gSignum
  , gFromInteger
  -- ** Fractional
  , gDivide
  , gRecip
  , gFromRational
  -- ** Floating
  , gPi
  , gExp
  , gLog
  , gSqrt
  , gPower
  , gLogBase
  , gSin
  , gCos
  , gTan
  , gAsin
  , gAcos
  , gAtan
  , gSinh
  , gCosh
  , gTanh
  , gAsinh
  , gAcosh
  , gAtanh
  ) where

import           Data.Data
import           GHC.Generics
import           Generics.OneLiner
import           Generics.OneLiner.Instances.Internal

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Num', then @'GNum' a@ has a 'Num' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Fractional', then @'GNum' a@ has a 'Fractional' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Floating', then @'GNum' a@ has a 'Floating' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
newtype GNum a = GNum { getGNum :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance (ADTRecord a, Constraints a Num)
      => Num (GNum a) where
    (+)         = c2 (gPlus @a)
    {-# INLINE (+) #-}
    (-)         = c2 (gMinus @a)
    {-# INLINE (-) #-}
    (*)         = c2 (gTimes @a)
    {-# INLINE (*) #-}
    negate      = c1 (gNegate @a)
    {-# INLINE negate #-}
    abs         = c1 (gAbs @a)
    {-# INLINE abs #-}
    signum      = c1 (gSignum @a)
    {-# INLINE signum #-}
    fromInteger = c0 (gFromInteger @a)
    {-# INLINE fromInteger #-}

instance ( ADTRecord a
         , Constraints a Num
         , Constraints a Fractional
         )
      => Fractional (GNum a) where
    (/)          = c2 (gDivide @a)
    {-# INLINE (/) #-}
    recip        = c1 (gRecip @a)
    {-# INLINE recip #-}
    fromRational = c0 (gFromRational @a)
    {-# INLINE fromRational #-}

instance ( ADTRecord a
         , Constraints a Num
         , Constraints a Fractional
         , Constraints a Floating
         )
      => Floating (GNum a) where
    pi      = c0 (gPi @a)
    {-# INLINE pi #-}
    exp     = c1 (gExp @a)
    {-# INLINE exp #-}
    log     = c1 (gLog @a)
    {-# INLINE log #-}
    sqrt    = c1 (gSqrt @a)
    {-# INLINE sqrt #-}
    (**)    = c2 (gPower @a)
    {-# INLINE (**) #-}
    logBase = c2 (gLogBase @a)
    {-# INLINE logBase #-}
    sin     = c1 (gSin @a)
    {-# INLINE sin #-}
    cos     = c1 (gCos @a)
    {-# INLINE cos #-}
    tan     = c1 (gTan @a)
    {-# INLINE tan #-}
    asin    = c1 (gAsin @a)
    {-# INLINE asin #-}
    acos    = c1 (gAcos @a)
    {-# INLINE acos #-}
    atan    = c1 (gAtan @a)
    {-# INLINE atan #-}
    sinh    = c1 (gSinh @a)
    {-# INLINE sinh #-}
    cosh    = c1 (gCosh @a)
    {-# INLINE cosh #-}
    tanh    = c1 (gTanh @a)
    {-# INLINE tanh #-}
    asinh   = c1 (gAsinh @a)
    {-# INLINE asinh #-}
    acosh   = c1 (gAcosh @a)
    {-# INLINE acosh #-}
    atanh   = c1 (gAtanh @a)
    {-# INLINE atanh #-}

-- $num
-- All of these implement the appropriate functions by carrying them over
-- every field of the data type

gPlus
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a -> a
gPlus = binaryOp @Num (+)
{-# INLINE gPlus #-}

gMinus
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a -> a
gMinus = binaryOp @Num (-)
{-# INLINE gMinus #-}

gTimes
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a -> a
gTimes = binaryOp @Num (*)
{-# INLINE gTimes #-}

gNegate
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a
gNegate = unaryOp @Num negate
{-# INLINE gNegate #-}

gAbs
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a
gAbs = unaryOp @Num abs
{-# INLINE gAbs #-}

gSignum
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a
gSignum = unaryOp @Num signum
{-# INLINE gSignum #-}

gFromInteger
    :: forall a. (ADTRecord a, Constraints a Num)
    => Integer -> a
gFromInteger x = nullaryOp @Num (fromInteger x)
{-# INLINE gFromInteger #-}

gDivide
    :: forall a. (ADTRecord a, Constraints a Fractional)
    => a -> a -> a
gDivide = binaryOp @Fractional (/)
{-# INLINE gDivide #-}

gRecip
    :: forall a. (ADTRecord a, Constraints a Fractional)
    => a -> a
gRecip = unaryOp @Fractional recip
{-# INLINE gRecip #-}

gFromRational
    :: forall a. (ADTRecord a, Constraints a Fractional)
    => Rational -> a
gFromRational x = nullaryOp @Fractional (fromRational x)
{-# INLINE gFromRational #-}

gPi
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a
gPi = nullaryOp @Floating pi
{-# INLINE gPi #-}

gExp
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gExp = unaryOp @Floating exp
{-# INLINE gExp #-}

gLog
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gLog = unaryOp @Floating log
{-# INLINE gLog #-}

gSqrt
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gSqrt = unaryOp @Floating sqrt
{-# INLINE gSqrt #-}

gPower
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a -> a
gPower = binaryOp @Floating (**)
{-# INLINE gPower #-}

gLogBase
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a -> a
gLogBase = binaryOp @Floating logBase
{-# INLINE gLogBase #-}

gSin
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gSin = unaryOp @Floating sin
{-# INLINE gSin #-}

gCos
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gCos = unaryOp @Floating cos
{-# INLINE gCos #-}

gTan
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gTan = unaryOp @Floating tan
{-# INLINE gTan #-}

gAsin
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAsin = unaryOp @Floating asin
{-# INLINE gAsin #-}

gAcos
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAcos = unaryOp @Floating acos
{-# INLINE gAcos #-}

gAtan
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAtan = unaryOp @Floating atan
{-# INLINE gAtan #-}

gSinh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gSinh = unaryOp @Floating sinh
{-# INLINE gSinh #-}

gCosh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gCosh = unaryOp @Floating cosh
{-# INLINE gCosh #-}

gTanh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gTanh = unaryOp @Floating atanh
{-# INLINE gTanh #-}

gAsinh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAsinh = unaryOp @Floating asinh
{-# INLINE gAsinh #-}

gAcosh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAcosh = unaryOp @Floating acosh
{-# INLINE gAcosh #-}

gAtanh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAtanh = unaryOp @Floating atanh
{-# INLINE gAtanh #-}

