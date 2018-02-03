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
-- instant 'Num' (and 'Fractional' and 'Floating') instance.
--
-- See README for details on usage instructions and motivations.
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

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Num', then @'GNum' a@ has a 'Num' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Fractional', then @'GNum' a@ has a 'Fractional' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Floating', then @'GNum' a@ has a 'Floating' instance.
--
newtype GNum a = GNum { getGNum :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance (ADTRecord a, Constraints (GNum a) Num)
      => Num (GNum a) where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance (ADTRecord a, Constraints (GNum a) Fractional)
      => Fractional (GNum a) where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

instance (ADTRecord a, Constraints (GNum a) Floating)
      => Floating (GNum a) where
    pi      = gPi
    exp     = gExp
    log     = gLog
    sqrt    = gSqrt
    (**)    = gPower
    logBase = gLogBase
    sin     = gSin
    cos     = gCos
    tan     = gTan
    asin    = gAsin
    acos    = gAcos
    atan    = gAtan
    sinh    = gSinh
    cosh    = gCosh
    tanh    = gTanh
    asinh   = gAsinh
    acosh   = gAcosh
    atanh   = gAtanh

-- $num
-- All of these implement the appropriate functions by carrying them over
-- every field of the data type

gPlus
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a -> a
gPlus = binaryOp @Num (+)

gMinus
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a -> a
gMinus = binaryOp @Num (-)

gTimes
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a -> a
gTimes = binaryOp @Num (*)

gNegate
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a
gNegate = unaryOp @Num negate

gAbs
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a
gAbs = unaryOp @Num abs

gSignum
    :: forall a. (ADTRecord a, Constraints a Num)
    => a -> a
gSignum = unaryOp @Num signum

gFromInteger
    :: forall a. (ADTRecord a, Constraints a Num)
    => Integer -> a
gFromInteger x = nullaryOp @Num (fromInteger x)

gDivide
    :: forall a. (ADTRecord a, Constraints a Fractional)
    => a -> a -> a
gDivide = binaryOp @Fractional (/)

gRecip
    :: forall a. (ADTRecord a, Constraints a Fractional)
    => a -> a
gRecip = unaryOp @Fractional recip

gFromRational
    :: forall a. (ADTRecord a, Constraints a Fractional)
    => Rational -> a
gFromRational x = nullaryOp @Fractional (fromRational x)

gPi
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a
gPi = nullaryOp @Floating pi

gExp
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gExp = unaryOp @Floating exp

gLog
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gLog = unaryOp @Floating log

gSqrt
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gSqrt = unaryOp @Floating sqrt

gPower
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a -> a
gPower = binaryOp @Floating (**)

gLogBase
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a -> a
gLogBase = binaryOp @Floating logBase

gSin
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gSin = unaryOp @Floating sin

gCos
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gCos = unaryOp @Floating cos

gTan
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gTan = unaryOp @Floating tan

gAsin
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAsin = unaryOp @Floating asin

gAcos
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAcos = unaryOp @Floating acos

gAtan
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAtan = unaryOp @Floating atan

gSinh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gSinh = unaryOp @Floating sinh

gCosh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gCosh = unaryOp @Floating cosh

gTanh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gTanh = unaryOp @Floating atanh

gAsinh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAsinh = unaryOp @Floating asinh

gAcosh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAcosh = unaryOp @Floating acosh

gAtanh
    :: forall a. (ADTRecord a, Constraints a Floating)
    => a -> a
gAtanh = unaryOp @Floating atanh

