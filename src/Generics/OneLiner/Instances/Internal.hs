
module Generics.OneLiner.Instances.Internal (
    c0
  , c1
  , c2
  , c2'
  ) where

import           Data.Coerce

c0 :: Coercible a b => b -> a
c0 = coerce

c1 :: Coercible a b => (b -> b) -> a -> a
c1 f = coerce . f . coerce

c2 :: Coercible a b => (b -> b -> b) -> a -> a -> a
c2 f x y = coerce (f (coerce x) (coerce y))

c2' :: Coercible a b => (b -> b -> c) -> a -> a -> c
c2' f x y = f (coerce x) (coerce y)

