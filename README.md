one-liner-instances
===================

This package uses machinery from *[one-liner][]* in order to provide default
implementations for methods from `Num`, `Fractional`, `Floating`, `Semigroup`,
and `Monoid`.  These will work for any types (deriving `Generic`) that are
made with one constructor, whose fields are all instances of that typeclass.

[one-liner]: https://hackage.haskell.org/package/one-liner

So, `gPlus` (generic addition) will work for:

```haskell
data Tup1 a b = Tup1 a b            -- requires Num a, Num b
data Tup2 a   = Tup2 Int a          -- requires Num a, Num b
data Tup3     = Tup3 Int Double
data Tup4 a b = Tup4 Int Double     -- no constraint on a or b
```

But not on:

```haskell
data Tup5 a   = Tup2 String a       -- String is not an instance of Num
```

These are implemented by applying the operation to every field.

Newtype wrappers
----------------

Similar to `WrappedMonoid` and `WarppedMonad` from *base*, some convenient
newtype wrappers are provided that will give free instances of `Num`, etc. for
appropriate types:

If `a` is a data type (deriving `Generic`) with a single constructor whose
fields all have instances of `Num`, then `GNum a` has a `Num` instance (and
same for `Fractional`, `Floating`, etc.).

If `a` is a data type (deriving `Generic`) with a single constructor whose
fields all have instances of `Semigroup`, then `GMonoid a` has a `Semigroup`
instance (and same for `Monoid`).
