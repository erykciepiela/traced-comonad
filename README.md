# Traced Comonad

Traced comonad is one of the simplest comonads to discover, as it's dual to fairly trivial writer monad, although might be cumbersome to grok. It's neither likely to come accross in Haskell projects. How come such, as it may seem, trivial case of comonad is so rarely used?

In order to build better intuition about it let me first analyse its definition and then come up with real world examples.

## How is traced comonad defined

```haskell
instance Monoid m => Comonad ((->) m) where
  extract :: Monoid m => (m -> a) -> a
  extract f = f mempty
  duplicate :: Monoid m => (m -> a) -> (m -> m -> a)
  duplicate f = \m m' -> f (m <> m')
  extend :: Monoid m => ((m -> a) -> b) -> (m -> a) -> (m -> b)
  extend f m2a = \m -> f (\m' -> m2a (m <> m'))
```

While this isn't actual implementation known from `Control.Comonad` module, it's equivalent and potentially easier to understand.

We know that each comonad has associated category, known as CoKleisli category.
So CoKliesli category over traced comonad will be defined by:

* identity `extract :: Monoid m => (m -> a) -> a` just as previously defined
* associative composition `(=>=) :: Monoid m => ((m -> a) -> b) -> ((m -> b) -> c) -> ((m -> a) -> c)`

Note that the latter is already provided in `Control.Comonad` module for every comonad instance.

Knowing that a category supports some specific notion of computation (having such examples for many categories), what is the notion of computation behind CoKliesli category over traced comonad?

## Where Traced commonad helps in the real world

Having intution about comonads that they describe a computation in some context, for traced commonad such context is some monoidal environment: environment that starts with `mempty` value and `mappend`s consequitive values, stacking them one over another and squashing.

An example of such monoidal environment can be a `ViewPoint` consisting of `x` and `y` coordinates (let's assume 2D plane here) where an observer stands, and angle `a` which is the orientation of an observer.

It's not hard to find out that `ViewPoint` is a monoid. 
Let's assume it's our monoidal enviornment.
Therefore, an arrow from `a` to `b` in our category is a function `(ViewPoint -> a) -> b`. Such arrow must either a) create `b` out of thin air or b) somehow know about `ViewPoint`(s) and a way how to turn `a`(s) into `b`.

What could `ViewPoint -> a` be? If `a` is a point on plain

`extract :: (ViewPoint -> a) -> a`

`(=>=) :: ((ViewPoint -> a) -> b) -> ((ViewPoint -> b) -> c) -> ((ViewPoint -> a) -> c)`

---
Let's see then what's the category over traced comonad over `ViewPoint`.
