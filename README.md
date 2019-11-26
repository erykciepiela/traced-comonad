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
So CoKliesli category over traced comonad for fixed monoid `m` will be defined by:

* identity `extract :: Monoid m => (m -> a) -> a` just as previously defined
* associative composition `(=>=) :: Monoid m => ((m -> a) -> b) -> ((m -> b) -> c) -> ((m -> a) -> c)` (parens added for readability). Note this function is already defined in `Control.Comonad` and is polymorhpic for all comonads.

Knowing that a category supports some specific notion of computation (having such examples for many categories), what is the notion of computation behind CoKliesli category over traced comonad?

## Where traced commonad helps in the real world

Having intution about comonads that they describe a computation in some context, for traced commonad such context is some monoidal environment: environment that starts with `mempty` value and `mappend`s consequitive values, stacking them one over another and squashing together.

An example of such monoidal environment can be a `ViewPoint` consisting of `x` and `y` coordinates (let's assume 2D plane here) where an observer stands, angle `a` which is the orientation of an observer, and scale factor `s` by which all object should appear to the obsever.

It's not hard to find out that `ViewPoint` is a monoid.
Let's assume it's our monoidal enviornment.
Therefore, an arrow from `a` to `b` in our category is a function `(ViewPoint -> a) -> b`. Such arrow must either a) create `b` out of thin air or b) somehow know about `ViewPoint`(s), turn them into `a`(s) and put them together in a `b`.

Function `squareFromDots :: Float -> (ViewPoint -> Dot) -> Square` constucts a square centered at `(0, 0)` with given width/height as first parameter, by constructing 4 `Dot`s at its corners (`(-w/2, -w/2)`, `(-w/2, w/2)`, `(w/2, w/2)`, `(w/2, -w/2)`) and making a square of these. Therefore, partially applied `squareFromDots f` (where `f` is `Float` number) is a valid arrow from `Dot` to `Square` in our category.

More generally, an arrow from `a` to `b` is a function that lays out `a`s on relative `ViewPoint`s and put the `a`s together in a `b`.

We should notice that an arrow from `a` to `b` doesn't specify what `a`s should be, it just places them in a relative `ViewPoint`.
`a`s are manufactured externally, an arrow only specifies in which relative `ViewPoint` they should be manufactured.
An arrow, however, knows of what type `a`s are, what `a`s have been already manufactured and can use this knowledge for example for a) setting a proper `ViewPoint`s for another `a`s to be produced or b) derive properties of a `b` that is returned.

Examples of arrows are :
`star :: Color -> (ViewPoint -> ViewPoint) -> Mark`
`dottedPolygon :: Color -> [Point] -> (ViewPoint -> Mark) -> Polygon`
`solidFill :: Color -> (ViewPoint -> Polygon) -> Area`
`star Black =>= dottedPolygon Blue [(0, 1), (2, 3), (4, 5)] =>= solidFill LightBlue`

## References

* *Comonads: what are they and what can you do with them* https://www.slideshare.net/davidoverton/comonad
