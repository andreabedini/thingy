# Thingy

An applicative instance for lists.

## Background

The
[QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.13.1/docs/Test-QuickCheck-Arbitrary.html)
documentation has a few paragraphs describing how to implement correctly
the function `shrink` for your `Arbitrary` instance.

What follow is an extract from QuickCheck documentation:

---

Most implementations of shrink should try at least three things:

1. Shrink a term to any of its immediate subterms. You can use subterms to do
this.
2. Recursively apply shrink to all immediate subterms. You can use
recursivelyShrink to do this.
3. Type-specific shrinkings such as replacing a constructor by a simpler
constructor.

For example, suppose we have the following implementation of binary trees:

```haskell
data Tree a = Nil | Branch a (Tree a) (Tree a)
```

We can then define shrink as follows:

```haskell
shrink Nil = []
shrink (Branch x l r) =
  -- shrink Branch to Nil
  [Nil] ++
  -- shrink to subterms
  [l, r] ++
  -- recursively shrink subterms
  [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]
```

There are a couple of subtleties here:

QuickCheck tries the shrinking candidates in the order they appear in the list,
so we put more aggressive shrinking steps (such as replacing the whole tree by
Nil) before smaller ones (such as recursively shrinking the subtrees).

It is tempting to write the last line as
```haskell
[Branch x' l' r' | x' <- shrink x, l' <- shrink l, r' <- shrink r]
```
but this is the wrong thing! It will force QuickCheck to shrink x, l and r
in tandem, and shrinking will stop once one of the three is fully shrunk.

---

The meaning of "in tandem" in the last sentence was a bit vague for me so I
had to unpack it a little bit to understand what the point was.

Let's forget about the node value `x` and assume shrinking `l` and `r`
gives three values each.

```haskell
shrink l = [l1, l2, l3]
shrink r = [r1, r2, r3]
```

What the last sentence above is saying is that the shrink function should
not produce the sequence

```haskell
[ Branch l1 r1
, Branch l1 r2
, Branch l1 r3
, Branch l2 r1
, Branch l2 r2
, Branch l2 r3
, Branch l3 r1
, Branch l3 r2
, Branch l3 r3
]
```

but instead

```haskell
[ Branch l r1
, Branch l r3
, Branch l r3
, Branch l1 r
, Branch l2 r
, Branch l3 r
]
```

I can only guess that by "in tandem" the authors of the documentation meant
we have terms where both sides are shrunk (e.g. `(l2, r3)`).

The desired behaviour is hidden in the shrink implementation for tuples.

```
λ> import Test.QuickCheck
λ> shrink 'd'
"abc"
λ> shrink ('d', 'd')
[('a','d'),('b','d'),('c','d'),('d','a'),('d','b'),('d','c')]
```

This is all nice but I was doing some experiments implementing shrink for a
data structure with additional invariants and I felt like I needed a way to
"push" a particular shrink function (of type `a -> [a]`) deep inside a
nested structure and "pull the results out" correctly.

Let me explain.

Say I have `stuff :: Map k a` and a function `myShrink :: a -> [a]`.
Shrinking the map values is not a problem since we can exploit the functor
instance for `Map k` to shrink each value independently.

```haskell
myShrink <$> stuff :: Map k [a]
```

But then we can't obtain a list of shrunk maps `Map k [a]` without relying
on the applicative instance for `[a]` which has the wrong behaviour seen
above.

```haskell
λ> traverse_ print $ sequence $ shrink <$> m
fromList [(1,'a'),(2,'a')]
fromList [(1,'a'),(2,'b')]
fromList [(1,'a'),(2,'c')]
fromList [(1,'b'),(2,'a')]
fromList [(1,'b'),(2,'b')]
fromList [(1,'b'),(2,'c')]
fromList [(1,'c'),(2,'a')]
fromList [(1,'c'),(2,'b')]
fromList [(1,'c'),(2,'c')]
```

This led me to think there might be a more suitable applicative instance
for lists, different from the monadic and the zipper ones.

## Enter my "Thingy"

Following the intuition above, I cooked up this applicative instance for
lists.

```haskell
newtype Thingy a = Thingy { unThingy :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative Thingy where

  pure x = Thingy [x]

  (Thingy _ ) <*> (Thingy [])         = Thingy []
  (Thingy []) <*> (Thingy _ )         = Thingy []
  (Thingy (f:fs)) <*> (Thingy (x:xs)) = Thingy $ f x : [ f' x | f' <- fs ] ++ [ f x | x <- xs ]
```

Which gives the right behaviour as demonstrated below

```haskell
λ> t = [0,0,0]                         -- list to shrink
λ> shink a = [ a + i | i <- [1,2,3] ]  -- shrink function
λ> map shrink t
[[1,2,3],[1,2,3],[1,2,3]]
λ> traverse_ print $ unThingy $ traverse (Thingy . shrink) t
[1,1,1]
[2,1,1]
[3,1,1]
[1,2,1]
[1,3,1]
[1,1,2]
[1,1,3]
```

This instance seems to be lawful (this repository includes some tests) but
I don't have a formal proof.

## Questions

1. Is this a known instance?
2. Can it be obtained from other well-known constructions?

## Prior art

After publishing this note Arnaud Spiwack pointed be to the following works

- https://hackage.haskell.org/package/successors
- https://www.tweag.io/blog/2020-12-03-shrinks-applicative

This seems to be a common (and indeed useful) reinvention.
