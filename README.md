# Reversing function arguments

This module provides the `reverseArgs` function which flips
the arguments of a function of arbitrary arity. The return
value of the flipped function can not be fully polymorphic
as this could imply it is a function.

Example:

```haskell
myFlip :: (a -> b -> c -> d -> [e]) -> d -> c -> b -> a -> [e]
myFlip = reverseArgs
```

However, if you supply a proof (of the form `IsFun a ~ 'False`)
that a is not a function, you can also return a polymorphic type.

Example:

```haskell
myFlip :: IsFun e ~ 'False => (a -> b -> c -> d -> e) -> d -> c -> b -> a -> e
myFlip = reverseArgs
```