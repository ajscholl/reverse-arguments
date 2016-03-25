{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function.Reverse
-- Copyright   :  (c) Anselm Jonas Scholl 2016
-- License     :  BSD3
--
-- Maintainer  :  anselm.scholl@tu-harburg.de
-- Stability   :  experimental
-- Portability :  non-portable (uses type families and more)
--
-- This module provides the 'reverseArgs' function which flips the arguments
-- of a function of arbitrary arity. The return value of the flipped function
-- can not be fully polymorphic as this could imply it is a function.
--
-- Example:
--
-- > myFlip :: (a -> b -> c -> d -> [e]) -> d -> c -> b -> a -> [e]
-- > myFlip = reverseArgs
--
-- However, if you supply a proof (of the form @IsFun a ~ 'False@) that a is not
-- a function, you can also return a polymorphic type.
--
-- Example:
--
-- > myFlip :: IsFun e ~ 'False => (a -> b -> c -> d -> e) -> d -> c -> b -> a -> e
-- > myFlip = reverseArgs
--

module Data.Function.Reverse where

import Data.Coerce
import Data.Functor.Identity
import Data.Type.Bool

-- * Reversing arguments

-- | Reverse the arguments of a function. Does work with polymorphic return
--   values if you supply a proof that the result is not a function.
{-# INLINEABLE reverseArgs #-}
reverseArgs :: (ReverseArgs (BoxResult a),
        Coercible a (BoxResult a),
        Coercible (ReversedArgs a) (BoxResult (ReversedArgs a)),
        ReversedArgs (BoxResult a) ~ BoxResult (ReversedArgs a)) => a -> ReversedArgs a
reverseArgs = unboxResult . reverseArgs' . boxResult

-- * Utilities

-- | Determine whether the argument is a function.
type family IsFun f where
    IsFun (a -> b) = 'True
    IsFun a = 'False

-- * Internal types and functions

-- ** Applying the last argument

-- | Apply the last argument of a function to it.
type family ApplyLast a z where
    ApplyLast (a -> b) z = If (IsFun b) (a -> ApplyLast b z) (MatchLastArg a b z)

-- | Match the last argument away.
type family MatchLastArg a b z where
    MatchLastArg z b z = b

-- | Like 'ApplyLast', but on the value level.
class ApplyingLast f z | f -> z where
    -- | Apply a function f to its last argument z.
    applyLast :: f -> z -> ApplyLast f z

-- base case: Just apply the function to the argument
instance {-# OVERLAPPABLE #-} (IsFun b ~ 'False, a ~ c) => ApplyingLast (a -> b) c where
    {-# INLINABLE applyLast #-}
    applyLast = id

-- induction step: shift to the right and recurse
instance {-# OVERLAPPING #-} (ApplyLast (a -> b -> c) z ~ (a -> ApplyLast (b -> c) z),
                             ApplyingLast (b -> c) z) => ApplyingLast (a -> b -> c) z where
    {-# INLINABLE applyLast #-}
    applyLast f z x = applyLast (f x) z

-- ** Reversing arguments

-- | Reverse the arguments of a function.
type ReversedArgs a = If (IsFun a) (ReverseOneArg a) a

-- | Reverse one of the arguments of a function and recurse for the rest.
type family ReverseOneArg a where
    ReverseOneArg (a -> b) = InsertAtEnd a (ReversedArgs b)

-- | Insert an argument at the end.
type InsertAtEnd a f = If (IsFun f) (InsertAtEndStep a f) (a -> f)

-- | Shift one to the left and insert something at the end.
type family InsertAtEndStep a f where
    InsertAtEndStep a (x -> y) = x -> InsertAtEnd a y

class ReverseArgs a where
    -- | Reverse the arguments of some function. Does not work with functions
    --   with fully polymorphic return values, use 'reverseArgs' instead.
    reverseArgs' :: a -> ReversedArgs a

-- base case: non-functions are already reversed
instance {-# OVERLAPPABLE #-} ReversedArgs a ~ a => ReverseArgs a where
    {-# INLINABLE reverseArgs' #-}
    reverseArgs' a = a

-- induction step: shift argument to the last position, apply it there and recurse
instance {-# OVERLAPPING #-} (ApplyingLast (a -> b) z,
                             ReversedArgs (a -> b) ~ (z -> r),
                             ReversedArgs (ApplyLast (a -> b) z) ~ r,
                             ReverseArgs (ApplyLast (a -> b) z)) => ReverseArgs (a -> b) where
    {-# INLINABLE reverseArgs' #-}
    reverseArgs' f z = reverseArgs' (applyLast f z)

-- ** Boxing results

-- | Box the result in the 'Identity' monad.
type BoxResult a = If (IsFun a) (BoxArrow a) (Identity a)

-- | Box the result of a function in the 'Identity' monad.
type family BoxArrow a where
    BoxArrow (a -> b) = a -> BoxResult b

-- | Box the result in the 'Identity' monad.
{-# INLINEABLE boxResult #-}
boxResult :: Coercible a (BoxResult a) => a -> BoxResult a
boxResult = coerce

-- | Unbox the result, which is in the 'Identity' monad.
{-# INLINEABLE unboxResult #-}
unboxResult :: Coercible a (BoxResult a) => BoxResult a -> a
unboxResult = coerce