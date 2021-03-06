-----------------------------------------------------------------------------
--
-- Module      :  Data.STSequence
-- Copyright   :
-- License     :
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Mutable Sequences. Just a humble start.
--  An enhanced version of STArrays.
--
-----------------------------------------------------------------------------

module Data.STSequence (
    empty,
    emptyWithBufferSize,
    fromArray,
    toArray,

    STSequence,

    null,
    length,

    (>>),
    push,
    pop,
    pushAll,
    concat,

    peek,
    first,
    last,
    poke,
    update
    )

where

import Prelude
import Control.Monad.ST (ST, STRef, readSTRef, writeSTRef, modifySTRef, newSTRef)
import Data.Array as A
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, Fn4)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(Nothing, Just))
import Extensions (undef, unsafeCoerce)

foreign import data STArray :: Type -> Type -> Type

type STSequencePrim h a = {
    buffer :: STArray h a,
    fillCounter :: STRef h Int
}

newtype STSequence h a = STSequence (STSequencePrim h a)


--------------------------------------------------------------------------------
-- Primitives ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. Array a -> Eff (st :: ST h | r) (STArray h a)
thaw = copyImpl

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array a)
freeze = copyImpl

-- | Create an immutable copy of a mutable array.
unsafeFreeze :: forall a h. STArray h a -> (Array a)
unsafeFreeze = unsafeCoerce

foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | Change the value at the specified index in a mutable array.
foreign import pokeSTArray :: forall a h r. Fn3 (STArray h a) Int a (Eff (st :: ST h | r) Boolean)

-- | Change the value at the specified index in a mutable array.
foreign import pokeAllSTArray :: forall a h r. Fn3 (STArray h a) Int (Array a) (Eff (st :: ST h | r) Boolean)

foreign import spliceSTArray :: forall a h r. Fn4 (STArray h a) Int Int (Array a) (Eff (st :: ST h | r) (Array a))

-- | Append the values in an immutable array to the end of a mutable array.
foreign import pushAllSTArray :: forall a h r. Fn2 (STArray h a) (Array a) (Eff (st :: ST h | r) Int)

foreign import peekSTArrayImplUnsafe :: forall a h e. Fn2 (STArray h a) Int (Eff (st :: ST h | e) a)

-- | Create an array with repeated instances of a value.
foreign import replicate :: forall a. Int -> a -> Array a

--------------------------------------------------------------------------------
-- STSequence creation ---------------------------------------------------------
----------------------------

initialSize :: Int
initialSize = 30

-- | Create a sequence with no elements.
-- |
empty :: forall a h r. Eff (st :: ST h | r) (STSequence h a)
empty = do
    a <- thaw (replicate initialSize (undef :: a))
    ref <- newSTRef 0
    pure $ STSequence {buffer : a, fillCounter : ref}

-- | Create a sequence with with no elements with a specified buffer size.
-- |
emptyWithBufferSize :: forall a h r. Int -> Eff (st :: ST h | r) (STSequence h a)
emptyWithBufferSize i = do
    a <- thaw (replicate i (undef :: a))
    ref <- newSTRef 0
    pure $ STSequence {buffer : a, fillCounter : ref}

-- | Convert an array to a STSequence
fromArray ::  forall a h r. Array a -> Eff (st :: ST h | r) (STSequence h a)
fromArray array = do
    let bufferGrowth = round (toNumber (A.length array) * 0.7)
    a <- thaw (array <> replicate bufferGrowth (undef :: a))
    ref <- newSTRef (A.length array)
    pure $ STSequence {buffer : a, fillCounter : ref}

-- | Convert a STSequence to an Array
toArray ::  forall a h r. (STSequence h a) -> Eff (st :: ST h | r) (Array a)
toArray (STSequence seq) = do
    fill <- readSTRef seq.fillCounter
    pure (A.slice 0 fill (unsafeFreeze seq.buffer))

--------------------------------------------------------------------------------
-- STSequence size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a sequence is empty.
null :: forall h a r. STSequence h a -> Eff (st :: ST h | r) Boolean
null seq = do
    l <- length seq
    pure (l == 0)

-- | Get the length of a sequence
length :: forall h a r. STSequence h a ->  Eff (st :: ST h | r) Int
length (STSequence seq) = readSTRef seq.fillCounter

-- | Get the buffer size of a sequence
-- |
bufferSize :: forall h a. STSequence h a ->  Int
bufferSize (STSequence seq) = A.length (unsafeFreeze seq.buffer)

--------------------------------------------------------------------------------
-- Extending arrays ------------------------------------------------------------
--------------------------------------------------------------------------------

infixr 6 push as >>

-- | Append an element to the end of a mutable sequence.
push :: forall h a r. STSequence h a -> a -> Eff (st :: ST h | r) (STSequence h a)
push s@(STSequence seq) ele = do
    fill <- readSTRef seq.fillCounter
    let bs = bufferSize s
    _ <-  if bs > fill
            then do
                    runFn3 pokeSTArray seq.buffer fill ele
            else do
                    let bufferGrowth = round (toNumber bs * 0.7)
                    _ <- runFn2 pushAllSTArray seq.buffer (replicate bufferGrowth (undef :: a))
                    runFn3 pokeSTArray seq.buffer fill ele
    _ <- modifySTRef seq.fillCounter (\i -> i + 1)
    pure s

-- | Remove an element from the end of a mutable sequence.
pop :: forall h a r. STSequence h a -> Eff (st :: ST h | r) (Maybe a)
pop s@(STSequence seq) = do
    fill <- readSTRef seq.fillCounter
    if fill == 0
        then pure Nothing
        else do
            res <- runFn2 peekSTArrayImplUnsafe seq.buffer (fill - 1)
            _ <- runFn3 pokeSTArray seq.buffer (fill - 1) (undef :: a)
            _ <- modifySTRef seq.fillCounter (\i -> i - 1)
            pure (Just res)

-- | Append the values in an immutable array to the end of a mutable sequence.
pushAll :: forall h a r. STSequence h a -> Array a -> Eff (st :: ST h | r) (STSequence h a)
pushAll s@(STSequence seq) r = do
    fill <- length s
    let arrayLength = A.length r
        totalSize = fill + arrayLength
    _ <- if bufferSize s >= totalSize
        then do
            runFn3 pokeAllSTArray seq.buffer fill r
        else do
            let bufferNewSize = round (toNumber totalSize * 1.7)
                bufferGrowth = bufferNewSize - bufferSize s
            _ <- runFn2 pushAllSTArray seq.buffer (replicate bufferGrowth (undef :: a))
            runFn3 pokeAllSTArray seq.buffer fill r
    _ <- writeSTRef seq.fillCounter totalSize
    pure s

-- | Concat two STSequences.
-- TODO: optimize
concat  :: forall h a r. STSequence h a -> STSequence h a -> Eff (st :: ST h | r) (STSequence h a)
concat s1 s2 = do
    r <- toArray s2
    pushAll s1 r

-- * Reading and updating values

-- | Read the value at the specified index in a mutable array.
peek
  :: forall a h r
   . STSequence h a
  -> Int
  -> Eff (st :: ST h | r) (Maybe a)
peek seq@(STSequence s) i = do
    l <- length seq
    if i >= 0 && i < l
        then liftM1 Just (runFn2 peekSTArrayImplUnsafe s.buffer i)
        else pure Nothing

first :: forall a h r . STSequence h a -> Eff (st :: ST h | r) (Maybe a)
first s@(STSequence seq) = peek s 0

last :: forall a h r . STSequence h a -> Eff (st :: ST h | r) (Maybe a)
last s@(STSequence seq) = do
    fill <- readSTRef seq.fillCounter
    peek s (fill - 1)


-- | Change the value at the specified index in a mutable array.
poke :: forall a h r. STSequence h a -> Int -> a -> Eff (st :: ST h | r) Boolean
poke seq@(STSequence s) i val = do
    l <- length seq
    if i >= 0 && i < l
        then do
            _ <- runFn3 pokeSTArray s.buffer i val
            pure true
        else pure false

-- | Change the value at the specified index in a mutable array from a function, which receives the old value.
update :: forall a h r. STSequence h a -> Int -> (a -> a) -> Eff (st :: ST h | r) Boolean
update seq@(STSequence s) i func = do
    l <- length seq
    if i >= 0 && i < l
        then do
            old <- runFn2 peekSTArrayImplUnsafe s.buffer i
            _ <- runFn3 pokeSTArray s.buffer i (func old)
            pure true
        else pure false
