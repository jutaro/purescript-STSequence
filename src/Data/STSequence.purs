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
    pushAll,
    concat,

    peek,
    poke
    )

where

import Prelude ((+), return, ($), bind, (++), (*), (>=), (==), (>),(-),(&&),(<), liftM1)
import Data.Array as A
import Control.Monad.Eff (Eff)
import Control.Monad.ST
import Data.Int (round, toNumber)
import Data.Function (Fn2, runFn2, Fn3, runFn3, Fn4)
import Data.Maybe (Maybe(Nothing, Just))
import Extensions (undef,unsafeCoerce)

foreign import data STArray :: * -> * -> *

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

foreign import peekSTArrayImplUnsafe :: forall a h e r. Fn2 (STArray h a) Int (Eff (st :: ST h | e) r)

--------------------------------------------------------------------------------
-- STSequence creation ---------------------------------------------------------
----------------------------

initialSize :: Int
initialSize = 30

-- | Create a sequence with no elements.
-- |
empty :: forall a h r. Eff (st :: ST h | r) (STSequence h a)
empty = do
    a <- thaw (A.replicate initialSize (undef :: a))
    ref <- newSTRef 0
    return $ STSequence {buffer : a, fillCounter : ref}

-- | Create a sequence with with no elements with a specified buffer size.
-- |
emptyWithBufferSize :: forall a h r. Int -> Eff (st :: ST h | r) (STSequence h a)
emptyWithBufferSize i = do
    a <- thaw (A.replicate i (undef :: a))
    ref <- newSTRef 0
    return $ STSequence {buffer : a, fillCounter : ref}

-- | Convert an array to a STSequence
fromArray ::  forall a h r. Array a -> Eff (st :: ST h | r) (STSequence h a)
fromArray array = do
    let bufferGrowth = round (toNumber (A.length array) * 0.7)
    a <- thaw (array ++ A.replicate bufferGrowth (undef :: a))
    ref <- newSTRef (A.length array)
    return $ STSequence {buffer : a, fillCounter : ref}

-- | Convert a STSequence to an Array
toArray ::  forall a h r. (STSequence h a) -> Eff (st :: ST h | r) (Array a)
toArray (STSequence seq) = do
    fill <- readSTRef seq.fillCounter
    return (A.slice 0 fill (unsafeFreeze seq.buffer))

--------------------------------------------------------------------------------
-- STSequence size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a sequence is empty.
null :: forall h a r. STSequence h a -> Eff (st :: ST h | r) Boolean
null seq = do
    l <- length seq
    return (l == 0)

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
    if bs > fill
        then do
                runFn3 pokeSTArray seq.buffer fill ele
        else do
                let bufferGrowth = round (toNumber bs * 0.7)
                runFn2 pushAllSTArray seq.buffer (A.replicate bufferGrowth (undef :: a))
                runFn3 pokeSTArray seq.buffer fill ele
    modifySTRef seq.fillCounter (\i -> i + 1)
    return s

-- | Append the values in an immutable array to the end of a mutable sequence.
pushAll :: forall h a r. STSequence h a -> Array a -> Eff (st :: ST h | r) (STSequence h a)
pushAll s@(STSequence seq) r = do
    fill <- length s
    let arrayLength = A.length r
        totalSize = fill + arrayLength
    if bufferSize s >= totalSize
        then do
            runFn3 pokeAllSTArray seq.buffer fill r
        else do
            let bufferNewSize = round (toNumber totalSize * 1.7)
                bufferGrowth = bufferNewSize - bufferSize s
            runFn2 pushAllSTArray seq.buffer (A.replicate bufferGrowth (undef :: a))
            runFn3 pokeAllSTArray seq.buffer fill r
    writeSTRef seq.fillCounter totalSize
    return s

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
        else return Nothing


-- | Change the value at the specified index in a mutable array.
poke :: forall a h r. STSequence h a -> Int -> a -> Eff (st :: ST h | r) Boolean
poke seq@(STSequence s) i val = do
    l <- length seq
    if i >= 0 && i < l
        then do
            runFn3 pokeSTArray s.buffer i val
            return true
        else return false
