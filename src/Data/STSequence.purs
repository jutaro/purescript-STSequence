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
-- |
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
    pushAll
    )

where

import Prelude ((+), return, ($), bind, (++), (*), (<=), (==))
import Data.Array as A
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Int (round, toNumber)
import Data.Function (Fn3, runFn3, Fn4, runFn4)
import Extensions (undef,unsafeCoerce)

foreign import data STArray :: * -> * -> *

type STSequencePrim h a = {
    buffer :: STArray h a,
    fillCounter :: Int
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

foreign import spliceSTArray :: forall a h r. Fn4 (STArray h a) Int Int (Array a) (Eff (st :: ST h | r) (Array a))

--------------------------------------------------------------------------------
-- STSequence creation ---------------------------------------------------------
--------------------------------------------------------------------------------

initialSize :: Int
initialSize = 30

-- | Create a sequence with a single element.
-- |
empty :: forall a h r. Eff (st :: ST h | r) (STSequence h a)
empty = do
    a <- thaw (A.replicate initialSize (undef :: a))
    return $ STSequence {buffer : a, fillCounter : 0}

-- | Create a sequence with a single element.
-- |
emptyWithBufferSize :: forall a h r. Int -> Eff (st :: ST h | r) (STSequence h a)
emptyWithBufferSize i = do
    a <- thaw (A.replicate i (undef :: a))
    return $ STSequence {buffer : a, fillCounter : 0}

fromArray ::  forall a h r. Array a -> Eff (st :: ST h | r) (STSequence h a)
fromArray array = do
    let bufferGrowth = round (toNumber (A.length array) * 0.7)
    a <- thaw (array ++ A.replicate bufferGrowth (undef :: a))
    return $ STSequence {buffer : a, fillCounter : A.length array}

toArray ::  forall a h r. (STSequence h a) -> Eff (st :: ST h | r) (Array a)
toArray (STSequence seq) = return (A.slice 0 seq.fillCounter (unsafeFreeze seq.buffer))

--------------------------------------------------------------------------------
-- STSequence size -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a sequence is empty.
-- |
-- | Running time: `O(1)`
null :: forall h a. STSequence h a -> Boolean
null seq = length seq == 0

-- | Get the length of a sequence
-- |
length :: forall h a. STSequence h a ->  Int
length (STSequence seq) = seq.fillCounter

-- | Get the buffer size of a sequence
-- |
bufferSize :: forall h a. STSequence h a ->  Int
bufferSize (STSequence seq) = A.length (unsafeFreeze seq.buffer)

--------------------------------------------------------------------------------
-- Extending arrays ------------------------------------------------------------
--------------------------------------------------------------------------------

infixr 6 push as >>

-- | An infix alias for `Cons`; attaches an element to the front of
-- | a sequence.
-- |
-- | Running time: `O(1)`
push :: forall h a r. STSequence h a -> a -> Eff (st :: ST h | r) (STSequence h a)
push s@(STSequence seq) ele =
    if bufferSize s <= seq.fillCounter
        then do
                runFn3 pokeSTArray seq.buffer seq.fillCounter ele
                return $ STSequence (seq{fillCounter = seq.fillCounter + 1})
        else do
                let bufferGrowth = round (toNumber (bufferSize s) * 0.7)
                newBuffer <- thaw $ unsafeFreeze seq.buffer ++ A.replicate bufferGrowth (undef :: a)
                runFn3 pokeSTArray newBuffer seq.fillCounter ele
                return $ STSequence {buffer: newBuffer, fillCounter : seq.fillCounter + 1}


pushAll :: forall h a r. STSequence h a -> Array a -> Eff (st :: ST h | r) (STSequence h a)
pushAll s@(STSequence seq) r =
    let arrayLength = A.length r
        totalSize = length s + arrayLength
    in if totalSize <= bufferSize s
        then do
            runFn4 spliceSTArray seq.buffer seq.fillCounter arrayLength r
            return $ STSequence (seq{fillCounter = seq.fillCounter + arrayLength})
        else do
            let bufferGrowth = round (toNumber totalSize * 0.7)
            newBuffer <- thaw $ unsafeFreeze seq.buffer ++ A.replicate bufferGrowth (undef :: a)
            runFn4 spliceSTArray newBuffer seq.fillCounter arrayLength r
            return $ STSequence {buffer: newBuffer, fillCounter : seq.fillCounter + 1}
