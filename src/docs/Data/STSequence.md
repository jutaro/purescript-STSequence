## Module Data.STSequence

Mutable Sequences. Just a humble start.

#### `STSequence`

``` purescript
newtype STSequence h a
```

#### `empty`

``` purescript
empty :: forall a h r. Eff (st :: ST h | r) (STSequence h a)
```

Create a sequence with no elements.


#### `emptyWithBufferSize`

``` purescript
emptyWithBufferSize :: forall a h r. Int -> Eff (st :: ST h | r) (STSequence h a)
```

Create a sequence with with no elements with a specified buffer size.


#### `fromArray`

``` purescript
fromArray :: forall a h r. Array a -> Eff (st :: ST h | r) (STSequence h a)
```

Convert an array to a STSequence

#### `toArray`

``` purescript
toArray :: forall a h r. STSequence h a -> Eff (st :: ST h | r) (Array a)
```

Convert a STSequence to an Array

#### `null`

``` purescript
null :: forall h a r. STSequence h a -> Eff (st :: ST h | r) Boolean
```

Test whether a sequence is empty.

#### `length`

``` purescript
length :: forall h a r. STSequence h a -> Eff (st :: ST h | r) Int
```

Get the length of a sequence

#### `(>>)`

``` purescript
infixr 6 push as >>
```

_right-associative / precedence 6_

#### `push`

``` purescript
push :: forall h a r. STSequence h a -> a -> Eff (st :: ST h | r) (STSequence h a)
```

Append an element to the end of a mutable sequence.

#### `pushAll`

``` purescript
pushAll :: forall h a r. STSequence h a -> Array a -> Eff (st :: ST h | r) (STSequence h a)
```

Append the values in an immutable array to the end of a mutable sequence.

#### `concat`

``` purescript
concat :: forall h a r. STSequence h a -> STSequence h a -> Eff (st :: ST h | r) (STSequence h a)
```

Concat two STSequences.


