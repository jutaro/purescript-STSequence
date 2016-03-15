## Module Data.STSequence



#### `STSequence`

``` purescript
newtype STSequence h a
```

#### `empty`

``` purescript
empty :: forall a h r. Eff (st :: ST h | r) (STSequence h a)
```

Create a sequence with a single element.


#### `emptyWithBufferSize`

``` purescript
emptyWithBufferSize :: forall a h r. Int -> Eff (st :: ST h | r) (STSequence h a)
```

Create a sequence with a single element.


#### `fromArray`

``` purescript
fromArray :: forall a h r. Array a -> Eff (st :: ST h | r) (STSequence h a)
```

#### `toArray`

``` purescript
toArray :: forall a h r. STSequence h a -> Eff (st :: ST h | r) (Array a)
```

#### `null`

``` purescript
null :: forall h a r. STSequence h a -> Eff (st :: ST h | r) Boolean
```

Test whether a sequence is empty.

Running time: `O(1)`

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


