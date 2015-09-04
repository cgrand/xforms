# xforms

More transducers and reducing functions for Clojure!

Transducers: `reduce`, `into`, `by-key`, `partition`, `pad` and `for`.

Reducing functions: `str`, `str!`, `avg`, `count`, `juxt`, `juxt-map`.

## Usage

Add this dependency to your project:

```clj
[net.cgrand/xforms "0.1.0-SNAPSHOT"]
```

```clj
=> (require '[net.cgrand.xforms :as x])
```

`str` and `str!` are two reducing functions to build Strings and StringBuilders in linear time.

```clj
=> (quick-bench (reduce str (range 256)))
             Execution time mean : 58,714946 µs
=> (quick-bench (reduce x/str (range 256)))
             Execution time mean : 11,609631 µs
```

`for` is the transducing cousin of `clojure.core/for`:

```clj
=> (quick-bench (reduce + (for [i (range 128) j (range i)] (* i j))))
             Execution time mean : 514,932029 µs
=> (quick-bench (transduce (x/for [i % j (range i)] (* i j)) + 0 (range 128)))
             Execution time mean : 373,814060 µs
```

`by-key` and `reduce` are two new transducers. Here is an example usage:

```clj
;; reimplementing group-by
(defn my-group-by [kfn coll]
  (into {} (x/by-key kfn (x/reduce conj)) coll))

;; let's go transient!
(defn my-group-by [kfn coll]
  (into {} (x/by-key kfn (x/into [])) coll))

=> (quick-bench (group-by odd? (range 256)))
             Execution time mean : 29,356531 µs
=> (quick-bench (my-group-by odd? (range 256)))
             Execution time mean : 20,604297 µs
```

Like `by-key`, `partition` also takes a transducer as an argument to allow further computation on the partition without buffering.

```clj
=> (sequence (x/partition 4 (x/reduce +)) (range 16))
(6 22 38 54)
```

Padding can be achieved using the `pad` function:

```clj
=> (sequence (x/partition 4 (comp (x/pad 4 (repeat :pad)) (x/into []))) (range 9))
([0 1 2 3] [4 5 6 7] [8 :pad :pad :pad])
```


`avg` is a reducing fn to compute the arithmetic mean. `juxt` and `juxt-map` are used to compute several reducing fns at once.

```clj
=> (into {} (x/by-key odd? (x/reduce (x/juxt + x/avg))) (range 256))
{false [16256 127], true [16384 128]}
=> (into {} (x/by-key odd? (x/reduce (x/juxt-map :sum + :mean x/avg :count x/count))) (range 256))
{false {:sum 16256, :mean 127, :count 128}, true {:sum 16384, :mean 128, :count 128}}
```

## On Partitioning

Both `by-key` and `partition` takes a transducer as parameter. This transducer is used to further process each partition.

It's worth noting that all transformed outputs are subsequently interleaved. See:

```clj
=> (sequence (x/partition 2 1 identity) (range 8))
(0 1 1 2 2 3 3 4 4 5 5 6 6 7 7)
=> (sequence (x/by-key odd? identity) (range 8))
([false 0] [true 1] [false 2] [true 3] [false 4] [true 5] [false 6] [true 7])
```

That's why most of the time the last stage of the sub-transducer will be a `x/reduce` or a `x/into`:

```clj
=> (sequence (x/partition 2 1 (x/into [])) (range 8))
([0 1] [1 2] [2 3] [3 4] [4 5] [5 6] [6 7] [7])
=> (sequence (x/by-key odd? (x/into [])) (range 8))
([false [0 2 4 6]] [true [1 3 5 7]])
```

## License

Copyright © 2015 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
