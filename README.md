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

## License

Copyright © 2015 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
