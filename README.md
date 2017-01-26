# xforms

More transducers and reducing functions for Clojure(script)!

[![Build Status](https://travis-ci.org/cgrand/xforms.png?branch=master)](https://travis-ci.org/cgrand/xforms)

*Transducers* (in `net.cgrand.xforms`) can be classified in three groups: regular ones, higher-order ones
(which accept other transducers as arguments) and 1-item ones which emit only 1 item out no matter how many went in.
They generally only make sense in the context of a higher-order transducer. 

 * regular ones: `partition` (1 arg), `reductions`, `for`, `window` and `window-by-time`
 * higher-order ones: `by-key`, `multiplex`, `transjuxt`, `partition` (2+ args)
 * 1-item ones: `reduce`, `into`, `transjuxt`, `last`, `count`, `avg`, `sd`, `min`, `minimum`, `max`, `maximum`, `str`
 
*Reducing functions* (in `net.cgrand.xforms.rfs`): `min`, `minimum`, `max`, `maximum`, `str`, `str!`, `avg`, `sd`, `juxt` and `last`.

Transducing contexts: `transjuxt` (for performing several transductions in a single pass), `into`, `count`.

## Usage

Add this dependency to your project:

```clj
[net.cgrand/xforms "0.8.3"]
```

```clj
=> (require '[net.cgrand.xforms :as x])
```

`str` and `str!` are two reducing functions to build Strings and StringBuilders in linear time.

```clj
=> (quick-bench (reduce str (range 256)))
             Execution time mean : 58,714946 µs
=> (quick-bench (reduce rf/str (range 256)))
             Execution time mean : 11,609631 µs
```

`for` is the transducing cousin of `clojure.core/for`:

```clj
=> (quick-bench (reduce + (for [i (range 128) j (range i)] (* i j))))
             Execution time mean : 514,932029 µs
=> (quick-bench (transduce (x/for [i % j (range i)] (* i j)) + 0 (range 128)))
             Execution time mean : 373,814060 µs
```

You can also use `for` like `clojure.core/for`: `(x/for [i (range 128) j (range i)] (* i j))` expands to `(eduction (x/for [i % j (range i)] (* i j)) (range 128))`.

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

Like `by-key`, `partition` also takes a transducer as last argument to allow further computation on the partition.

```clj
=> (sequence (x/partition 4 (x/reduce +)) (range 16))
(6 22 38 54)
```

Padding is achieved as usual:

```clj
=> (sequence (x/partition 4 4 (repeat :pad) (x/into [])) (range 9))
([0 1 2 3] [4 5 6 7] [8 :pad :pad :pad])
```


`avg` is a transducer to compute the arithmetic mean. `transjuxt` is used to perform several transductions at once.

```clj
=> (into {} (x/by-key odd? (x/transjuxt [(x/reduce +) x/avg])) (range 256))
{false [16256 127], true [16384 128]}
=> (into {} (x/by-key odd? (x/transjuxt {:sum (x/reduce +) :mean x/avg :count x/count})) (range 256))
{false {:sum 16256, :mean 127, :count 128}, true {:sum 16384, :mean 128, :count 128}}
```

`window` is a new transducer to efficiently compute a windowed accumulator:

```clj
;; sum of last 3 items
=> (sequence (x/window 3 + -) (range 16))
(0 1 3 6 9 12 15 18 21 24 27 30 33 36 39 42)

=> (def nums (repeatedly 8 #(rand-int 42)))
#'user/nums
=> nums
(11 8 32 26 6 10 37 24)

;; avg of last 4 items
=> (sequence
     (x/window 4 x/avg #(x/avg %1 %2 -1))
     nums)
(11 19/2 17 77/4 18 37/2 79/4 77/4)

;; min of last 3 items
=> (sequence
     (x/window 3
       (fn
         ([] (sorted-set))
         ([s] (first s))
         ([s x] (conj s x)))
       disj)
     nums)
(11 8 8 8 6 6 6 10)
```

## On Partitioning

Both `by-key` and `partition` takes a transducer as parameter. This transducer is used to further process each partition.

It's worth noting that all transformed outputs are subsequently interleaved. See:

```clj
=> (sequence (x/partition 2 1 identity) (range 8))
(0 1 1 2 2 3 3 4 4 5 5 6 6 7)
=> (sequence (x/by-key odd? identity) (range 8))
([false 0] [true 1] [false 2] [true 3] [false 4] [true 5] [false 6] [true 7])
```

That's why most of the time the last stage of the sub-transducer will be a 1-item transducer like `x/reduce` or `x/into`:

```clj
=> (sequence (x/partition 2 1 (x/into [])) (range 8))
([0 1] [1 2] [2 3] [3 4] [4 5] [5 6] [6 7])
=> (sequence (x/by-key odd? (x/into [])) (range 8))
([false [0 2 4 6]] [true [1 3 5 7]])
```

## Simple examples

`(group-by kf coll)` is `(into {} (x/by-key kf (x/into []) coll))`.

`(plumbing/map-vals f m)` is `(into {} (x/by-key (map f)) m)`.

My faithful `(reduce-by kf f init coll)` is now `(into {} (x/by-key kf (x/reduce f init)))`.

`(frequencies coll)` is `(into {} (x/by-key identity x/count) coll)`.

## On key-value pairs

Clojure `reduce-kv` is able to reduce key value pairs without allocating vectors or map entries: the key and value
are passed as second and third arguments of the reducing function.

Xforms allows a reducing function to advertise its support for key value pairs (3-arg arity) by implementing the `KvRfable` protocol (in practice using the `kvrf` macro).

Several xforms transducers and transducing contexts leverage `reduce-kv` and `kvrf`. When these functions are used together, pairs can be transformed without being allocated.

<table>
  <thead>
    <tr><th>fn<th>kvs in?<th>kvs out?
  </thead>
  <tbody>
    <tr><td>`for`<td>when first binding is a pair<td>when `body-expr` is a pair
    <tr><td>`reduce`<td>when is `f` is a kvrf<td>no
    <tr><td>1-arg `into`<br>(transducer)<td>when `to` is a map<td>no
    <tr><td>3-arg `into`<br>(transducing context)<td>when `from` is a map<td>when `to` is a map
    <tr><td>`by-key`<br>(as a transducer)<td>when is `kfn` and `vfn` are unspecified or `nil`<td>when `pair` is `vector` or unspecified
    <tr><td>`by-key`<br>(as a transducing context on values)<td>no<td>no
  </tbody>
<table>

```clj
;; plain old sequences
=> (let [m (zipmap (range 1e5) (range 1e5))]
     (crit/quick-bench
       (into {}
         (for [[k v] m]
           [k (inc v)]))))
Evaluation count : 12 in 6 samples of 2 calls.
             Execution time mean : 55,150081 ms
    Execution time std-deviation : 1,397185 ms

;; x/for but pairs are allocated (because of into) 
=> (let [m (zipmap (range 1e5) (range 1e5))]
     (crit/quick-bench
       (into {}
         (x/for [[k v] _]
           [k (inc v)])
         m)))
Evaluation count : 18 in 6 samples of 3 calls.
             Execution time mean : 39,119387 ms
    Execution time std-deviation : 1,456902 ms
    
;; x/for but no pairs are allocated (thanks to x/into) 
=> (let [m (zipmap (range 1e5) (range 1e5))]
     (crit/quick-bench (x/into {}
               (x/for [[k v] %]
                 [k (inc v)])
               m)))
Evaluation count : 24 in 6 samples of 4 calls.
             Execution time mean : 24,276790 ms
    Execution time std-deviation : 364,932996 µs
```

## Changelog

### 0.7.2

 * Fix transients perf issue in Clojurescript

### 0.7.1

 * Works with Clojurescript (even self-hosted).

### 0.7.0

 * Added 2-arg arity to `x/count` where it acts as a transducing context e.g. `(x/count (filter odd?) (range 10))`
 * Preserve type hints in `x/for` (and generally with `kvrf`).

### 0.6.0

 * Added `x/reductions`
 * Now if the first collection expression in `x/for` is not a placeholder then `x/for` works like `x/for` but returns an eduction and performs all iterations using reduce. 

## License

Copyright © 2015-2016 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
