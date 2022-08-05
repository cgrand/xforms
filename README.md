# xforms

More transducers and reducing functions for Clojure(script)!

[![Build Status](https://travis-ci.org/cgrand/xforms.png?branch=master)](https://travis-ci.org/cgrand/xforms)

*Transducers* can be classified in three groups: regular ones, higher-order ones
(which accept other transducers as arguments) and aggregators (transducers which emit only 1 item out no matter how many went in).
Aggregators generally only make sense in the context of a higher-order transducer. 

In `net.cgrand.xforms`:

 * regular ones: `partition` (1 arg), `reductions`, `for`, `take-last`, `drop-last`, `sort`, `sort-by`, `wrap`, `window` and `window-by-time`
 * higher-order ones: `by-key`, `into-by-key`, `multiplex`, `transjuxt`, `partition` (2+ args), `time`
 * aggregators: `reduce`, `into`, `without`, `transjuxt`, `last`, `count`, `avg`, `sd`, `min`, `minimum`, `max`, `maximum`, `str`

In `net.cgrand.xforms.io`:
 * `sh` to use any process as a reducible collection (of stdout lines) or as a transducers (input as stdin lines, stdout lines as output).

 
*Reducing functions*

 * in `net.cgrand.xforms.rfs`: `min`, `minimum`, `max`, `maximum`, `str`, `str!`, `avg`, `sd`, `last` and `some`.
 * in `net.cgrand.xforms.io`: `line-out` and `edn-out`.

(in `net.cgrand.xforms`)

*Transducing contexts*:

 * in `net.cgrand.xforms`: `transjuxt` (for performing several transductions in a single pass), `iterator` (clojure only), `into`, `without`, `count`, `str` (2 args) and `some`.
 * in `net.cgrand.xforms.io`: `line-out` (3+ args) and `edn-out` (3+ args).
 * in `net.cgrand.xforms.nodejs.stream`: `transformer`.

*Reducible views* (in `net.cgrand.xforms.io`): `lines-in` and `edn-in`.

**Note:** it should always be safe to update to the latest xforms version; short of bugfixes, breaking changes are avoided.

## Add as a dependency

For specific coordinates see the [latest release](/releases/latest).

## Usage

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
     (x/window 4 rf/avg #(rf/avg %1 %2 -1))
     nums)
(11 19/2 17 77/4 18 37/2 79/4 77/4)

;; min of last 3 items
=> (sequence
        (x/window 3
          (fn
            ([] (sorted-map))
            ([m] (key (first m)))
            ([m x] (update m x (fnil inc 0))))
          (fn [m x]
            (let [n (dec (m x))]
              (if (zero? n)
                (dissoc m x)
                (assoc m x (dec n))))))
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

That's why most of the time the last stage of the sub-transducer will be an aggregator like `x/reduce` or `x/into`:

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

### 0.19.3

 * Add `deps.edn` to enable usage as a [git library](https://clojure.org/guides/deps_and_cli#_using_git_libraries)
 * Bump `macrovich` to make Clojure and ClojureScript provided dependencies #34
 * Fix reflection warnings in `xforms.io` #35 #36
 * Add compatibility with [babashka](https://github.com/babashka/babashka) #42
 * Fix `x/destructuring-pair?` #44 #45
 * Fix `x/into` performance hit with small maps #46 #47
 * Fix reflection and shadowing warnings in tests

### 0.19.2

 * Fix infinity symbol causing issues with ClojureScript #31

### 0.19.0

`time` allows to measure time spent in one transducer (excluding time spent downstream).

```clj
=> (time ; good old Clojure time
     (count (into [] (comp
                     (x/time "mapinc" (map inc))
                     (x/time "filterodd" (filter odd?))) (range 1e6))))
filterodd: 61.771738 msecs
mapinc: 143.895317 msecs
"Elapsed time: 438.34291 msecs"
500000
```

First argument can be a function that gets passed the time (in ms),
this allows for example to log time instead of printing it.

### 0.9.5

 * Short (up to 4) literal collections (or literal collections with `:unroll` metadata) in collection positions in `x/for` are unrolled.
   This means that the collection is not allocated.
   If it's a collection of pairs (e.g. maps), pairs themselves won't be allocated.

### 0.9.4

 * Add `x/into-by-key` short hand

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

## Troubleshooting xforms in a Clojurescript dev environment

If you use xforms with Clojurescript and the Emacs editor to start your figwheel REPL be sure to include the `cider.nrepl/cider-middleware` to your figwheel's nrepl-middleware. 
```
  :figwheel {...
             :nrepl-middleware [cider.nrepl/cider-middleware;;<= that middleware
                                refactor-nrepl.middleware/wrap-refactor
                                cemerick.piggieback/wrap-cljs-repl]
             ...}
```
Otherwise a strange interaction occurs and every results from your REPL evaluation would be returned as a String. Eg.:
```
cljs.user> 1
"1"
cljs.user>
```
instead of:
```
cljs.user> 1
1
cljs.user>
```


## License

Copyright © 2015-2016 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
