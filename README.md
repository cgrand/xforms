# xforms

More transducers and reduciing functions for Clojure!

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
  (into {} (x/by-key kfn (x/reduce (completing conj! persistent!))) coll))

=> (quick-bench (group-by odd? (range 256)))
             Execution time mean : 29,356531 µs
=> (quick-bench (my-group-by odd? (range 256)))
             Execution time mean : 20,604297 µs
```



## License

Copyright © 2015 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
