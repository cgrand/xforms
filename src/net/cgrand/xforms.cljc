(ns net.cgrand.xforms
  "Extra transducers for Clojure"
  {:author "Christophe Grand"}
  #?(:cljs (:require-macros
             [net.cgrand.macrovich :as macros]
             [net.cgrand.xforms :refer [for kvrf let-complete]])
      :clj (:require [net.cgrand.macrovich :as macros]))
  (:refer-clojure :exclude [some reduce reductions into count for partition
                            str last keys vals min max drop-last take-last
                            sort sort-by time])
  (:require [#?(:clj clojure.core :cljs cljs.core) :as core]
    [net.cgrand.xforms.rfs :as rf])
  #?(:cljs (:import [goog.structs Queue])))

(macros/deftime

(defn- no-user-meta? [x]
  (= {} (dissoc (or (meta x) {}) :file :line :column :end-line :end-column)))

(defmacro unreduced->
  "Thread first while threaded value is not reduced.
   Doesn't unreduce the final value."
  ([x] x)
  ([x expr & exprs]
  `(let [x# ~x]
     (if (reduced? x#)
       x#
       (unreduced-> (-> x# ~expr) ~@exprs)))))

(defn- pair? [x] (and (vector? x) (= 2 (core/count x))))
(defn- destructuring-pair? [x]
  (and (pair? x) (not (or (keyword? x) (= '& x)))))

(defmacro for
 "Like clojure.core/for with the first expression being replaced by % (or _). Returns a transducer.
   When the first expression is not % (or _) returns an eduction."
 [[binding %or_ & seq-exprs] body-expr]
 (if-not (and (symbol? %or_) (#{"%" "_"} (name %or_)))
   `(eduction (for [~binding ~'% ~@seq-exprs] ~body-expr) ~%or_)
   (let [rf (gensym 'rf)
         acc (gensym 'acc)
         rpairs (core/partition 2 (rseq (vec seq-exprs)))
         build (fn [init]
                 (core/reduce (fn [body [expr binding]]
                                (case binding
                                  :let `(let ~expr ~body)
                                  :when `(if ~expr ~body ~acc)
                                  :while `(if ~expr ~body (reduced ~acc))
                                  (if (and (coll? expr) (not (seq? expr))
                                        (or (<= (core/count expr) 4) (:unroll (meta expr))))
                                    (let [body-rf (gensym 'body-rf)]
                                      (if (and (destructuring-pair? binding) (every? vector? expr))
                                        `(let [~body-rf (fn [~acc ~@binding] ~body)]
                                           (unreduced (unreduced-> ~acc 
                                                        ~@(map (fn [[k v]] `(~body-rf ~k ~v)) expr))))
                                        `(let [~body-rf (fn [~acc ~binding] ~body)]
                                           (unreduced (unreduced-> ~acc 
                                                        ~@(map (fn [v] `(~body-rf ~v)) expr))))))
                                    (if (destructuring-pair? binding)
                                      `(let [expr# ~expr]
                                         (if (and (map? expr#) (kvreducible? expr#))
                                           (core/reduce-kv (fn [~acc ~@binding] ~body) ~acc expr#)
                                           (core/reduce (fn [~acc ~binding] ~body) ~acc expr#)))
                                      `(core/reduce (fn [~acc ~binding] ~body) ~acc ~expr)))))
                   init rpairs))
         nested-reduceds (core/for [[expr binding] rpairs
                                    :when (not (keyword? binding))] 
                           `reduced)
         body (build `(let [acc# (~rf ~acc ~@(if (and (pair? body-expr) (no-user-meta? body-expr))
                                               body-expr
                                               [body-expr]))]
                        (if (reduced? acc#)
                          (-> acc# ~@nested-reduceds)
                          acc#)))]
     `(fn [~rf]
        (let [~rf (ensure-kvrf ~rf)]
          (kvrf
            ([] (~rf))
            ([~acc] (~rf ~acc))
            ([~acc ~binding] ~body)))))))

(defn- arity [[arglist & body :as fn-body]]
  (let [[fixargs varargs] (split-with (complement #{'&}) arglist)]
    (if (seq varargs) (zipmap (range (core/count fixargs) 4) (repeat fn-body)))
    {(core/count fixargs) fn-body}))

(defmacro kvrf [name? & fn-bodies]
  (let [name (if (symbol? name?) name? (gensym '_))
        fn-bodies (if (symbol? name?) fn-bodies (cons name? fn-bodies))
        fn-bodies (if (vector? (first fn-bodies)) (list fn-bodies) fn-bodies)
        arities (core/into {} (mapcat arity) fn-bodies)
        _ (when-not (core/some arities [2 3]) (throw (ex-info "Either arity 2 or 3 should be defined in kvrf." {:form &form})))
        fn-bodies (cond-> fn-bodies
                    (not (arities 3)) (conj (let [[[acc arg] & body] (arities 2)]
                                              (if (destructuring-pair? arg)
                                                (let [[karg varg] arg]
                                                  `([~acc ~karg ~varg] ~@body))
                                                `([~acc k# v#] (let [~arg (macros/case :clj (clojure.lang.MapEntry. k# v#) :cljs [k# v#])] ~@body)))))
                    (not (arities 2)) (conj (let [[[acc karg varg] & body] (arities 3)]
                                              `([~acc [~karg ~varg]] ~@body))))]
    `(reify
       #?@(:bb [] ;; babashka currently only supports reify with one Java interface at a time
           :default [~@(macros/case :clj '[clojure.lang.Fn])])
       KvRfable
       (some-kvrf [this#] this#)
       ~(macros/case :cljs `core/IFn :clj 'clojure.lang.IFn)
       ~@(core/for [[args & body] fn-bodies]
           (let [nohint-args (map (fn [arg] (if (:tag (meta arg)) (gensym 'arg) arg)) args)
                 rebind (mapcat (fn [arg nohint]
                                  (when-not (= arg nohint) [arg nohint])) args nohint-args)]
             `(~(macros/case :cljs `core/-invoke :clj 'invoke) [~name ~@nohint-args] ~@(if (seq rebind) [`(let [~@rebind] ~@body)] body)))))))

(defmacro ^:private let-complete [[binding volatile] & body]
  `(let [v# @~volatile]
     (when-not (identical? v# ~volatile) ; self reference as sentinel
       (vreset! ~volatile ~volatile)
       (let [~binding v#]
         ~@body))))
)

(declare into reduce multiplex by-key)

(defprotocol KvRfable "Protocol for reducing fns that accept key and val as separate arguments."
  (some-kvrf [f] "Returns a kvrf or nil"))

(macros/usetime

(defn kvreducible? [coll]
 (satisfies? #?(:clj clojure.core.protocols/IKVReduce :cljs IKVReduce) coll))

(extend-protocol KvRfable
  #?(:clj Object :cljs default) (some-kvrf [_] nil)
  #?@(:clj [nil (some-kvrf [_] nil)]))

(defn ensure-kvrf [rf]
  (or (some-kvrf rf)
    (kvrf
      ([] (rf))
      ([acc] (rf acc))
      ([acc x] (rf acc x)))))

(defn reduce
  "A transducer that reduces a collection to a 1-item collection consisting of only the reduced result.
   Unlike reduce but like transduce it does call the completing arity (1) of the reducing fn."
  ([f]
    (fn [rf]
      (let [vacc (volatile! (f))]
        (let [f (ensure-kvrf f)]
          (kvrf
            ([] (rf))
            ([acc] (let-complete [f-acc vacc]
                     (rf (unreduced (rf acc (f (unreduced f-acc)))))))
            ([acc x]
              (if (reduced? (vswap! vacc f x))
                (reduced acc)
                acc))
            ([acc k v]
              (if (reduced? (vswap! vacc f k v))
                (reduced acc)
                acc)))))))
  ([f init]
    (reduce (fn ([] init) ([acc] (f acc)) ([acc x] (f acc x))))))

(defn- into-rf [to]
  (cond
    #?(:clj (instance? clojure.lang.IEditableCollection to)
        :cljs (satisfies? IEditableCollection to))
    (if (map? to)
      (kvrf
        ([] (transient to))
        ([acc] (persistent! acc))
        ([acc x] (conj! acc x))
        ([acc k v] (assoc! acc k v)))
      (fn
        ([] (transient to))
        ([acc] (persistent! acc))
        ([acc x] (conj! acc x))))
    (map? to)
    (kvrf
      ([] to)
      ([acc] acc)
      ([acc x] (conj acc x))
      ([acc k v] (assoc acc k v)))
    :else
    (fn
      ([] to)
      ([acc] acc)
      ([acc x] (conj acc x)))))

(defn into
  "Like clojure.core/into but with a 1-arg arity returning a transducer which accumulate every input in a collection and outputs only the accumulated collection."
  ([to]
    (reduce (into-rf to)))
  ([to from]
    (into to identity from))
  ([to xform from]
    (let [rf (xform (into-rf to))]
      (if-let [rf (and (map? from) (kvreducible? from) (some-kvrf rf))]
        (rf (core/reduce-kv rf (rf) from))
        (rf (core/reduce rf (rf) from))))))

(defn- without-rf [from]
  (cond
    #?(:clj (instance? clojure.lang.IEditableCollection from)
        :cljs (satisfies? IEditableCollection from))
    (if (map? from)
      (fn
        ([] (transient from))
        ([acc] (persistent! acc))
        ([acc x] (dissoc! acc x)))
      (fn
        ([] (transient from))
        ([acc] (persistent! acc))
        ([acc x] (disj! acc x))))
    (map? from)
    (fn
      ([] from)
      ([acc] acc)
      ([acc x] (dissoc acc x)))
    :else
    (fn
      ([] from)
      ([acc] acc)
      ([acc x] (disj acc x)))))

(defn without
  "The opposite of x/into: dissociate or disjoin from the target."
  ([target]
    (reduce (without-rf target)))
  ([target keys]
    (without target identity keys))
  ([target xform keys]
    (let [rf (xform (without-rf target))]
      (if-let [rf (and (map? keys) (kvreducible? keys) (some-kvrf rf))]
        (rf (core/reduce-kv rf (rf) keys))
        (rf (core/reduce rf (rf) keys))))))

(defn minimum
  ([comparator]
    (minimum comparator nil))
  ([comparator absolute-maximum]
    (reduce (rf/minimum comparator absolute-maximum))))

(defn maximum
  ([comparator]
    (maximum comparator nil))
  ([comparator absolute-minimum]
    (reduce (rf/maximum comparator absolute-minimum))))

(def min (reduce rf/min))

(def max (reduce rf/max))

(defn str
  "When used as a value, it's an aggregating transducer that concatenates input values
   into a single output value. 
   When used as a function of two args (xform and coll) it's a transducing context that
   concatenates all values in a string."
  {:arglists '([xform coll])}
  ([rf] ((reduce rf/str) rf))
  ([xform coll]
    (transduce xform rf/str coll)))

(defn wrap
  "Transducer. Adds open as the first item, and close as the last. Optionally inserts delim between each input item."
  ([open close]
    (fn [rf]
      (let [vrf (volatile! nil)]
        (vreset! vrf
          (fn [acc x]
            (let [acc (rf acc open)]
              (vreset! vrf rf)
              (if (reduced? acc)
                acc
                (rf acc x)))))
        (fn
          ([] (rf))
          ([acc] (rf (unreduced (rf acc close))))
          ([acc x] (@vrf acc x))))))
  ([open close delim]
    (comp (interpose delim) (wrap open close))))

(defn vals [rf]
  (kvrf
    ([] (rf))
    ([acc] (rf acc))
    ([acc k v] (rf acc v))))

(defn keys [rf]
  (kvrf
    ([] (rf))
    ([acc] (rf acc))
    ([acc k v] (rf acc k))))

;; for both map entries and vectors 
(defn- key' [kv] (nth kv 0))
(defn- val' [kv] (nth kv 1))

(defn- nop-rf "The noop reducing function" ([acc] acc) ([acc _] acc) ([acc _ _] acc))

(defn- multiplexable
  "Returns a multiplexable reducing function (doesn't init or complete the uderlying rf, wraps reduced -- like preserving-reduced)"
  [rf]
  (let [rf (ensure-kvrf rf)]
    (kvrf
     ([])
     ([acc] acc) ; no init no complete rf
     ([acc x]
       (let [acc (rf acc x)]
         (if (reduced? acc)
           (reduced acc)
           acc)))
     ([acc k v]
       (let [acc (rf acc k v)]
         (if (reduced? acc)
           (reduced acc)
           acc))))))

(defn by-key
  "Returns a transducer which partitions items according to kfn.
   It applies the transform specified by xform to each partition.
   Partitions contain the \"value part\" (as returned by vfn) of each item.
   The resulting transformed items are wrapped back into a \"pair\" using the pair function.
   Default values for kfn, vfn and pair are first, second (or identity if kfn is specified) and vector."
  ([xform] (by-key nil nil vector xform))
  ([kfn xform] (by-key kfn identity vector xform))
  ([kfn vfn xform] (by-key kfn vfn vector xform))
  ([kfn vfn pair xform]
    (let [pair (if (identical? vector pair) ::default pair)]
      (fn [rf]
        (let [mrf (multiplexable rf)
              make-rf (cond
                        (nil? pair) (constantly mrf)
                        (= ::default pair)
                        (fn [k] (fn ([acc] acc) ([acc v] (mrf acc k v))))
                        :else (fn [k] (fn ([acc] acc) ([acc v] (mrf acc (pair k v))))))
              m (volatile! (transient {}))]
          (if (and (nil? kfn) (nil? vfn))
            (kvrf self
              ([] (rf))
              ([acc] (let-complete [m m] (rf (core/reduce (fn [acc krf] (krf acc)) acc (core/vals (persistent! m))))))
              ([acc k v]
                (let [krf (or (get @m k) (doto (xform (make-rf k)) (->> (vswap! m assoc! k))))
                      acc (krf acc v)]
                   (if (reduced? acc)
                     (if (reduced? @acc)
                       (do
                         (vreset! m (transient {})) ; no need to run completions
                         @acc) ; downstream is done, propagate
                       (do
                         (vswap! m assoc! k nop-rf)
                         (krf @acc))) ; TODO think again
                     acc))))
            (let [kfn (or kfn key')
                  vfn (or vfn val')]
              (kvrf self
                ([] (rf))
                ([acc] (let-complete [m m] (rf (core/reduce (fn [acc krf] (krf acc)) acc (core/vals (persistent! m))))))
                ([acc x]
                  (let [k (kfn x)
                        krf (or (get @m k) (doto (xform (make-rf k)) (->> (vswap! m assoc! k))))
                        acc (krf acc (vfn x))]
                    (if (reduced? acc)
                      (if (reduced? @acc)
                        (do
                          (vreset! m (transient {})) ; no need to run completions
                          @acc) ; downstream is done, propagate
                        (do
                          (vswap! m assoc! k nop-rf)
                          (krf @acc)))
                      acc)))))))))))

(defn into-by-key
  "A shorthand for the common case (comp (x/by-key ...) (x/into coll))."
  [coll & by-key-args]
  (comp (apply by-key by-key-args) (into coll)))

(macros/replace
  [#?(:cljs {(java.util.ArrayDeque. n) (Queue.)
             .add .enqueue
             .poll .dequeue
             .size .getCount})
   #?(:clj {(.getValues dq) dq})]
  
  (defn partition
    "Returns a partitioning transducer. Each partition is independently transformed using the xform transducer."
    ([n]
      (partition n n (into [])))
    ([n step-or-xform]
      (if (fn? step-or-xform)
        (partition n n step-or-xform)
        (partition n step-or-xform (into []))))
    ([^long n step pad-or-xform]
      (if (fn? pad-or-xform)
        (let [xform pad-or-xform]
          (fn [rf]
            (let [mxrf (multiplexable rf)
                  dq (java.util.ArrayDeque. n)
                  barrier (volatile! n)
                  xform (comp (map #(if (identical? dq %) nil %)) xform)]
              (fn
                ([] (rf))
                ([acc] (.clear dq) (rf acc))
                ([acc x]
                  (let [b (vswap! barrier dec)]
                    (when (< b n) (.add dq (if (nil? x) dq x)))
                    (if (zero? b)
                      ; this transduce may return a reduced because of mxrf wrapping reduceds coming from rf
                      (let [acc (transduce xform mxrf acc (.getValues dq))]
                        (dotimes [_ (core/min n step)] (.poll dq))
                        (vswap! barrier + step)
                        acc)
                      acc)))))))
        (partition n step pad-or-xform (into []))))
    ([^long n step pad xform]
      (fn [rf]
        (let [mxrf (multiplexable rf)
              dq (java.util.ArrayDeque. n)
              barrier (volatile! n)
              xform (comp (map #(if (identical? dq %) nil %)) xform)]
          (fn
            ([] (rf))
            ([acc] (if (< @barrier n)
                     (let [xform (comp cat (take n) xform)
                           ; don't use mxrf for completion: we want completion and don't want reduced-wrapping 
                           acc (transduce xform rf acc [(.getValues dq) pad])]
                       (vreset! barrier n)
                       (.clear dq)
                       acc)
                     (rf acc)))
            ([acc x]
              (let [b (vswap! barrier dec)]
                (when (< b n) (.add dq (if (nil? x) dq x)))
                (if (zero? b)
                  ; this transduce may return a reduced because of mxrf wrapping reduceds coming from rf
                  (let [acc (core/transduce xform mxrf acc (.getValues dq))]
                    (dotimes [_ (core/min n step)] (.poll dq))
                    (vswap! barrier + step)
                    acc)
                  acc))))))))
  
  #_(defn zip [xform1 xform2]
     (fn [rf]
       (let )))
  
  (defn take-last [^long n]
    (fn [rf]
      (let [dq (java.util.ArrayDeque. n)]
        (fn
          ([] (rf))
          ([acc] (transduce (map #(if (identical? dq %) nil %)) rf acc (.getValues dq)))
          ([acc x]
            (.add dq (if (nil? x) dq x))
            (when (< n (.size dq)) (.poll dq))
            acc)))))
  
  (defn drop-last 
    ([] (drop-last 1))
    ([^long n]
      (fn [rf]
        (let [dq (java.util.ArrayDeque. n)
              xform (map #(if (identical? dq %) nil %))
              rf (xform rf)]
          (fn
            ([] (rf))
            ([acc] (rf acc))
            ([acc x]
              (.add dq (if (nil? x) dq x))
              (if (< n (.size dq)) 
                (rf acc (.poll dq))
                acc)))))))
  
  )

(defn sort
  ([] (sort compare))
  ([cmp]
    (fn [rf]
      (let [buf #?(:clj (java.util.ArrayList.) :cljs #js [])]
        (fn
          ([] (rf))
          ([acc] (rf (core/reduce rf acc (doto buf #?(:clj (java.util.Collections/sort cmp) :cljs (.sort cmp))))))
          ([acc x] (#?(:clj .add :cljs .push) buf x) acc))))))

(defn sort-by
  ([kfn] (sort-by kfn compare))
  ([kfn cmp]
    (sort (fn [a b]
            #?(:clj (.compare ^java.util.Comparator cmp (kfn a) (kfn b))
               :cljs (cmp (kfn a) (kfn b)))))))

(defn reductions
  "Transducer version of reductions. There's a difference in behavior when init is not provided: (f) is used.
   So x/reductions works like x/reduce or transduce, not like reduce and reductions when no init and 1-item input."
  ([f] (reductions f (f)))
  ([f init]
    (fn [rf]
      (let [prev (volatile! nil)]
        (vreset! prev prev) ; cheap sentinel to detect the first call, this is done to avoid having a 1-item delay
        (fn
          ([] (rf)) ; no you can't emit init there since there's no guarantee that this arity is going to be called
          ([acc] (if (identical? @prev prev)
                   (rf (unreduced (rf acc init)))
                   (rf acc)))
          ([acc x]
            (if (identical? @prev prev)
              (let [acc (rf acc (vreset! prev init))]
                (if (reduced? acc)
                  acc
                  (recur acc x)))
              (let [curr (vswap! prev f x)]
                (if (reduced? curr)
                  (ensure-reduced (rf acc @curr))
                  (rf acc curr))))))))))

(def avg (reduce rf/avg))
(def sd (reduce rf/sd))

(defn window
  "Returns a transducer which computes an accumulator over the last n items
   using two functions: f and its inverse invf.

   The accumulator is initialized with (f).
   It is updated to (f (invf acc out) in) where \"acc\" is the current value,
   \"in\" the new item entering the window, \"out\" the item exiting the window.
   The value passed to the dowstream reducing function is (f acc) enabling acc to be
   mutable and 1-arity f to project its state to a value.

   If you don't want to see the accumulator until the window is full then you need to
   use (drop (dec n)) to remove them.

   If you don't have an inverse function, consider using partition and reduce: 
   (x/partition 4 (x/reduce rf))"
  [n f invf]
  (fn [rf]
    (let [ring (object-array n)
          vi (volatile! (- n))
          vwacc (volatile! (f))]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc x]
          (let [i @vi
                wacc @vwacc] ; window accumulator
            (if (neg? i) ; not full yet
              (do
                (aset ring (+ n i) x)
                (vreset! vi (inc i))
                (rf acc (f (vreset! vwacc (f wacc x)))))
              (let [x' (aget ring i)]
                (aset ring i x)
                (vreset! vi (let [i (inc i)] (if (= n i) 0 i)))
                (rf acc (f (vreset! vwacc (f (invf wacc x') x))))))))))))

#?(:clj
    (defn iterator
      "Iterator transducing context, returns an iterator on the transformed data.
       Equivalent to (.iterator (eduction xform (iterator-seq src-iterator))) except there's is no buffering on values (as in iterator-seq),
       This buffering may cause problems when mutable objects are returned by the src-iterator."
      ^java.util.Iterator [xform ^java.util.Iterator src-iterator]
      (let [NULL (Object.)
            dq (java.util.ArrayDeque. 32)
            rf (xform (fn ([acc] acc) ([acc x] (.push dq (if (some? x) x NULL)) acc)))
            vopen (volatile! true)
            ensure-next #(or (some? (.peek dq))
                           (and @vopen
                             (if (.hasNext src-iterator)
                               (let [acc (rf nil (.next src-iterator))]
                                 (when (reduced? acc) 
                                   (rf nil)
                                   (vreset! vopen false))
                                 (recur))
                               (do 
                                 (rf nil)
                                 (vreset! vopen false)
                                 (recur)))))]
        (reify java.util.Iterator
          (hasNext [_]
            (ensure-next))
          (next [_]
            (if (ensure-next)
              (let [x (.poll dq)]
                (if (identical? NULL x) nil x))
              (throw (java.util.NoSuchElementException.))))))))

#?(:clj
    (defn window-by-time
      "ALPHA
   Returns a transducer which computes a windowed accumulator over chronologically sorted items.
   
   timef is a function from one item to its scaled timestamp (as a double). The window length is always 1.0
   so timef must normalize timestamps. For example if timestamps are in seconds (and under the :ts key),
   to get a 1-hour window you have to use (fn [x] (/ (:ts x) 3600.0)) as timef.

   n is the integral number of steps by which the window slides. With a 1-hour window, 4 means that the window slides every 15 minutes.

   f and invf work like in #'window."
      ([timef n f]
        (window-by-time timef n 
          (fn 
            ([] clojure.lang.PersistentQueue/EMPTY)
            ([q] (f (core/reduce f (f) q)))
            ([q x] (conj q x)))
          (fn [q _] (pop q))))
      ([timef n f invf]
        (let [timef (fn [x] (long (Math/floor (* n (timef x)))))]
          (fn [rf]
           (let [dq (java.util.ArrayDeque.)
                 vwacc (volatile! (f))
                 flush!
                 (fn [acc ^long from-ts ^long to-ts]
                   (loop [ts from-ts acc acc wacc @vwacc]
                     (let [x (.peekFirst dq)]
                       (cond
                         (= ts (timef x))
                         (do
                           (.pollFirst dq)
                           (recur ts acc (invf wacc x)))
                         (= ts to-ts)
                         (do
                           (vreset! vwacc wacc)
                           acc)
                         :else
                         (let [acc (rf acc (f wacc))]
                           (if (reduced? acc)
                             (do
                               (vreset! vwacc wacc)
                               acc)
                             (recur (inc ts) acc wacc)))))))]
             (fn
               ([] (rf))
               ([acc]
                 (let [acc (if-not (.isEmpty dq)
                             (unreduced (rf acc (f @vwacc)))
                             acc)]
                   (rf acc)))
               ([acc x]
                 (let [limit (- (timef x) n)
                       prev-limit (if-some [prev-x (.peekLast dq)]
                                    (- (timef prev-x) n)
                                    limit)
                       _ (.addLast dq x) ; so dq is never empty for flush!
                       acc (flush! acc prev-limit limit)]
                   (when-not (reduced? acc)
                     (vswap! vwacc f x))
                   acc)))))))))

(defn count
  "Count the number of items. Either used directly as a transducer or invoked with two args
   as a transducing context."
  ([rf]
    (let [n #?(:clj (java.util.concurrent.atomic.AtomicLong.) :cljs (atom 0))]
      (fn
        ([] (rf))
        ([acc] (rf (unreduced (rf acc #?(:clj (.get n) :cljs @n)))))
        ([acc _] #?(:clj (.incrementAndGet n) :cljs (swap! n inc)) acc))))
  ([xform coll]
    (transduce (comp xform count) rf/last coll)))

(defn multiplex
  "Returns a transducer that runs several transducers (sepcified by xforms) in parallel.
   If xforms is a map, values of the map are transducers and keys are used to tag each
   transducer output:
   => (into [] (x/multiplex [(map inc) (map dec)]) (range 3))
   [1 -1 2 0 3 1] ; no map, no tag
   => (into [] (x/multiplex {:up (map inc) :down (map dec)}) (range 3))
   [[:up 1] [:down -1] [:up 2] [:down 0] [:up 3] [:down 1]]"
  [xforms]
  (fn [rf]
    (let [mrf (multiplexable (ensure-kvrf rf))
          rfs (volatile! (if (map? xforms)
                           (into {} (for [[k xform] %
                                          :let [xform (comp xform (for [x %] [k x]))]]
                                      [k (xform mrf)])
                             xforms)
                         (into #{} (map #(% mrf)) xforms)))
          invoke-rfs (if (map? xforms)
                       (fn [acc invoke]
                         (reduce-kv
                           (fn [acc tag rf]
                             (let [acc (invoke rf acc)]
                               (if (reduced? acc)
                                 (if (reduced? @acc)
                                   (do
                                     (vreset! rfs nil)
                                     acc) ; downstream is done, propagate
                                   (do (vswap! rfs dissoc tag) (rf @acc)))
                                 acc)))
                           acc @rfs))
                       (fn [acc invoke]
                         (core/reduce
                           (fn [acc rf]
                             (let [acc (invoke rf acc)]
                               (if (reduced? acc)
                                 (if (reduced? @acc)
                                   (do
                                     (vreset! rfs nil)
                                     acc) ; downstream is done, propagate
                                   (do (vswap! rfs disj rf) (rf @acc)))
                                 acc)))
                           acc @rfs)))]
      (kvrf
        ([] (rf))
        ([acc] (rf (invoke-rfs acc #(%1 %2))))
        ([acc x]
          (let [acc (invoke-rfs acc #(%1 %2 x))]
            (if (zero? (core/count @rfs))
              (ensure-reduced acc)
              acc)))
        ([acc k v]
          (let [acc (invoke-rfs acc #(%1 %2 k v))]
            (if (zero? (core/count @rfs))
              (ensure-reduced acc)
              acc)))))))

(def last (reduce rf/last))

(defn some
  "Process coll through the specified xform and returns the first local true value."
  [xform coll]
  (transduce xform rf/some nil coll))

(defn transjuxt
  "Performs several transductions over coll at once. xforms-map can be a map or a sequential collection.
   When xforms-map is a map, returns a map with the same keyset as xforms-map.
   When xforms-map is a sequential collection returns a vector of same length as xforms-map.
   Returns a transducer when coll is omitted."
  ([xforms-map]
    (let [collect-xform (if (map? xforms-map) 
                          (into {})
                          (reduce (kvrf
                                    ([] (core/reduce (fn [v _] (conj! v nil))
                                          (transient []) (range (core/count xforms-map))))
                                    ([v] (persistent! v))
                                    ([v i x] (assoc! v i x)))))
          xforms-map (if (map? xforms-map) xforms-map (zipmap (range) xforms-map))]
      (comp
        (multiplex (into {} (by-key (map #(comp % (take 1)))) xforms-map))
        collect-xform)))
  ([xforms-map coll]
    (transduce (transjuxt xforms-map) rf/last coll)))

(macros/replace
  [#?(:cljs {(java.util.concurrent.atomic.AtomicLong.) (atom 0)
             (System/nanoTime) (system-time)
             (.addAndGet at (- t (System/nanoTime))) (swap! at + (- t (system-time)))
             (.addAndGet at (- (System/nanoTime) t)) (swap! at + (- (system-time) t))
             .size .getCount})]
  
  (defn time
    "Measures the time spent in this transformation and prints the measured time.
   tag-or-f may be either a function of 1 argument (measured time in ms) in which case
   this function will be called instead of printing, or tag-or-f will be print before the measured time."
    ([xform] (time "Elapsed time" xform))
    ([tag-or-f xform]
      (let [pt (if (fn? tag-or-f)
                 tag-or-f
                 #(println (core/str tag-or-f ": " % " msecs")))]
        (fn [rf]
          (let [at (java.util.concurrent.atomic.AtomicLong.)
                rf
                (fn
                  ([] (rf))
                  ([acc] (let [t (System/nanoTime)
                               r (rf acc)]
                           (.addAndGet at (- t (System/nanoTime)))
                           r))
                  ([acc x] 
                    (let [t (System/nanoTime) 
                          r (rf acc x)]
                      (.addAndGet at (- t (System/nanoTime)))
                      r)))
                rf (xform rf)]
            (fn 
              ([] (rf))
              ([acc]
                (let [t (System/nanoTime)
                      r (rf acc)
                      total (.addAndGet at (- (System/nanoTime) t))]
                  (pt #?(:clj (* total 1e-6) :cljs total))
                  r))
              ([acc x]
                (let [t (System/nanoTime)
                      r (rf acc x)]
                  (.addAndGet at (- (System/nanoTime) t))
                  r)))))))))

#_(defn rollup
   "Roll-up input data along the provided dimensions (which are functions of one input item),
   Values of interest are extracted from items using the valfn function and are then summarized
   by summary-fn (a reducing function over values returned by valfn or summaries).
   Each level of rollup is a map with two keys: :summary and :details."
   ([dimensions valfn summary-fn]
     (let [[dim & dims] (reverse dimensions)]
       (core/reduce
         (fn [xform dim]
           (comp
             (by-key dim xform)
             (transjuxt
               {:detail (into {})
                :summary (comp vals (map :summary) (reduce summary-fn))})))
         (comp (by-key dim (map valfn))
           (transjuxt
             {:detail (into {})
              :summary (comp vals (reduce summary-fn))}))
         dims)))
   ([dimensions valfn summary-fn coll]
     (into {} (rollup dimensions valfn summary-fn) coll)))
)
