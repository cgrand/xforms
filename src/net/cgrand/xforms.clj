(ns net.cgrand.xforms
  "Extra transducers for Clojure"
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [reduce into count for partition str juxt first])
  (:require [clojure.core :as clj]))

(defmacro for
  "Like clojure.core/for with the first expression being replaced by % (or _). Returns a transducer."
  [[binding %or_ & seq-exprs] body-expr]
  (assert (and (symbol? %or_) (#{"%" "_"} (name %or_)))
    "The second element of the comprehension vector must be % or _.")
  (let [rf (gensym 'rf)
        acc (gensym 'acc)
        pair? #(and (vector? %) (= 2 (clj/count %)))
        build (fn [init]
                (clj/reduce (fn [body [expr binding]]
                      (case binding
                        :let `(let ~expr ~body)
                        :when `(if ~expr ~body ~acc)
                        :while `(if ~expr ~body (reduced ~acc))
                        `(clj/reduce (fn [~acc ~binding] ~body) ~acc ~expr)))
                  init (clj/partition 2 (rseq (vec seq-exprs)))))
        body (build `(~rf ~acc ~body-expr))
        kvbody (when (pair? body-expr) (build `(~rf ~acc ~@body-expr)))
        fnsym (if (and (pair? binding) (not (some keyword? binding)) (not (some #{'&} (filter symbol? binding)))) `kvrf `fn)]
    (if kvbody
      `(fn [~rf]
         (if-some [~rf (some-kvrf ~rf)]
           (~fnsym
             ([] (~rf))
             ([~acc] (~rf ~acc))
             ([~acc ~binding] ~kvbody)
             ~@(when (= fnsym `kvrf) [`([~acc ~@binding] ~kvbody)]))
           (~fnsym
             ([] (~rf))
             ([~acc] (~rf ~acc))
             ([~acc ~binding] ~body)
             ~@(when (= fnsym `kvrf) [`([~acc ~@binding] ~body)]))))
      `(fn [~rf]
         (~fnsym
           ([] (~rf))
           ([~acc] (~rf ~acc))
           ([~acc ~binding] ~body)
           ~@(when (= fnsym `kvrf) [`([~acc ~@binding] ~body)]))))))

(defprotocol KvRfable "Protocol for reducing fns that takes key and val as separate arguments."
  (some-kvrf [f] "Returns a kvrf or nil"))

(extend-protocol KvRfable
  Object (some-kvrf [_] nil)
  nil (some-kvrf [_] nil))

(defmacro kvrf [name? & fn-bodies]
  (let [name (if (symbol? name?) name? (gensym '_))
        fn-bodies (if (symbol? name?) fn-bodies (cons name? fn-bodies))
        fn-bodies (if (vector? (clj/first fn-bodies)) (list fn-bodies) fn-bodies)]
    `(reify
       clojure.lang.Fn
       KvRfable
       (some-kvrf [this#] this#)
       clojure.lang.IFn
       ~@(clj/for [[args & body] fn-bodies]
           `(invoke [~name ~@args] ~@body)))))

(defn ensure-kvrf [rf]
  (or (some-kvrf rf)
    (kvrf
      ([] (rf))
      ([acc] (rf acc))
      ([acc x] (rf acc x))
      ([acc k v] (rf acc (clojure.lang.MapEntry. k v))))))

(defn reduce
  "A transducer that reduces a collection to a 1-item collection consisting of only the reduced result.
   Unlike reduce but like transduce it does call the completing arity (1) of the reducing fn."
  ([f]
    (fn [rf]
      (let [vacc (volatile! (f))]
        (if-some [f (some-kvrf f)]
          (kvrf
            ([] (rf))
            ([acc] (rf (unreduced (rf acc (f (unreduced @vacc))))))
            ([acc x]
              (if (reduced? (vswap! vacc f x))
                (reduced acc)
                acc))
            ([acc k v]
              (if (reduced? (vswap! vacc f k v))
                (reduced acc)
                acc)))
          (fn
            ([] (rf))
            ([acc] (rf (unreduced (rf acc (f (unreduced @vacc))))))
            ([acc x]
              (if (reduced? (vswap! vacc f x))
                (reduced acc)
                acc)))))))
  ([f init]
    (reduce (fn ([] init) ([acc] (f acc)) ([acc x] (f acc x))))))

(defn- into-rf [to]
  (cond
    (instance? clojure.lang.IEditableCollection to)
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
      (if-let [rf (and (map? from) (satisfies? clojure.core.protocols/IKVReduce from) (some-kvrf rf))]
        (rf (clj/reduce-kv rf (rf) from))
        (rf (clj/reduce rf (rf) from))))))

(defmacro ^:private or-instance? [class x y]
  (let [xsym (gensym 'x_)]
    `(let [~xsym ~x]
       (if (instance? ~class ~xsym) ~(with-meta xsym {:tag class}) ~y))))

(defn str!
  "Like xforms/str but returns a StringBuilder."
  ([] (StringBuilder.))
  ([sb] (or-instance? StringBuilder sb (StringBuilder. (clj/str sb)))) ; the instance? checks are for compatibility with str in case of seeded reduce/transduce.
  ([sb x] (.append (or-instance? StringBuilder sb (StringBuilder. (clj/str sb))) x)))

(def str
  "Reducing function to build strings in linear time. Acts as replacement for clojure.core/str in a reduce/transduce call."
  (completing str! clj/str))

;; for both map entries and vectors 
(defn- key' [kv] (nth kv 0))
(defn- val' [kv] (nth kv 1))

(defn- noprf "The noop reducing function" ([acc] acc) ([acc _] acc) ([acc _ _] acc))

(defn- multiplexable
  "Creates a multiplexable reducing function (doesn't init or complete the uderlying rf)."
  [rf]
  (if-some [rf (some-kvrf rf)]
    (kvrf ([]) ([acc] acc) ([acc x] (rf acc x)) ([acc k v] (rf acc k v)))
    (fn ([]) ([acc] acc) ([acc x] (rf acc x))))) ; no init no complete rf

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
        (let [make-rf (cond
                        (nil? pair) (constantly (multiplexable rf))
                        (= ::default pair)
                        (let [rf (ensure-kvrf rf)]
                          (fn [k] (fn ([acc] acc) ([acc v] (rf acc k v)))))
                        :else (fn [k] (fn ([acc] acc) ([acc v] (rf acc (pair k v))))))
              m (volatile! (transient {}))]
          (if (and (nil? kfn) (nil? vfn))
            (kvrf self
              ([] (rf))
              ([acc] (rf (clj/reduce (fn [acc krf] (krf acc)) acc (vals (persistent! @m)))))
              ([acc x]
                (self acc (key' x) (val' x)))
              ([acc k v]
                (let [krf (or (get @m k) (doto (xform (make-rf k)) (->> (vswap! m assoc! k))))
                      acc (krf acc v)]
                   (when (reduced? acc) ; complete?
                     (vswap! m assoc! k noprf))
                   (unreduced acc))))
            (let [kfn (or kfn key')
                  vfn (or vfn val')]
              (fn
                ([] (rf))
                ([acc] (rf (clj/reduce (fn [acc krf] (krf acc)) acc (vals (persistent! @m)))))
                ([acc x]
                  (let [k (kfn x)
                        krf (or (get @m k) (doto (xform (make-rf k)) (->> (vswap! m assoc! k))))
                        acc (krf acc (vfn x))]
                    (when (reduced? acc) ; complete?
                      (vswap! m assoc! k noprf))
                    (unreduced acc)))))))))))

(defn- spawn
  "Every n items, spawns a new pipeline."
  [n xform]
  (fn [rf]
    (let [mxrf (multiplexable rf)
          vrfs (volatile! [])
          m (volatile! 0)]
      (fn
        ([] (rf))
        ([acc]
          (rf (clj/reduce #(%2 %1) acc @vrfs)))
        ([acc x]
          (let [rfs @vrfs
                step! (fn [acc rf]
                        (let [acc (rf acc x)]
                          (if (reduced? acc)
                            (rf (unreduced acc))
                            (do
                              (vswap! vrfs conj! rf)
                              acc))))]
            (vreset! vrfs (transient []))
            (let [acc (clj/reduce step! acc rfs)
                  acc (if (neg? (vswap! m dec))
                        (do
                          (vswap! m + n)
                          (step! acc (xform mxrf)))
                        acc)]
              (vswap! vrfs persistent!)
              acc)))))))

(defn pad [n padding-coll]
  (fn [rf]
    (let [n (volatile! n)]
      (fn
        ([] (rf))
        ([acc]
          (rf (clj/reduce ((take @n) rf) acc padding-coll)))
        ([acc x]
          (vswap! n dec)
          (rf acc x))))))

(defn partition
  "Returns a partitioning transducer. Each partition is independently transformed using the xform transducer.
   Unlike clojure.core/partition the last partitions may be incomplete.
   Partitions can be padded using #'pad."
  ; being strict towards partition size implies buffering and avoiding unecessary buffering is part of this
  ; library goal. So partition won't support it. However a buffer transducer may be an option.
  ([n]
    (partition n n (into [])))
  ([n xform]
    (partition n n xform))
  ([n step xform]
    (spawn step (comp (take n) xform))))

(defn avg
  "Reducing fn to compute the arithmetic mean."
  ([] (transient [0 0]))
  ([[n sum]] (/ sum n))
  ([acc x] (avg acc x 1))
  ([[n sum :as acc] x w]
    (-> acc (assoc! 0 (+ n w)) (assoc! 1 (+ sum (* w x))))))

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

(defn window-by-time
  "Returns a transducer which computes a windowed accumulator over chronologically sorted items.
   
   timef is a function from one item to its scaled timestamp (as a double). The window length is always 1.0
   so timef must normalize timestamps. For example if timestamps are in seconds (and under the :ts key),
   to get a 1-hour window you have to use (fn [x] (/ (:ts x) 3600.0)) as timef.

   n is the integral number of steps by which the window slides. With a 1-hour window, 4 means that the window slides every 15 minutes.

   f and invf work like in #'window."
  ([timef n f]
    (window-by-time timef n 
      (fn 
        ([] clojure.lang.PersistentQueue/EMPTY)
        ([q] (f (clj/reduce f (f) q)))
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
               acc))))))))

(defn count ([] 0) ([n] n) ([n _] (inc n)))

(defn juxt
  "Returns a reducing fn which compute all rfns at once and whose final return
   value is a vector of the final return values of each rfns."
  [& rfns]
  (if (some some-kvrf rfns)
    (let [rfns (mapv ensure-kvrf rfns)]
      (kvrf
        ([] (mapv #(vector % (volatile! (%))) rfns))
        ([acc] (mapv (fn [[rf vacc]] (rf (unreduced @vacc))) acc))
        ([acc x]
          (let [some-unreduced (clj/reduce (fn [some-unreduced [rf vacc]] 
                                             (when-not (reduced? @vacc) (vswap! vacc rf x) true))
                                 false acc)]
            (if some-unreduced acc (reduced acc))))
        ([acc k v]
          (let [some-unreduced (clj/reduce (fn [some-unreduced [rf vacc]] 
                                             (when-not (reduced? @vacc) (vswap! vacc rf k v) true))
                                 false acc)]
            (if some-unreduced acc (reduced acc))))))
    (let [rfns (vec rfns)]
      (fn
        ([] (mapv #(vector % (volatile! (%))) rfns))
        ([acc] (mapv (fn [[rf vacc]] (rf (unreduced @vacc))) acc))
        ([acc x]
          (let [some-unreduced (clj/reduce (fn [some-unreduced [rf vacc]] 
                                             (when-not (reduced? @vacc) (vswap! vacc rf x) true))
                                 false acc)]
            (if some-unreduced acc (reduced acc))))))))

(defn multiplex
  [xforms-map]
  (fn [rf]
    (let [mrf (multiplexable (ensure-kvrf rf))
          rfs-map (volatile! (into {} (for [[k xform] %
                                            :let [xform (comp xform (for [x %] [k x]))]]
                                        [k (xform mrf)])
                               xforms-map))]
      (kvrf
        ([] (rf))
        ([acc] (rf acc))
        ([acc x]
          (let [acc (reduce-kv
                      (fn [acc tag rf]
                        (let [acc (rf acc x)]
                          (if (reduced? acc)
                            (do (vswap! rfs-map dissoc tag) (rf @acc))
                            acc)))
                      acc @rfs-map)]
            (if (zero? (clj/count @rfs-map))
              (reduced acc)
              acc)))
        ([acc k v]
          (let [acc (reduce-kv
                      (fn [acc tag rf]
                        (let [acc (rf acc k v)]
                          (if (reduced? acc)
                            (do (vswap! rfs-map dissoc tag) (rf @acc))
                            acc)))
                      acc @rfs-map)]
            (if (zero? (clj/count @rfs-map))
              (reduced acc)
              acc)))))))

(defn juxt-map
  [& key-rfns]
  (let [f (apply juxt (take-nth 2 (next key-rfns)))
        keys (vec (take-nth 2 key-rfns))]
    (if-some [f (some-kvrf f)]
      (kvrf
        ([] (f))
        ([acc] (zipmap keys (f acc)))
        ([acc x] (f acc x))
        ([acc k v] (f acc k v)))
      (fn
        ([] (f))
        ([acc] (zipmap keys (f acc)))
        ([acc x] (f acc x))))))

(defn first
  "Reducing function that returns the first value or nil if none."
  ([] nil)
  ([x] x)
  ([_ x] (reduced x)))

(defn transjuxt
  "Performs several transductions over coll at once. xforms-map can be a map or a sequential collection.
   When xforms-map is a map, returns a map with the same keyset as xforms-map.
   When xforms-map is a sequential collection returns a vector of same length as xforms-map.
   Returns a transducer when coll is omitted."
  ([xforms-map]
    (let [[f args] (if (map? xforms-map)
                     [juxt-map (comp (by-key (map #(% first))) cat)]
                     [juxt (map #(% first))])]
      (fn [rf]
        ((reduce (apply f (sequence args xforms-map))) rf))))
  ([xforms-map coll]
    (transduce (transjuxt xforms-map) first coll)))

#_(defn intermix
   [xforms]
   (fn [rf]
     (let [mxrf (multiplexable rf)
           rfs (volatile! (into #{} (map #(%2 mxrf)) xforms))]
       (fn
         )))
   )
