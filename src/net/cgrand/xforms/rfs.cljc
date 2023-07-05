(ns net.cgrand.xforms.rfs
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [str last min max some])
  #?(:cljs (:require-macros
             [net.cgrand.macrovich :as macros]
             [net.cgrand.xforms.rfs :refer [or-instance?]])
      :clj (:require [net.cgrand.macrovich :as macros]))
  (:require [#?(:clj clojure.core :cljs cljs.core) :as core])
  #?(:cljd (:require ["dart:math" :as Math]))
  #?(:cljs (:import [goog.string StringBuffer])))

(macros/deftime
  (defmacro ^:private or-instance? [class x y]
    (let [xsym (gensym 'x_)]
      `(let [~xsym ~x]
         (if #?(:cljd (dart/is? ~xsym ~class)
                :default (instance? ~class ~xsym))
           ~(with-meta xsym {:tag class}) ~y)))))

(declare str!)

(macros/usetime

#? (:cljs
     (defn ^:private cmp [f a b]
       (let [r (f a b)]
         (cond
           (number? r) r
           r -1
           (f b a) 1
           :else 0))))

(defn minimum
 ([#?(:cljd comparator :clj ^java.util.Comparator comparator :cljs comparator)]
  (let [#?@(:cljd [comparator (dart-comparator comparator)] :default [])]
    (fn
      ([] nil)
      ([x] x)
      ([a b] (cond
               (nil? a) b
               (nil? b) a
               (pos? #?(:cljd (comparator a b)
                        :clj (.compare comparator a b)
                        :cljs (cmp comparator a b))) b
               :else a)))))
 ([#?(:cljd comparator :clj ^java.util.Comparator comparator :cljs comparator) absolute-maximum]
  (let [#?@(:cljd [comparator (dart-comparator comparator)] :default [])]
    (fn
      ([] ::+infinity)
      ([x] (if (#?(:clj identical? :cljs keyword-identical?) ::+infinity x)
             absolute-maximum
             x))
      ([a b]
       (if (or
             (#?(:clj identical? :cljs keyword-identical?) ::+infinity a)
             (pos? #?(:cljd (comparator a b)
                      :clj (.compare comparator a b)
                      :cljs (cmp comparator a b))))
         b a))))))

(defn maximum
  ([#?(:cljd comparator :clj ^java.util.Comparator comparator :cljs comparator)]
   (let [#?@(:cljd [comparator (dart-comparator comparator)] :default [])]
     (fn
       ([] nil)
       ([x] x)
       ([a b] (cond
                (nil? a) b
                (nil? b) a
                (neg? #?(:cljd (comparator a b)
                         :clj (.compare comparator a b)
                         :cljs (cmp comparator a b))) b
                :else a)))))
  ([#?(:cljd comparator :clj ^java.util.Comparator comparator :cljs comparator) absolute-minimum]
   (let [#?@(:cljd [comparator (dart-comparator comparator)] :default [])]
     (fn
       ([] ::-infinity)
       ([x] (if (#?(:clj identical? :cljs keyword-identical?) ::-infinity x)
              absolute-minimum
              x))
       ([a b]
        (if (or (#?(:clj identical? :cljs keyword-identical?) ::-infinity a)
              (neg? #?(:cljd (comparator a b)
                       :clj (.compare comparator a b)
                       :cljs (cmp comparator a b))))
          b a))))))

(def min (minimum compare))

(def max (maximum compare))

(defn avg
  "Reducing fn to compute the arithmetic mean."
  ([] nil)
  ([#?(:cljd ^{:tag #/(List double)} acc :clj ^doubles acc :cljs ^doubles acc)]
   (when acc (/ (aget acc 1) (aget acc 0))))
  ([acc x] (avg acc x 1))
  ([#?(:cljd ^{:tag #/(List double)} acc :clj ^doubles acc :cljs ^doubles acc) x w]
    (let [acc (or acc #?(:cljd (double-array 2) :clj (double-array 2) :cljs #js [0.0 0.0]))]
      (doto acc
        (aset 0 (+ (aget acc 0) w))
        (aset 1 (+ (aget acc 1) (* w x)))))))

(defn sd
  "Reducing fn to compute the standard deviation. Returns 0 if no or only one item."
  ([] #?(:cljd (double-array 3) :clj (double-array 3) :cljs #js [0.0 0.0 0.0]))
  ([#?(:cljd ^{:tag #/(List double)} a :default ^doubles a)]
    (let [s (aget a 0) n (aget a 2)]
      (if (< 1 n)
        (Math/sqrt (/ s (dec n)))
        0.0)))
  ([#?(:cljd ^{:tag #/(List double)} a :default ^doubles a) x]
    (let [s (aget a 0) m (aget a 1) n (aget a 2)
          d (- x m)
          n (inc n)
          m' (+ m (/ d n))]
      (doto a
        (aset 0 (+ s (* d (- x m'))))
        (aset 1 m')
        (aset 2 n)))))

(defn last
  "Reducing function that returns the last value."
  ([] nil)
  ([x] x)
  ([_ x] x))

(defn some
  "Reducing function that returns the first logical true value."
  ([] nil)
  ([x] x)
  ([_ x] (when x (reduced x))))

(defn str!
  "Like xforms/str but returns a StringBuilder."
  ([] (#?(:cljd StringBuffer :clj StringBuilder. :cljs StringBuffer.)))
  ([sb] (or-instance? #?(:cljd StringBuffer  :clj StringBuilder :cljs StringBuffer) sb
          (#?(:cljd StringBuffer :clj StringBuilder. :cljs StringBuffer.) (core/str sb))))
  ; the instance? checks are for compatibility with str in case of seeded reduce/transduce.
  ([sb x] (#?(:cljd .write :default .append)
            (or-instance?
              #?(:cljd StringBuffer :clj StringBuilder :cljs StringBuffer) sb
              (#?(:cljd StringBuffer :clj StringBuilder. :cljs StringBuffer.) (core/str sb)))
            x)))

(def str
  "Reducing function to build strings in linear time. Acts as replacement for clojure.core/str in a reduce/transduce call."
  (completing str! core/str))

#_(defn juxt
   "Returns a reducing fn which compute all rfns at once and whose final return
   value is a vector of the final return values of each rfns."
   [& rfns]
   (let [rfns (mapv ensure-kvrf rfns)]
     (kvrf
       ([] (mapv #(vector % (volatile! (%))) rfns))
       ([acc] (mapv (fn [[rf vacc]] (rf (unreduced @vacc))) acc))
       ([acc x]
         (let [some-unreduced (core/reduce (fn [some-unreduced [rf vacc]]
                                            (when-not (reduced? @vacc) (vswap! vacc rf x) true))
                                false acc)]
           (if some-unreduced acc (reduced acc))))
       ([acc k v]
         (let [some-unreduced (core/reduce (fn [some-unreduced [rf vacc]]
                                            (when-not (reduced? @vacc) (vswap! vacc rf k v) true))
                                false acc)]
           (if some-unreduced acc (reduced acc)))))))

#_(defn juxt-map
   [& key-rfns]
   (let [f (apply juxt (take-nth 2 (next key-rfns)))
         keys (vec (take-nth 2 key-rfns))]
     (let [f (ensure-kvrf f)]
       (kvrf
         ([] (f))
         ([acc] (zipmap keys (f acc)))
         ([acc x] (f acc x))
         ([acc k v] (f acc k v))))))
)
