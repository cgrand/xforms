(ns net.cgrand.xforms.rfs
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [str last min max])
  (:require [clojure.core :as clj]))

(defn minimum
  ([comparator]
    (minimum comparator nil))
  ([^java.util.Comparator comparator absolute-maximum]
    (fn
      ([] ::+∞)
      ([x] (if (identical? ::+∞ x)
             absolute-maximum
             x))
      ([a b] (if (or (identical? ::+∞ a) (pos? (.compare comparator a b))) b a)))))

(defn maximum
  ([comparator]
    (maximum comparator nil))
  ([^java.util.Comparator comparator absolute-minimum]
    (fn
      ([] ::-∞)
      ([x] (if (identical? ::-∞ x)
             absolute-minimum
             x))
      ([a b] (if (or (identical? ::-∞ a) (neg? (.compare comparator a b))) b a)))))

(def min (minimum compare))

(def max (maximum compare))

(defn avg
  "Reducing fn to compute the arithmetic mean."
  ([] (transient [0 0]))
  ([[n sum]] (/ sum n))
  ([acc x] (avg acc x 1))
  ([[n sum :as acc] x w]
    (-> acc (assoc! 0 (+ n w)) (assoc! 1 (+ sum (* w x))))))

(defn last
  "Reducing function that returns the last value."
  ([] nil)
  ([x] x)
  ([_ x] x))

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

#_(defn juxt
   "Returns a reducing fn which compute all rfns at once and whose final return
   value is a vector of the final return values of each rfns."
   [& rfns]
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