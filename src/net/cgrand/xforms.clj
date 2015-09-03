(ns net.cgrand.xforms
  "Extra transducers for Clojure"
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [reduce for partition str])
  (:require [clojure.core :as clj]))

(defmacro for
  "Like clojure.core/for with the first expression being replaced by % (or _). Returns a transducer."
  [[binding %or_ & seq-exprs] body]
  (assert (and (symbol? %or_) (#{"%" "_"} (name %or_)))
    "The second element of the comprehension vector must be % or _.")
  (let [rf (gensym 'rf)
        acc (gensym 'acc)
        body
        (clj/reduce (fn [body [expr binding]]
                      (case binding
                        :let `(let ~expr ~body)
                        :when `(if ~expr ~body ~acc)
                        :while `(if ~expr ~body (reduced ~acc))
                        `(reduce (fn [~acc ~binding] ~body) ~acc ~expr))) 
          `(~rf ~acc ~body)
          (clj/partition 2 (rseq (vec seq-exprs))))]
    `(fn [~rf]
       (fn
         ([] (~rf))
         ([~acc] (~rf ~acc))
         ([~acc ~binding] ~body)))))

(defn reduce
  "A transducer that reduces a collection to a 1-item collection consisting of only the reduced result."
  ([f]
    (fn [rf]
     (let [vacc (volatile! (f))]
       (fn
         ([] (rf))
         ([acc] (rf (rf acc (f (unreduced @vacc)))))
         ([acc x]
           (if (reduced? (vswap! vacc f x))
             (reduced acc)
             acc))))))
  ([f init]
    (reduce (fn ([] init) ([acc] (f acc)) ([acc x] (f acc x))))))

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

(defn- noprf "The noop reducing function" ([acc] acc) ([acc _] acc))

(defn by-key
  "Returns a transducer which partitions items according to kfn.
   It applies the transform specified by xform to each partition.
   Partitions contain the \"value part\" (as returned by vfn) of each item.
   The resulting transformed items are wrapped back into a \"pair\" using the pair function.
   Default values for kfn, vfn and pair are first, second and vector."
  ([xform] (by-key key' val' vector xform))
  ([kfn xform] (by-key kfn identity vector xform))
  ([kfn vfn xform] (by-key kfn vfn vector xform))
  ([kfn vfn pair xform]
    (fn [rf]
      (let [make-rf (if pair
                      (fn [k] (fn ([acc] (rf acc)) ([acc v] (rf acc (pair k v)))))
                      (constantly rf))
            m (volatile! (transient {}))]
        (fn self
          ([] (rf))
          ([acc] (clj/reduce (fn [acc krf] (krf acc)) acc (vals (persistent! @m))))
          ([acc x]
            (let [k (kfn x)
                  krf (or (get @m k) (doto (xform (make-rf x)) (->> (vswap! m assoc! k))))
                  acc (krf acc (vfn x))]
              (when (reduced? acc)
                (vswap! m assoc! k noprf))
              (unreduced acc))))))))



