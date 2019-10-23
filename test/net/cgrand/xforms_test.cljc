(ns net.cgrand.xforms-test
  (:require [clojure.test :refer [is deftest testing]]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as rf]))

(defn trial
  "A transducing context for testing that transducers are well-behaved towards
  linear use of the accumulator, init, completion and handling of reduced values.
  A \"poisonous\" reducing function rf is passed to the transducer.
  n is the number of calls to rf before it returns a reduced.
  accs is a collection of successive return values for rf."
  ([xform n coll]
    (trial xform n (repeatedly #(#?(:clj Object. :cljs js/Object.))) coll))
  ([xform n accs coll]
    (let [vaccs (volatile! accs)
          vstate (volatile! {:n n :acc (first @vaccs) :state :init})
          check-acc (fn [acc]
                      (when (reduced? acc)
                        (throw (ex-info "Called with reduced accumulator" (assoc @vstate :actual-acc acc))))
                      (when-not (identical? acc (:acc @vstate))
                        (throw (ex-info "Called with an unexpected accumulator (either non-linear or out of thin air)" (assoc @vstate :actual-acc acc)))))
          rf (fn
               ([]
                 (when-not (= :init (:state @vstate))
                   (throw (ex-info "Init arity called on a started or completed rf." @vstate)))
                 (:acc (vswap! vstate assoc :state :started)))
               ([acc]
                 (when (= :completed (:state @vstate))
                   (throw (ex-info "Completion arity called on an already completed rf." @vstate)))
                 (check-acc acc)
                 (:acc (vswap! vstate assoc :state :completed :acc (first (vswap! vaccs next)))))
               ([acc x]
                 (when (= :completed (:state @vstate))
                   (throw (ex-info "Step arity called on an already completed rf." @vstate)))
                 (when (= :reduced (:state @vstate))
                   (throw (ex-info "Step arity called on a reduced rf." @vstate)))
                 (check-acc acc)
                 (let [n (:n @vstate)]
                   (let [acc (first (vswap! vaccs next))]
                     (if (pos? n)
                      (:acc (vswap! vstate assoc :acc acc :n (dec n)))
                      (reduced (:acc (vswap! vstate assoc :acc acc :state :reduced))))))))
          res (transduce xform rf coll)]
      (check-acc res)
      (when-not (= :completed (:state @vstate))
        (throw (ex-info "Completion arity never called" @vstate)))
      true)))

(deftest proper-rf-usage
  (testing "Ensuring that reducing functions returned by transducers are well-behaved."
    (is (trial (x/by-key odd? identity)
          4 (range 16)))
    (is (trial (x/by-key odd? identity nil identity)
          4 (range 16)))
    (is (trial (x/by-key odd? (take 2))
          8 (range 16)))
    (is (trial (x/by-key odd? identity)
          8 (range 2)))
    (is (trial (x/partition 3 identity)
          4 (range 16)))
    (is (trial (x/partition 3 (take 2))
          8 (range 16)))
    (is (trial (x/partition 3 (take 2))
          8 (range 2)))
    (is (trial (x/reductions conj [])
          8 (range 2)))
    (is (trial (x/reductions conj)
          8 (range 2)))
    (is (trial (x/into [])
          4 (range 16)))
    (is (trial (x/for [x % y (range x)] [x y])
          4 (range 16)))
    (is (trial (x/reduce +)
          4 (range 16)))))

(deftest reductions
  (is (= (into [] (x/reductions +) (range 10)) [0 0 1 3 6 10 15 21 28 36 45]))
  (is (= (into [] (x/reductions +) (range 0)) [0]))
  (is (= (into [] (x/reductions +) (range 1)) [0 0]))
  (is (= (into [] (x/reductions +) (range 2)) [0 0 1]))
  (is (= (into [] (comp (x/reductions +) (take 2)) (range)) [0 0]))
  (is (= (into [] (comp (x/reductions +) (take 3)) (range)) [0 0 1]))
  (is (= (into [] (comp (take 3) (x/reductions +)) (range)) [0 0 1 3]))
  (is (= (into [] (x/reductions (constantly (reduced 42)) 0) (range)) [0 42])))

(deftest partition
  (is (= (into [] (x/partition 2 1 nil (x/into [])) (range 8))
        [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6] [6 7] [7]]))
  (is (= (into [] (x/partition 2 1 (x/into [])) (range 8))
        [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6] [6 7]]))
  (is (= (into [] (comp (x/partition 2 2 nil) (x/into [])) (range 8))
        [[[0 1] [2 3] [4 5] [6 7]]])))

(deftest without
  (is (= {0 :ok 2 :ok 4 :ok 6 :ok 8 :ok} (x/without (zipmap (range 10) (repeat :ok)) (filter odd?) (range 20))))
  (is (= #{0 2 4 6 8 } (x/without (set (range 10)) (filter odd?) (range 20)))))

#?(:clj
    (deftest iterator
      (is (true? (.hasNext (x/iterator x/count (.iterator (range 5))))))
      (is (is (= [5] (iterator-seq (x/iterator x/count (.iterator (range 5)))))))
      (is (= [[0 1] [1 2] [2 3] [3 4] [4]] (iterator-seq (x/iterator (x/partition 2 1 nil) (.iterator (range 5)))))))
    
    (deftest window-by-time
      (is (= (into 
               []
               (x/window-by-time :ts 4
                 (fn 
                   ([] clojure.lang.PersistentQueue/EMPTY)
                   ([q] (vec q))
                   ([q x] (conj q x)))
                 (fn [q _] (pop q)))
               (map (fn [x] {:ts x}) (concat (range 0 2 0.5) (range 3 5 0.25))))
             [[{:ts 0}] ; t =  0
      [{:ts 0}] ; t =  0.25
      [{:ts 0} {:ts 0.5}] ; t =  0.5
      [{:ts 0} {:ts 0.5}] ; t =  0.75
      [{:ts 0.5} {:ts 1.0}] ; t =  1.0
      [{:ts 0.5} {:ts 1.0}] ; t =  1.25
      [{:ts 1.0} {:ts 1.5}] ; t =  1.5
      [{:ts 1.0} {:ts 1.5}] ; t =  1.75
      [{:ts 1.5}] ; t =  2.0
      [{:ts 1.5}] ; t =  2.25
      [] ; t =  2.5
      [] ; t =  2.75
      [{:ts 3}] ; t =  3.0
      [{:ts 3} {:ts 3.25}] ; t =  3.25
      [{:ts 3} {:ts 3.25} {:ts 3.5}] ; t =  3.5
      [{:ts 3} {:ts 3.25} {:ts 3.5} {:ts 3.75}] ; t =  3.75
      [{:ts 3.25} {:ts 3.5} {:ts 3.75} {:ts 4.0}] ; t =  4.0
      [{:ts 3.5} {:ts 3.75} {:ts 4.0} {:ts 4.25}] ; t =  4.25
      [{:ts 3.75} {:ts 4.0} {:ts 4.25} {:ts 4.5}] ; t =  4.5
      [{:ts 4.0} {:ts 4.25} {:ts 4.5} {:ts 4.75}]])))) ; t =  4.75

(deftest do-not-kvreduce-vectors
  (is (= {0 nil 1 nil} (x/into {} (x/for [[k v] %] [k v]) [[0] [1]])))
  (is (= {0 nil 1 nil} (x/into {} (x/for [_ % [k v] [[0] [1]]] [k v]) ["a"]))))

(deftest sorting
  (is (= (range 100) (x/into [] (x/sort) (shuffle (range 100)))))
  (is (= (reverse (range 100)) (x/into [] (x/sort >) (shuffle (range 100)))))
  (is (= (sort-by str (range 100)) (x/into [] (x/sort-by str) (shuffle (range 100)))))
  (is (= (sort-by str (comp - compare) (range 100)) (x/into [] (x/sort-by str (comp - compare)) (shuffle (range 100))))))

(deftest by-key
  (testing "Respects reduced from multiplexed reductions and from the downstream reduction."
    (is (= {:x 3 :y 5}
           (into {}
                 (x/by-key (x/reduce rf/some))
                 [[:x 3] [:y 5] [:x 2]]))
        "Respects multiplexed reduced.")

    (is (= [:x 4]
           (transduce
             (x/by-key (map inc))
             rf/some
             [[:x 3] [:y 5] [:x 2]]))
        "Respects downstream reduced.")

    (is (= [:y 4]
           (transduce
             (x/by-key (x/reduce (fn
                                   ([] 0)
                                   ([sum] sum)
                                   ([sum x]
                                    (let [sum (+ sum x)]
                                      (if (> sum 4) (reduced 4) sum))))))
             rf/some
             [[:x 3] [:y 5] [:x 2]]))
        "Respects reduced in downstream complete.")))

(deftest multiplex
  (testing "Respects reduced from multiplexed reductions and from the downstream reduction."
    (testing "Respects multiplexed reduced."
      (is (= [3]
             (into []
                   (x/multiplex [(x/reduce rf/some)])
                   [3 5 2])))
      (is (= {:x 3}
             (into {}
                   (x/multiplex {:x (x/reduce rf/some)})
                   [3 5 2]))))

    (testing "Respects downstream reduced."
      (is (= 4
             (transduce
               (x/multiplex [(map inc)])
               rf/some
               [3 5 2])))
      (is (= [:x 4]
             (transduce
               (x/multiplex {:x (map inc)})
               rf/some
               [3 5 2]))))

    (testing "Doesn't repeat multiplexed completion."
      (is (= 2
             (transduce
               (x/multiplex [(x/reduce rf/last)])
               rf/some
               [3 5 2])))
      (is (= [:x 2]
             (transduce
               (x/multiplex {:x (x/reduce rf/last)})
               rf/some
               [3 5 2]))))

    (testing "Respects reduced in downstream complete."
      (is (= 3
             (transduce
               (x/multiplex [(x/reduce rf/some) (x/reduce rf/some)])
               rf/some
               [3 5 2])))
      (is (= 3
             (transduce
               (x/multiplex {:x (x/reduce rf/some) :y (x/reduce rf/some)})
               (completing rf/some second)
               [3 5 2]))))))
