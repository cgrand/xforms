(ns net.cgrand.xforms.io
  (:require [clojure.java.io :as io]
    [clojure.edn :as edn]))

(defn keep-opts [m like]
  (let [ns (namespace like)]
    (into {}
      (keep (fn [[k v]]
              (when (= ns (or (namespace k) ns))
                [(keyword (name k)) v])))
      m)))

(defn lines-in
  "Returns a reducible view over the provided input.
   Input is read line by line. Coercion of the input is done by io/reader (and opts are passed to it).
   Input is automatically closed upon completion or error."
  [in & opts]
  (let [no-init (Object.)]
    (reify clojure.lang.IReduce 
      (reduce [self f] (.reduce self f no-init))
      (reduce [self f init]
        (with-open [rdr (apply io/reader in opts)]
          (let [rdr (cond-> rdr (not (instance? java.io.BufferedReader rdr)) java.io.BufferedReader.)
                init (if (identical? init no-init)
                       (or (.readLine rdr) (f))
                       init)]
            (loop [state init]
              (if-some [line (.readLine rdr)]
                (let [state (f state line)]
                  (if (reduced? state)
                    (unreduced state)
                    (recur state)))
                state))))))))

(defn lines-out
  "Reducing function that writes values serialized to its accumulator (a java.io.Writer).
   Returns the writer."
  ([w] w)
  ([^java.io.Writer w line]
    (doto w
      (.write (str line))
      (.newLine))))

(defn edn-in
  "Returns a reducible view over the provided input.
   Input is read form by form. Coercion of the input is done by io/reader.
   Input is automatically closed upon completion or error.
   Unqualified options are passed to both edn/read and io/writer, options qualified by clojure.java.io
   are only passed (once dequalified) to io/writer, options qualified by clojure.edn are only passed to
   edn/read"
  [in & {:as opts}]
  (let [no-init (Object.)]
    (reify clojure.lang.IReduce 
     (reduce [self f] (.reduce self f no-init))
     (reduce [self f init]
       (with-open [rdr (apply io/reader in (mapcat seq (keep-opts opts ::io/opts)))]
         (let [rdr (cond-> rdr (not (instance? java.io.PushbackReader rdr)) java.io.PushbackReader.)
               opts (assoc (keep-opts opts ::edn/opts) :eof no-init)
               init (if (identical? init no-init)
                      (let [form (edn/read opts rdr)]
                        (if (identical? no-init form)
                          (f)
                          form))
                      init)]
           (loop [state init]
             (let [form (edn/read opts rdr)]
               (if (identical? no-init form)
                 state
                 (let [state (f state form)]
                   (if (reduced? state)
                     (unreduced state)
                     (recur state))))))))))))

(defn edn-out
  "Reducing function that writes values serialized as EDN to its accumulator (a java.io.Writer).
   Returns the writer."
  ([w] w)
  ([^java.io.Writer w x]
    (binding [*out* w
              *print-length* nil
              *print-level* nil
              *print-dup* false
              *print-meta* false
              *print-readably* true]
      (prn x)
      w)))