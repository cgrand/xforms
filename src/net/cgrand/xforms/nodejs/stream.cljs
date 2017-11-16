(ns net.cgrand.xforms.nodejs.stream)

(def ^:private Transform (.-Transform (js/require "stream")))

(defn transformer
  "Returns a stream.Transform object that performs the specified transduction.
   options is a js object as per stream.Transform -- however :readableObjectMode and :writableObjectMode are set to true by default."
  ([xform] (transformer #js {} xform))
  ([options xform]
    (let [xrf (xform (fn
                       ([transform] (doto transform .end))
                       ([transform x] 
                         (when-not (.push transform x)
                           (throw (js/Error. "Transformer's internal buffer is full, try passing a larger :highWaterMark option.")))
                         transform)))]
      (specify! (Transform. (.assign js/Object #js {:readableObjectMode true
                                                    :writableObjectMode true} options))
        Object
        (_transform [this x _ cb]
          (try
            (when (reduced? (xrf this x))
              (.push this nil))
            (cb)
            (catch :default err
              (cb err))))
        (_flush [this cb]
          (try
            (xrf this)
            (cb)
            (catch :default err
              (cb err))))))))
