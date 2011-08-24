(ns quadrant.core
  (:gen-class))

(defn update! [trmp k f]
  (assoc! trmp k (f (trmp k))))

(defn flip-along-x [[x y]] [x -y])
(defn flip-along-y [[x y]] [-x y])

(defn update-locs [f locs [start end]]
  (loop [locs-tr (transient locs) i start]
    (if (> i end) (persistent! locs-tr)
        (recur (update! locs-tr i f) (inc i)))))

(defn count-quadrants [locs [start end]]
  (loop [quadrants (transient [0 0 0 0]) i start]
    (if (> i end) (persistent! quadrants) (let [[x y] (locs i)
                                                q = (if (> x 0) (if (> y 0) 0 3) (if (> y 0) 1 2))]
                                            (recur (update! quadrants q inc) (inc i))))))
  

(defn read-stdin []
  (let [vs (or (seq (doall (line-seq (java.io.BufferedReader. *in*))))
               (seq (doall (line-seq
                            (java.io.BufferedReader.
                             (java.io.FileReader. "inp.txt"))))))]
    (->> (map #(re-seq #"[^ ]+" %) vs)
         (map #(map read-string %)))))

#_(read-stdin)

(defn -main []
  
  
    
