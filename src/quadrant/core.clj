(ns quadrant.core
  (:gen-class)
  (:require [clojure.zip :as z])
  (:use [quadrant.debug]))

(defn update! [trmp k f]
  (assoc! trmp k (f (trmp k))))

(def _ :undefined)

(defmacro thrush-with-pattern [[pattern] first & exprs]
  (if (seq exprs) `(let [~pattern ~first] (thrush-with-pattern [~pattern] ~@exprs)) first))

(defmacro thrush-with-pattern-dbg [[pattern] first & exprs]
  (if (seq exprs) `(let [sym# ~first
                         ~pattern sym#]                     
                     (println ['~first sym#])
                     (thrush-with-pattern-dbg [~pattern] ~@exprs))
      `(let [sym# ~first]
         (println ['~first sym#]) sym#)))

(defn add-quadrant-counts [a b]
  (let [mycompare (fn [{:keys [q1 q2 q3 q4] :or {q1 0 q2 0 q3 0 q4 0}}
                       {:keys [q1- q2- q3- q4-] :or {q1- 0 q2- 0 q3- 0 q4- 0}}]
                    (first (keep (comp not zero?) (map compare [q1 q2 q3 q4] [q1- q2- q3- q4-]))))
        helper (memoize (fn [a b] (if (and a b) (merge-with + a b) (or a b))))]
    (if (= (mycompare a b) 1) (helper b a) (helper a b))))

(let [quadrant-flipper {'X {:q1 :q4 :q2 :q3 :q3 :q2 :q4 :q1}
                        'Y {:q1 :q2 :q2 :q1 :q3 :q4 :q4 :q3}
                        'C identity}
      map-keys (fn map-keys [orig-map old-new-key-map]
                 (into {} (map (fn [[k v]] [(old-new-key-map k) v]) orig-map)))
      map-vals (fn map-vals [orig-map old-new-val-map]
                 (into {} (map (fn [[k v]] [k (old-new-val-map v)]) orig-map)))]
  (defn execute-cmd [tree [cmd start end]]
    #_(clojure.inspector/inspect-tree (self-keyed-map tree cmd start end))
    (let [root-loc (z/zipper identity #(keep identity [(:l %) (:r %)])
                             (fn [nd [x y & rest]]
                               (cond
                                rest (throw (Exception. "Invalid use of make-node in zipper"))
                                (and x y) (assoc nd :l x :r y)
                                x (assoc nd :l x)
                                :else nd)) tree)
          helper1 (fn helper1 [loc parent-whole-modifier parent-part-modifier s e]
                    (let [{:keys [start end mid val child-modifier]} (z/node loc)
                          f-new-whole (if child-modifier (comp parent-whole-modifier child-modifier) parent-whole-modifier)
                          f-new-part (if child-modifier (comp parent-part-modifier child-modifier) parent-part-modifier)]
                      (cond
                       (and (nil? s) (nil? e)) [nil (z/edit loc (fn [nd] (-> (update-in nd [:val] #(map-keys % parent-whole-modifier))
                                                                             (assoc :child-modifiers f-new-whole))))]
                       (and (= s start) (= e end)) (let [ret-loc (z/edit loc (fn [nd] (-> (update-in nd [:val] #(map-keys % parent-part-modifier))
                                                                                          (assoc :child-modifiers f-new-part))))]
                                                     [(:val (z/node ret-loc)) ret-loc])
                       (< s mid e) (let [loc-left-initial (z/down loc)
                                         [v-left loc-left-final] (helper1 loc-left-initial f-new-whole f-new-part s (int mid))
                                         loc-right-initial (z/right loc-left-final)
                                         [v-right loc-right-final] (helper1 loc-right-initial f-new-whole f-new-part (inc (int mid)) e)
                                         cur-loc (z/up loc-right-final)
                                         ret-loc (z/edit cur-loc (fn [nd] (-> (dissoc nd :child-modifiers)
                                                                              (assoc :val (add-quadrant-counts (:val (z/node loc-left-final))
                                                                                                               (:val (z/node loc-right-final)))))))]
                                     [(add-quadrant-counts v-left v-right) ret-loc])
                       (< e mid) (let [loc-left-initial (z/down loc)
                                       [v-left loc-left-final] (helper1 loc-left-initial f-new-whole f-new-part s e)
                                       loc-right-initial (z/right loc-left-final)
                                       [_ loc-right-final] (helper1 loc-left-final f-new-whole identity nil nil)
                                       cur-loc (z/up loc-right-final)
                                       ret-loc (z/edit cur-loc (fn [nd] (-> (dissoc nd :child-modifiers)
                                                                            (assoc :val (add-quadrant-counts (:val (z/node loc-left-final))
                                                                                                             (:val (z/node loc-right-final)))))))]
                                   [v-left ret-loc])
                       (< mid s) (let [loc-left-initial (z/down loc)
                                       [_ loc-left-final] (helper1 loc-left-initial f-new-whole identity nil nil)
                                       loc-right-initial (z/right loc-left-final)
                                       [v-right loc-right-final] (helper1 loc-right-initial f-new-whole f-new-part s e)
                                       cur-loc (z/up loc-right-final)
                                       ret-loc (z/edit cur-loc (fn [nd] (-> (dissoc nd :child-modifiers)
                                                                            (assoc :val (add-quadrant-counts (:val (z/node loc-left-final))
                                                                                                             (:val (z/node loc-right-final)))))))]
                                   [v-right ret-loc]))))
          [v loc] (helper1 root-loc identity (quadrant-flipper cmd) start end)]
      [v (z/root loc)])))

#_(solve)
(defn read-stdin [& {:keys [fname] :or {fname "inp.txt"}}]
  (let [vs (or (seq (doall (line-seq (java.io.BufferedReader. *in*))))
               (seq (doall (line-seq
                            (java.io.BufferedReader.
                             (java.io.FileReader. fname))))))
        n-q (->> (map #(re-seq #"[^ ]+" %) vs)
                 (map #(map read-string %)))
        [q1 q2 q3 q4] [{:q1 1} {:q2 1} {:q3 1} {:q4 1}]
        n (ffirst n-q)
        nodes (->> n-q (drop 1) (take n) (map (fn [i [x y]] {:start i :end i :val (if (> x 0) (if (> y 0) q1 q4) (if (> y 0) q2 q3))}) (drop 1 (range))))
        tree (loop [[x y :as cur-nodes] nodes]
               (if-not y x
                       (let [new-nodes (map (fn [[{:as w1 s1 :start e1 :end v1 :val} {:as w2 s2 :start e2 :end v2 :val}]]
                                              (if w2 {:start s1 :end e2 :mid (/ (+ e1 s2) 2) :val (add-quadrant-counts v1 v2) :l w1 :r w2} w1)) (partition-all 2 cur-nodes))]
                         (recur new-nodes))))
        [[nq] & queries] (drop (+ n 1) n-q)]
    [tree (into [] (take nq queries))]))

#_(time (def d (read-stdin :fname "inp.txt")))
#_(time (read-stdin :fname "inp10.txt"))
#_(time (def tt (read-stdin :fname "inp11.txt")))
#_(time (def tt (read-stdin :fname "t.txt")))
#_(read-stdin)

(defn solve []
  (let [[tree queries] (read-stdin :fname "l.2.txt")]
    (reduce (fn [ct [cmd _ _ :as q]]
              (let [[v nt] (execute-cmd ct q)]
                (if (= cmd 'C) (let [{:keys [q1 q2 q3 q4] :or {q1 0 q2 0 q3 0 q4 0}} v]
                                 (println (str q1 " " q2 " " q3 " " q4)))) nt)) tree queries)))
#_(solve)
(defn -main []
  (solve))

(comment 
  (def s {:l {:l {:val 4}
              :r {:val 6}
              :val 10}
          :r {:val 20
              :l {:val 11
                  :l {:val 3}
                  :r {:val 8}}
              :r {:val 9
                  :l {:val 2}
                  :r {:val 7}}}
          :val 30})
  (def btree (z/zipper identity  #(keep identity [(:l %) (:r %)])
                       (fn [nd [x y & rest]]
                         (cond
                          rest (throw (Exception. "Invalid use of make-node in zipper"))
                          (and x y) (assoc nd :l x :r y)
                          x (assoc nd :l x)
                          :else nd)) s))
  (def inced-btree 
    (let [myinc #(when %
                   (clojure.pprint/pprint (:val %))
                   (update-in % [:val] inc))]
      (loop [loc btree]
        (if (z/end? loc) (z/root loc)
            (recur (-> (z/edit loc myinc) z/next)))))))