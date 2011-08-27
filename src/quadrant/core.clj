(ns quadrant.core
  (:refer-clojure)
  (:gen-class)
  (:require [clojure.zip :as z]))

(defmacro thrush-with-pattern [[pattern] first & exprs]
  (if (seq exprs) `(let [~pattern ~first] (thrush-with-pattern [~pattern] ~@exprs)) first))

(defn add-quadrant-counts [a b]
  {:pre [#_(do (println [a b]) true)]}
  (let [mycompare (fn [{:keys [q1 q2 q3 q4] :or {q1 0 q2 0 q3 0 q4 0}}
                       {:keys [q1- q2- q3- q4-] :or {q1- 0 q2- 0 q3- 0 q4- 0}}]
                    (first (keep (comp not zero?) (map compare [q1 q2 q3 q4] [q1- q2- q3- q4-]))))
        helper (memoize (fn [a b] (merge-with + a b)))]
    (if (= (mycompare a b) 1) (helper b a) (helper a b))))

(let [quadrant-flipper {'X {:q1 :q4 :q2 :q3 :q3 :q2 :q4 :q1}
                        'Y {:q1 :q2 :q2 :q1 :q3 :q4 :q4 :q3}
                        'C identity}
      map-keys (fn map-keys [orig-map old-new-key-map]
                 (into {} (map (fn [[k v]] [(old-new-key-map k) v]) orig-map)))]
  (defn execute-cmd [root-loc [cmd start end]]
    (let [map-composer (fn [& fs]
                         (reduce (fn [f1 f2]
                                   (cond
                                    (= f1 identity) f2
                                    (= f2 identity) f1
                                    :else (let [[[x y] :as quadrant-pairs] (map (fn [[k v]] [k (f1 v)]) f2)]
                                            (if (= x y) identity
                                                (into {} quadrant-pairs))))) fs))
          helper (fn helper [loc parent-out-of-range-mapper parent-in-range-mapper [s e :as rng]]
                   (let [{:keys [start end mid val child-mapper] :as nd :or {child-mapper identity}} (z/node loc)
                         out-of-range-mapper (map-composer parent-out-of-range-mapper child-mapper)
                         in-range-mapper (map-composer parent-in-range-mapper child-mapper)]
                     (cond
                      (nil? rng) [{} (z/edit loc (fn [nd] (thrush-with-pattern [x]
                                                            (update-in nd [:val] #(map-keys % parent-out-of-range-mapper))
                                                            (if mid (if (not= identity out-of-range-mapper)
                                                                      (assoc x :child-mapper out-of-range-mapper)
                                                                      (dissoc x :child-mapper)) x))))]
                      (and (= s start) (= e end)) (let [ret-loc (z/edit loc (fn [nd]
                                                                              (thrush-with-pattern [x]
                                                                                (update-in nd [:val] #(map-keys % parent-in-range-mapper))
                                                                                (if mid (if (not= identity in-range-mapper)
                                                                                          (assoc x :child-mapper in-range-mapper)
                                                                                          (dissoc x :child-mapper)) x))))]
                                                    [(:val (z/node ret-loc)) ret-loc])
                      :else
                      (let [case-type (cond (< s mid e) :s-m-e (< e mid) :s-e-m (< mid s) :m-s-e)
                            loc-left-initial (z/down loc)
                            m (int mid)
                            [v-left loc-left-final] (helper loc-left-initial out-of-range-mapper in-range-mapper (case case-type :s-m-e [s m] :s-e-m [s e] :m-s-e nil))
                            loc-right-initial (z/right loc-left-final)
                            [v-right loc-right-final] (helper loc-right-initial out-of-range-mapper in-range-mapper (case case-type :s-m-e [(inc m) e] :s-e-m nil :m-s-e [s e]))
                            cur-loc (z/up loc-right-final)
                            ret-loc (z/edit cur-loc (fn [nd] (-> (dissoc nd :child-mapper)
                                                                 (assoc :val (add-quadrant-counts (:val (z/node loc-left-final))
                                                                                                  (:val (z/node loc-right-final)))))))]
                        [(add-quadrant-counts v-left v-right) ret-loc]))))]
      (helper root-loc identity (quadrant-flipper cmd) [start end]))))


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

(defn solve []
  (let [[tree queries] (read-stdin :fname "inp100.txt")
        root-loc (z/zipper :mid #(keep identity [(:l %) (:r %)])
                           (fn [nd [x y & rest]]
                             #_(println "calling make-node")
                             #_(clojure.pprint/pprint (self-keyed-map nd x y))
                             (cond
                              rest (throw (Exception. "Invalid use of make-node in zipper"))
                              (and x y) (assoc nd :l x :r y :val (add-quadrant-counts (:val x) (:val y)))
                              x (throw (Exception. "Invalid use of make-node in zipper"))
                              :else nd)) tree)]
    (reduce (fn [loc [id [cmd _ _ :as q]]]
              (let [[v new-loc] (execute-cmd loc q)]
                (if (= cmd 'C) (let [{:keys [q1 q2 q3 q4] :or {q1 0 q2 0 q3 0 q4 0}} v]
                                 (println (str q1 " " q2 " " q3 " " q4)))) new-loc))
            root-loc (map-indexed vector queries))))

(defn -main []
  (solve))

