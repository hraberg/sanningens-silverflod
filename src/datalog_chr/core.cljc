(ns datalog-chr.core
  (:require [clojure.core :as cc]
            [clojure.walk :as w]
            [datascript.core :as d]))

(defn lvar? [v]
  (and (symbol? v) (= \? (first (name v)))))

(defn extract-lvars [x]
  (let [v (volatile! #{})]
    (w/postwalk #(when (lvar? %)
                   (vswap! v conj %)) x)
    @v))

(defn compile-rhs [rhs]
  (let [vars (sort (mapcat extract-lvars rhs))
        src `(fn [~@vars] ~rhs)]
    (with-meta (eval src) {:src src :vars vars})))

(defn build-rule [{:keys [then] :as rule}]
  (assoc rule :then
         (cond-> then
           (not (fn? then)) (some-> compile-rhs))))

(defn format-rule [{:keys [take drop then] when' :when}]
  (vec (concat take
               (when (and take drop)
                 ['\\])
               drop
               [(if-not drop
                  '==>
                  '<=>)]
               (some-> when' (concat ['|]))
               (condp some [then]
                 vector? then
                 nil? [true]
                 (-> then meta :src vector)))))

(defn run-rule [conn {:keys [lhs rhs to-drop]}]
  (when-let [result (d/q lhs conn)]
    (let [[to-drop args] (split-at to-drop result)]
      [to-drop (some-> rhs (apply args))])))

(defn add-tx [to-add]
  (map (fn [entity id]
         (assoc (cond->> entity
                  (vector? entity) (apply hash-map))
                :db/id (- (inc id))))
       to-add (range)))

(defn retract-tx [to-drop]
  (for [id to-drop]
    [:db.fn/retractEntity id]))

(defn rule->executable-rule [rule]
  (let [{:keys [take drop when then]} (build-rule rule)
        head (concat take drop)
        head-vars (map (comp symbol (partial str "?"))
                       (range (count head)))]
    {:lhs (vec (concat [:find (vec (concat (cc/drop (count take) head-vars)
                                           (-> then meta :vars)))]
                       [:where]
                       (map (comp vec cons) head-vars head)
                       (cc/when (> (count head-vars) 1)
                         [[(cons 'not= head-vars)]])
                       when))
     :rhs then
     :to-drop (count drop)}))

(defn run [conn all-rules]
  (let [all-rules (map rule->executable-rule all-rules)]
    (loop [[rule & rules] (shuffle all-rules) chages nil]
      (let [[to-drop to-add] (run-rule @conn rule)
            txs (concat (add-tx to-add) (retract-tx to-drop))
            {:keys [tx-data]} (d/transact! conn txs)
            chages (concat chages tx-data)]
        (if (and (nil? rules) (empty? chages))
          conn
          (recur (or rules (shuffle all-rules)) (when rules
                                                  chages)))))))

(defn run-once [rules wm]
  @(doto (d/create-conn)
     (d/transact! (add-tx wm))
     (run rules)))

(defn predicate-values [pred conn]
  (d/q '[:find ?x :in $ ?pred
         :where [_ ?pred ?x]]
       conn pred))

(def gcd-rules '[{:drop [[:gcd 0]]}

                 {:take [[:gcd ?n]]
                  :drop [[:gcd ?m]]
                  :when [[(>= ?m ?n)]
                         [(pos? ?n)]]
                  :then [[:gcd (- ?m ?n)]]}])

(->> (run-once gcd-rules [[:gcd 9] [:gcd 6] [:gcd 3]])
     (predicate-values :gcd)
     (= #{[3]})
     assert)

(def prime-rules '[{:take [[:prime ?i]]
                    :drop [[:prime ?j]]
                    :when [[(mod ?j ?i) ?mod]
                           [(zero? ?mod)]]}

                   {:drop [[:upto 1]]}

                   {:drop [[:upto ?n]]
                    :when [[(> ?n 1)]]
                    :then [[:prime ?n]
                           [:upto (dec ?n)]]}])

(->> (run-once prime-rules [[:upto 7]])
     (predicate-values :prime)
     (= #{[7] [5] [3] [2]})
     assert)
