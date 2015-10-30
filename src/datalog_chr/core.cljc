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

(defn rule->map [rule]
  (if (map? rule)
    rule
    (->> (partition 2 (partition-by keyword? rule))
         (reduce (fn [acc [[kw] clause]]
                   (assoc acc kw (vec clause))) {}))))

(defn entity->constraint [e]
  (mapv e (sort (keys (dissoc e :db/id)))))

(defn constraint->entity [c]
  (zipmap (map #(keyword "chr" (str "at_" %)) (range)) c))

(defn constraint->datoms [id c]
  (mapv (comp vec (partial cons id))
        (constraint->entity c)))

(defn position-constraints->datoms [cs]
  (->> cs
       (map-indexed
        (fn [idx c]
          (constraint->datoms (symbol (str "?" idx)) c)))
       (reduce into [])))

(defn build-rule [rule]
  (let [{:keys [then] :as rule} (rule->map rule)]
    (cond-> rule
      (sequential? then) (assoc :then (compile-rhs then)))))

(defn format-rule [{:keys [take drop then when]}]
  (vec (concat take
               (cc/when (and take drop)
                 ['\\])
               drop
               [(if-not drop
                  '==>
                  '<=>)]
               (some-> when (concat ['|]))
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
                  (vector? entity) constraint->entity)
                :db/id (- (inc id))))
       to-add (range)))

(defn retract-tx [to-drop]
  (for [id to-drop]
    [:db.fn/retractEntity id]))

(defn rule->executable-rule [rule]
  (let [{:keys [take drop when then]} (build-rule rule)
        head (position-constraints->datoms (concat take drop))
        head-vars (distinct (map first head))]
    {:lhs (vec (concat [:find (vec (concat (cc/drop (count take) head-vars)
                                           (-> then meta :vars)))]
                       [:where]
                       head
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

(defn constraints [db]
  (->> (d/q '[:find [(pull ?e [*]) ...] :where [?e]] db)
       (map entity->constraint)
       set))

(def gcd-rules '[[:drop [:gcd 0]]

                 [:take [:gcd ?n]
                  :drop [:gcd ?m]
                  :when
                  [(>= ?m ?n)]
                  [(pos? ?n)]
                  :then [:gcd (- ?m ?n)]]])

(->> (run-once gcd-rules [[:gcd 9] [:gcd 6] [:gcd 3]])
     constraints
     (= #{[:gcd 3]})
     assert)

(def prime-rules '[[:take [:prime ?i]
                    :drop [:prime ?j]
                    :when
                    [(mod ?j ?i) ?mod]
                    [(zero? ?mod)]]

                   [:drop [:upto 1]]

                   [:drop [:upto ?n]
                    :when [(> ?n 1)]
                    :then
                    [:prime ?n]
                    [:upto (dec ?n)]]])

(->> (run-once prime-rules [[:upto 7]])
     constraints
     (= #{[:prime 7] [:prime 5] [:prime 3] [:prime 2]})
     assert)

(def fib-rules '[[:take
                  [:upto ?max]
                  [:fib ?a ?av]
                  [:fib ?b ?bv]
                  :when
                  [(inc ?a) ?b]
                  [(< ?b ?max)]
                  :then
                  [:fib (inc ?b) (+ ?av ?bv)]]])
