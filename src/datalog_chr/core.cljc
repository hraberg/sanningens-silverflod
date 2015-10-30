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

(defn compile-rhs [name rhs]
  (let [vars (sort (mapcat extract-lvars rhs))
        src `(~'fn ~(symbol (str (or name "rhs"))) [~@vars] ~rhs)]
    (with-meta (eval src) {:src src :vars vars})))

(defn rule->map [rule]
  (if (map? rule)
    rule
    (->> (partition 2 (partition-by keyword? rule))
         (reduce (fn [acc [[k] clause]]
                   (assoc acc k (vec clause))) {}))))

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
  (let [{:keys [then] [name] :name :as rule} (rule->map rule)]
    (cond-> (assoc rule :name (or name (d/squuid)))
      (sequential? then) (assoc :then (compile-rhs name then)))))

(defn format-rule [{:keys [name take drop then when]}]
  (vec (concat (cc/when name
                 [name (symbol "@")])
               take
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
                 (->> then meta :src last)))))

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
  (let [{:keys [name take drop when then]} (build-rule rule)
        head (position-constraints->datoms (concat take drop))
        head-vars (vec (distinct (map first head)))]
    ;; TODO: In flight attempt to filter out tried constraints.
    {:name name
     :lhs (vec (concat [:find (vec (concat head-vars
                                           (-> then meta :vars)))]
                       [:where]
                       head
                       (cc/when (> (count head-vars) 1)
                         [[(cons 'not= head-vars)]])
                       when))
     :rhs then
     :to-take (count take)
     :to-drop (count drop)}))

(defn run-rule [conn {:keys [lhs rhs to-take to-drop]} tried]
  (when-let [result (d/q lhs conn)]
    (let [[to-take result] (split-at to-take result)
          [to-drop args] (split-at to-drop result)]
      [to-take to-drop (some-> rhs (apply args))])))

(defn run
  ([conn all-rules]
   (run conn all-rules nil))
  ([conn all-rules max-runs]
   (let [all-rules (map rule->executable-rule all-rules)]
     (loop [[{:keys [name] :as rule} & rules] (shuffle all-rules) changes nil runs 0 tried {}]
       (let [[to-take to-drop to-add :as result] (run-rule @conn rule (tried name))
             tried (cond-> tried
                     result (update-in [name] (fnil conj #{}) (vec (concat to-take to-drop))))
             txs (concat (add-tx to-add) (retract-tx to-drop))
             {:keys [tx-data]} (d/transact! conn txs)
             changes (concat changes tx-data)]
         (if (or (and (nil? rules) (empty? changes))
                 (and max-runs (= runs max-runs)))
           conn
           (recur (or rules (shuffle all-rules))
                  (when rules
                    changes)
                  (inc runs)
                  tried)))))))

(defn run-once
  ([rules wm]
   (run-once rules wm nil))
  ([rules wm max-runs]
   @(doto (d/create-conn)
      (d/transact! (add-tx wm))
      (run rules max-runs))))

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

(->> (run-once gcd-rules #{[:gcd 9] [:gcd 6] [:gcd 3]})
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

(->> (run-once prime-rules #{[:upto 7]})
     constraints
     (= #{[:prime 7] [:prime 5] [:prime 3] [:prime 2]})
     assert)

(def fib-rules '[[:name fib
                  :take
                  [:upto ?max]
                  [:fib ?b ?bv]
                  :drop ;; TODO: this drop shouldn't be necessary, rules should only fire once per set of constraints.
                  [:fib ?a ?av]
                  :when
                  [(inc ?a) ?x]
                  [(= ?x ?b)]
                  [(< ?b ?max)]
                  :then
                  [:fib (inc ?b) (+ ?av ?bv)]]])

(->> (run-once fib-rules #{[:upto 5] [:fib 1 1] [:fib 2 1]})
     constraints
     (= #{[:upto 5] [:fib 5 5] [:fib 4 3]})
     assert)

;; http://chrjs.net/playground.html
