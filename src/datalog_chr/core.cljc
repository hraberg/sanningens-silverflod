(ns datalog-chr.core
  (:require [clojure.core :as cc]
            [datascript.core :as d]))

(defn lvar? [v]
  (and (symbol? v) (= \? (first (name v)))))

(defn extract-lvars [x]
  (set (filter lvar? (flatten x))))

(defn compile-rhs [name rhs]
  (let [vars (vec (sort (extract-lvars rhs)))
        src `(~'fn ~(symbol (str (or name "rhs"))) [~@vars] ~rhs)]
    (with-meta (eval src) {:src src :vars vars})))

(defn rule->map [rule]
  (cond-> rule
    (not (map? rule)) (->> (partition-by keyword?)
                           (partition 2)
                           (reduce (fn [acc [[k] clause]]
                                     (assoc acc k (vec clause))) {}))))

(defn entity->constraint [e]
  (mapv e (sort (keys (dissoc e :db/id)))))

(defn constraint->entity [c]
  (zipmap (map #(keyword "chr" (str "at_" %)) (range)) c))

(defn entity->datoms [id e]
  (mapv (comp vec (partial cons id)) e))

(defn head-constraints->datoms [cs]
  (->> (vec cs)
       (reduce-kv (fn [acc idx c]
                    (->> (constraint->entity c)
                         (entity->datoms (symbol (str "?" idx)))
                         (concat acc)))
                  [])))

(defn build-rule [rule]
  (let [{:keys [then] [name] :name :as rule} (rule->map rule)]
    (cond-> (assoc rule :name (or name (d/squuid)))
      (sequential? then) (assoc :then (compile-rhs name then)))))

(defn format-rule [{:keys [take drop then when] [name] :name}]
  (vec (concat (cc/when name
                 [name \@])
               take
               (cc/when (and take drop)
                 [\\])
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

(defn constraints-not-tried? [tried-constraints & vars]
  (not (contains? tried-constraints (vec vars))))

(defn rule->executable-rule [rule]
  (let [{:keys [name take drop when then]} (build-rule rule)
        head (vec (head-constraints->datoms (concat take drop)))
        head-vars (vec (distinct (map first head)))
        not-tried-sym (gensym "?__constraints-not-tried?__")]
    {:name name
     :lhs (vec (concat [:find (vec (concat head-vars
                                           (-> then meta :vars)))]
                       [:in '$ not-tried-sym]
                       [:where]
                       head
                       [[(cons not-tried-sym head-vars)]]
                       (cc/when (> (count head-vars) 1)
                         [[(cons 'not= head-vars)]])
                       when))
     :rhs then
     :to-take (count take)
     :to-drop (count drop)}))

(defn run-rule [conn {:keys [lhs rhs to-take to-drop]} tried-constraints]
  ;; Potentially we want to reify info about which combinations has
  ;; been tried and maybe even the rules into the db itself.
  (when-let [result (d/q lhs conn (partial head-constraints-not-tried? tried-constraints))]
    (let [head-count (+ to-take to-drop)]
      [(subvec result 0 to-take)
       (subvec result to-take head-count)
       (some-> rhs (apply (subvec result head-count)))])))

(defn run
  ([conn all-rules]
   (run conn all-rules nil))
  ([conn all-rules max-runs]
   (let [all-rules (mapv rule->executable-rule all-rules)]
     (loop [[{:keys [name] :as rule} & rules] (shuffle all-rules) changes nil runs 0 tried-constraints {}]
       (let [[to-take to-drop to-add :as result] (run-rule @conn rule (tried-constraints name #{}))
             txs (concat (add-tx to-add) (retract-tx to-drop))
             {:keys [tx-data]} (d/transact! conn txs)
             changes (concat changes tx-data)]
         (if (or (and (nil? rules) (empty? changes))
                 (= runs max-runs))
           conn
           (recur (or rules (shuffle all-rules))
                  (when rules
                    changes)
                  (inc runs)
                  (cond-> tried-constraints
                    result (update-in [name] (fnil conj #{}) (vec (concat to-take to-drop)))))))))))

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
