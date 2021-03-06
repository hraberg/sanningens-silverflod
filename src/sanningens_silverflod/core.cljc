(ns sanningens-silverflod.core
  (:require [clojure.core :as cc]
            [datascript.core :as d]))

(defn lvar? [v]
  (and (symbol? v) (= \? (first (name v)))))

(defn lvar [name]
  (symbol (str \? name)))

(defn extract-lvars [x]
  (set (filter lvar? (flatten x))))

(def builtins '[unify])

(defn compile-rhs [name rhs]
  (let [vars (vec (sort (extract-lvars rhs)))
        src `(~'fn ~(symbol (str (or name "rhs"))) [~@vars] ~rhs)]
    (with-meta (eval `(do (require ['~'sanningens-silverflod.core :refer '~builtins])
                          ~src)) {:src src :vars vars})))

(defn parse-rule->rule-map [rule]
  (->> (partition-by keyword? rule)
       (partition 2)
       (reduce (fn [acc [[k] clause]]
                 (assoc acc k (cond-> (vec clause)
                                (= :name k) first))) {})))

(defn entity->constraint [e]
  (let [ks (keys (dissoc e :db/id))]
    (vec (cons (keyword (namespace (first ks)))
               (remove #{::no-attributes} (map e (sort ks)))))))

(defn constraint-position-attribute [name idx]
  (keyword (cc/name name) (str "at_" idx)))

(defn constraint->entity [[name & values]]
  (zipmap (map (partial constraint-position-attribute name) (range))
          (cond->> values
            (empty? values) (cons ::no-attributes))))

(defn entity->datoms [id e]
  (mapv (comp vec (partial cons id)) e))

(defn head-constraints->datoms [cs]
  (->> (vec cs)
       (reduce-kv (fn [acc idx c]
                    (->> (constraint->entity c)
                         (entity->datoms (lvar idx))
                         (concat acc))) [])))

(defn rule-map->chr [{:keys [take drop then when name]}]
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
               (if (vector? then)
                 then
                 [true]))))

(declare lvar-equals lvar-value)

;; TODO: these LVars are a hack which modify the store without
;; asserting new facts by updating the atom. Inspired by
;; http://yieldprolog.sourceforge.net/
(deftype LVar [name value]
  #?@(:cljs
       [IDeref
        (-deref [this]
               (lvar-value this))

        IEquiv
        (-equiv [this other]
                (lvar-equals this other))])

  #?@(:clj
       [clojure.lang.IDeref
        (deref [this]
               (lvar-value this))

        Object
        (equals [this other]
                (lvar-equals this other))]))

(defn lvar-chain [x]
  (if (and (instance? LVar x)
           (instance? LVar @(.value ^LVar x)))
    (recur @(.value ^LVar x))
    x))

(defn lvar-value [x]
  (let [v (lvar-chain x)]
    (if (instance? LVar v)
      @(.value ^LVar v)
      v)))

(defn lvar-equals [x y]
  (= (lvar-value x) (lvar-value y)))

(defn new-lvar
  ([name]
   (new-lvar name ::unbound))
  ([name value]
    (->LVar name (atom value))))

(defn unbound-lvar? [x]
  (and (instance? LVar x)
       (= ::unbound @(.value ^LVar x))))

(defn unify [x y]
  (let [x (lvar-chain x)]
    (if (unbound-lvar? x)
      (reset! (.value ^LVar x) y)
      (unify y x)))
  nil)

(defn replace-lvars [entity]
  (let [vars (extract-lvars entity)
        vars (zipmap vars (map #(new-lvar %) vars))]
    (replace vars entity)))

(defn add-tx [to-add]
  (mapv (fn [entity id]
          (cond->> (replace-lvars entity)
            (vector? entity) constraint->entity))
        to-add (range)))

(defn retract-tx [to-drop]
  (mapv (partial vector :db.fn/retractEntity) to-drop))

(defn constraints-not-tried? [tried-constraints & vars]
  (not (contains? tried-constraints (vec vars))))

(defn rule-map->executable-rule [{:keys [name take drop when then]}]
  (let [head (vec (head-constraints->datoms (concat take drop)))
        head-vars (vec (distinct (map first head)))
        not-tried-sym (gensym (lvar "constraints-not-tried?"))
        rhs (compile-rhs name then)]
    {:name (or name (d/squuid))
     :lhs (vec (concat [:find (vec (concat head-vars (-> rhs meta :vars)))]
                       [:in '$ not-tried-sym]
                       [:where]
                       (reverse head)
                       [[(cons not-tried-sym head-vars)]]
                       (cc/when (> (count head-vars) 1)
                         [[(cons 'not= head-vars)]])
                       when))
     :rhs rhs
     :head (vec (vals (group-by first head)))
     :to-take (count take)
     :to-drop (count drop)}))

(defn constraints [db]
  (->> (d/q '[:find [(pull ?e [*]) ...] :where [?e]] db)
       (map entity->constraint)
       set))

(defn matches-head? [db head]
  (d/q (concat [:find (ffirst head)'. :where] head) db))

(defn rules-matching-some-constraint [db rules]
  (for [{:keys [head] :as rule} rules
        :when (some (partial matches-head? db) head)]
    rule))

(defn new-datoms [tx-data]
  (mapv seq (filter last tx-data)))

(defn id->constraint [db id]
  (entity->constraint (d/pull db '[*] id)))

(defn ensure-constraints-exist [db head]
  (when-let [missing (seq (for [constraint head
                                :when (not (d/pull db '[:db/id] constraint))]
                            constraint))]
    (throw (ex-info "Missing constraints:" {:missing (vec missing)}))))

;; Potentially we want to reify info about which combinations has
;; been tried and maybe even the rules into the db itself.
;; not and not-join are to be supported in datascript.query-v3
(defn run-rule [db {:keys [lhs rhs to-take to-drop]} tried-constraints]
  (when-let [result (d/q lhs db (partial constraints-not-tried? tried-constraints))]
    (let [head-count (+ to-take to-drop)]
      {:to-take (subvec result 0 to-take)
       :to-drop (subvec result to-take head-count)
       :to-add (remove nil? (some-> rhs (apply (subvec result head-count))))})))

(defn run
  ([conn all-rules]
   (run conn all-rules nil))
  ([conn all-rules max-runs]
   (let [all-rules (mapv (comp rule-map->executable-rule parse-rule->rule-map) all-rules)]
     (loop [[{:keys [name] :as rule} & rules] (rules-matching-some-constraint @conn all-rules)
            tried-constraints {}
            runs 0]
       (let [{:keys [to-take to-drop to-add] :as result} (run-rule @conn rule (tried-constraints name #{}))
             head (vec (concat to-take to-drop))
             txs (concat [[:db.fn/call ensure-constraints-exist head]]
                         (add-tx to-add) (retract-tx to-drop))
             {:keys [tx-data]} (d/transact! conn txs)]
          (if (or (and (nil? rules) (empty? tx-data))
                 (= runs max-runs))
           conn
           (recur (or (concat (rules-matching-some-constraint (new-datoms tx-data) all-rules) rules)
                      (rules-matching-some-constraint @conn all-rules))
                  (cond-> tried-constraints
                    result (update-in [name] (fnil conj #{}) head))
                  (inc runs))))))))

(defn run-once
  ([rules wm]
   (run-once rules wm nil))
  ([rules wm max-runs]
   (doto (d/create-conn)
     (d/transact! (add-tx wm))
     (run rules max-runs))))
