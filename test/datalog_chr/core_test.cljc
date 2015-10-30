(ns datalog-chr.core-test
  (:require
    #?(:cljs [cljs.test :as t :refer-macros [is are deftest testing]]
       :clj  [clojure.test :as t :refer [is are deftest testing]])
    [datalog-chr.core :as chr]))

(def gcd '[[:drop [:gcd 0]]

           [:take [:gcd ?n]
            :drop [:gcd ?m]
            :when
            [(>= ?m ?n)]
            [(pos? ?n)]
            :then [:gcd (- ?m ?n)]]])

(def prime '[[:take [:prime ?i]
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

(def fib '[[:name fib
            :take
            [:upto ?max]
            [:fib ?a ?av]
            [:fib ?b ?bv]
            :when
            [(inc ?a) ?x]
            [(= ?x ?b)]
            [(< ?b ?max)]
            :then
            [:fib (inc ?b) (+ ?av ?bv)]]])

(deftest basic-rule-firing
  (are [rules wm expected] (= expected (chr/constraints @(chr/run-once rules wm)))

    gcd #{[:gcd 9] [:gcd 6] [:gcd 3]}
    #{[:gcd 3]}

    prime #{[:upto 7]}
    #{[:prime 7] [:prime 5] [:prime 3] [:prime 2]}

    fib #{[:upto 5] [:fib 1 1] [:fib 2 1]}
    #{[:upto 5] [:fib 5 5] [:fib 4 3] [:fib 3 2] [:fib 2 1] [:fib 1 1]}))

(deftest format-rules
  (are [rules expected] (= expected (mapv (comp chr/format-rule chr/rule->map) rules))

    gcd
    '[[[:gcd 0] <=> true]

      [[:gcd ?n] \\ [:gcd ?m] <=> [(>= ?m ?n)] [(pos? ?n)] | [:gcd (- ?m ?n)]]]

    prime
    '[[[:prime ?i] \\ [:prime ?j] <=> [(mod ?j ?i) ?mod] [(zero? ?mod)] | true]
      [[:upto 1] <=> true]
      [[:upto ?n] <=> [(> ?n 1)] | [:prime ?n] [:upto (dec ?n)]]]

    fib
    '[[fib \@ [:upto ?max] [:fib ?a ?av] [:fib ?b ?bv]
       ==> [(inc ?a) ?x] [(= ?x ?b)] [(< ?b ?max)] | [:fib (inc ?b) (+ ?av ?bv)]]]))
