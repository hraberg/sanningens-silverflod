(ns sanningens-silverflod.core-test
  (:require
    #?(:cljs [cljs.test :as t :refer-macros [is are deftest testing]]
       :clj  [clojure.test :as t :refer [is are deftest testing]])
    [sanningens-silverflod.core :as chr]))

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
  (are [rules expected] (= expected (mapv (comp chr/rule-map->chr chr/parse-rule->rule-map) rules))

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

;; The examples from http://chrjs.net/playground.html (MIT License)

(deftest chr-js-playground-examples
  (are [rules wm expected] (= expected (chr/constraints @(chr/run-once rules wm)))

    '[;; cleanup @ gcd(0) <=> true
      [:name cleanup
       :drop [:gcd 0]]

      ;; gcd(N) \ gcd(M) <=> 0 < N, N <= M | gcd(M % N)
      [:take [:gcd ?n]
       :drop [:gcd ?m]
       :when
       [(pos? ?n)]
       [(<= ?n ?m)]
       :then
       [:gcd (mod ?m ?n)]]]

    #{[:gcd 12] [:gcd 8]}
    #{[:gcd 4]}

    '[;; upto(N), fib(A,AV), fib(B,BV) ==>
      ;;   B === A+1, B < N | fib(B+1,AV+BV)
      [:take
       [:upto ?n]
       [:fib ?a ?av]
       [:fib ?b ?bv]
       :when
       [(inc ?a) ?x]
       [(= ?x ?b)]
       [(< ?b ?n)]
       :then
       [:fib (inc ?b) (+ ?av ?bv)]]]

    #{[:fib 1 1] [:fib 2 1] [:upto 10]}
    #{[:upto 10] [:fib 1 1] [:fib 2 1] [:fib 3 2] [:fib 4 3] [:fib 5 5]
      [:fib 6 8] [:fib 7 13] [:fib 8 21] [:fib 9 34] [:fib 10 55]}

    '[;; gen   @ upto(N) <=> N > 1 | upto(N-1), prime(N)
      [:name gen
       :drop [:upto ?n]
       :when [(> ?n 1)]
       :then
       [:upto (dec ?n)]
       [:prime ?n]]

      ;; sift  @ prime(X) \ prime(Y) <=> Y % X === 0 | true
      [:name sift
       :take [:prime ?x]
       :drop [:prime ?y]
       :when
       [(mod ?y ?x) ?mod]
       [(zero? ?mod)]]]

    #{[:upto 12]}
    #{[:upto 1] [:prime 2] [:prime 3] [:prime 5] [:prime 7] [:prime 11]}

    '[;; rem_long @ path(X,Y,L1) \ path(X,Y,L2)
      ;;   <=> L1 <= L2 | true
      [:name rem-long
       :take [:path ?x ?y ?l1]
       :drop [:path ?x ?y ?l2]
       :when [(<= ?l1 ?l2)]]

      ;; path_add @ path(X,Y,L1), path(Y,Z,L2)
      ;;   ==> X !== Z | path(X,Z,L1+L2)
      [:name path-add
       :take
       [:path ?x ?y ?l1]
       [:path ?y ?z ?l2]
       :when
       [(not= ?x ?z)]
       :then
       [:path ?x ?z (+ ?l1 ?l2)]]]

    #{[:path "London" "Berlin" 1100]
      [:path "Berlin" "Vienna" 650]
      [:path "Vienna" "London" 1500]
      [:path "Vienna" "Paris" 1200]
      [:path "Ulm" "Vienna" 600]
      [:path "Paris" "Ulm" 700]}
	#{[:path "London" "Berlin" 1100]
      [:path "Berlin" "Vienna" 650]
      [:path "London" "Vienna" 1750]
      [:path "Vienna" "London" 1500]
      [:path "Berlin" "London" 2150]
      [:path "Vienna" "Berlin" 2600]
      [:path "Vienna" "Paris" 1200]
      [:path "Berlin" "Paris" 1850]
      [:path "London" "Paris" 2950]
      [:path "Ulm" "Vienna" 600]
      [:path "Ulm" "London" 2100]
      [:path "Ulm" "Berlin" 3200]
      [:path "Ulm" "Paris" 1800]
      [:path "Paris" "Ulm" 700]
      [:path "Vienna" "Ulm" 1900]
      [:path "Berlin" "Ulm" 2550]
      [:path "London" "Ulm" 3650]
      [:path "Paris" "Vienna" 1300]
      [:path "Paris" "London" 2800]
      [:path "Paris" "Berlin" 3900]}

    ;; '[;; succ(A,A) <=> true
    ;;   [:drop [:succ ?a ?a]]

    ;;   ;; succ(A,B) \ succ(A,C) <=> A < B, B <= C | succ(B,C)
    ;;   [:take [:succ ?a ?b]
    ;;    :drop [:succ ?a ?c]
    ;;    :when
    ;;    [(< ?a ?b)]
    ;;    [(<= ?b ?c)]
    ;;    :then
    ;;    [:succ ?b ?c]]

    ;;   ;; upto(N), succ(S,X) \ hamming(S) <=> X < N |
    ;;   ;;   succ(X,2*X),
    ;;   ;;   succ(X,3*X),
    ;;   ;;   succ(X,5*X),
    ;;   ;;   hamming(X)

    ;;   [:take
    ;;    [:upto ?n]
    ;;    [:succ ?s ?x]
    ;;    :drop
    ;;    [:hamming ?s]
    ;;    :when
    ;;    [(< ?x ?n)]
    ;;    :then
    ;;    [:succ ?x (* 2 ?x)]
    ;;    [:succ ?x (* 3 ?x)]
    ;;    [:succ ?x (* 5 ?x)]
    ;;    [:hamming ?x]]]

    ;; #{[:succ 0 1] [:hamming 0] ["upto" 50]}
    ;; #{[:succ 0 1]
    ;;   [:upto 50]
    ;;   [:succ 1 2]
    ;;   [:succ 2 3]
    ;;   [:succ 3 4]
    ;;   [:succ 4 5]
    ;;   [:succ 5 6]
    ;;   [:succ 9 10]
    ;;   [:succ 6 8]
    ;;   [:succ 8 9]
    ;;   [:succ 10 12]
    ;;   [:succ 12 15]
    ;;   [:succ 18 20]
    ;;   [:succ 15 16]
    ;;   [:succ 16 18]
    ;;   [:succ 20 24]
    ;;   [:succ 24 25]
    ;;   [:succ 25 27]
    ;;   [:succ 27 30]
    ;;   [:succ 40 45]
    ;;   [:succ 36 40]
    ;;   [:succ 30 32]
    ;;   [:succ 32 36]
    ;;   [:succ 45 48]
    ;;   [:succ 48 50]
    ;;   [:succ 75 80]
    ;;   [:succ 50 54]
    ;;   [:succ 54 60]
    ;;   [:succ 72 75]
    ;;   [:succ 120 125]
    ;;   [:succ 80 81]
    ;;   [:succ 81 90]
    ;;   [:succ 125 135]
    ;;   [:succ 60 64]
    ;;   [:succ 64 72]
    ;;   [:succ 90 96]
    ;;   [:succ 96 100]
    ;;   [:succ 150 160]
    ;;   [:succ 100 108]
    ;;   [:succ 108 120]
    ;;   [:succ 160 180]
    ;;   [:succ 180 200]
    ;;   [:succ 200 225]
    ;;   [:succ 135 144]
    ;;   [:succ 144 150]
    ;;   [:succ 225 240]
    ;;   [:hamming 48]}
))
