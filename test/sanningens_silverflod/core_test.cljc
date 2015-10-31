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

;; /**
;;  *
;;  * Computes greatest common divisor by Euclidean algorithm.
;;  *
;;  * Example Usage:
;;  *   gcd(12), gcd(8)
;;  *
;;  */

;; cleanup @ gcd(0) <=> true
;; gcd(N) \ gcd(M) <=> 0 < N, N <= M | gcd(M % N)

;; /**
;;  *
;;  * Computes Fibonacci numbers by Bottom-Up Evaluation.
;;  *
;;  * Example Usage:
;;  *   fib(1,1), fib(2,1), upto(10)
;;  */

;; upto(N), fib(A,AV), fib(B,BV) ==>
;;   B === A+1, B < N | fib(B+1,AV+BV)

;; /**
;;  *
;;  * Generate prime numbers by Sieve of Eratosthenes.
;;  *
;;  * Example Usage:
;;  *   upto(12)
;;  */

;; gen   @ upto(N) <=> N > 1 | upto(N-1), prime(N)
;; sift  @ prime(X) \ prime(Y) <=> Y % X === 0 | true

;; /**
;;  *
;;  * Computes for a directed graph
;;  *   the shortest length for each path.
;;  *
;;  * Example Usage:
;;  *   example
;;  */

;; rem_long @ path(X,Y,L1) \ path(X,Y,L2)
;;   <=> L1 <= L2 | true
;; path_add @ path(X,Y,L1), path(Y,Z,L2)
;;   ==> X !== Z | path(X,Z,L1+L2)

;; example <=>
;;   path('London','Berlin',1100),
;;   path('Berlin','Vienna',650),
;;   path('Vienna','London',1500),
;;   path('Vienna','Paris',1200),
;;   path('Ulm','Vienna',600),
;;   path('Paris','Ulm',700)

;; /**
;;  *
;;  * Solves the Hamming Problem which is to build the
;;  *   infinite ascending sequence of positive numbers
;;  *   containing no prime factors other than 2, 3 and 5.
;;  *
;;  * Example Usage:
;;  *   succ(0,1), hamming(0), upto(50)
;;  */

;; succ(A,A) <=> true
;; succ(A,B) \ succ(A,C) <=> A < B, B <= C | succ(B,C)

;; upto(N), succ(S,X) \ hamming(S) <=> X < N |
;;   succ(X,2*X),
;;   succ(X,3*X),
;;   succ(X,5*X),
;;   hamming(X)
