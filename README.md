
# Sanningens Silverflod

Constraint Handling Rules on top of DataScript.

```clojure
[[:drop [:gcd 0]]

 [:take [:gcd ?n]
  :drop [:gcd ?m]
  :when
  [(>= ?m ?n)]
  [(pos? ?n)]
  :then [:gcd (- ?m ?n)]]]

(->> #{[:gcd 9] [:gcd 6] [:gcd 3]}
     (run-once gcd-rules)
     constraints)
;=> #{[:gcd 3]}
```

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
