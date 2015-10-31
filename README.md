# Sanningens Silverflod

*Allt genom lunden gröna, rinner det en flod* -- [Träd, Gräs och Stenar, 1970](https://youtu.be/1sF2LdPyd7o)

### Constraint Handling Rules on top of DataScript.

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


## References

* http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/Papers/chr-lnai08.pdf
* https://lifeware.inria.fr/~tmartine/papers/martinez11chr.pdf


## License

Copyright © 2015 Håkan Råberg

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
