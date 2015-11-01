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
* http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.132.9800&rep=rep1&type=pdf
* http://www.qatar.cmu.edu/~sllam/my_papers/published/SLLAM_tplp11.pdf
* http://www.qatar.cmu.edu/~sllam/my_papers/workshop/SLLAM_CHR_2008.pdf
* http://www.ccs.neu.edu/home/turon/scalable-joins.pdf

* http://chrjs.net/
* https://github.com/takeoutweight/Clojure-CHR

## License

Copyright © 2015 Håkan Råberg

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
