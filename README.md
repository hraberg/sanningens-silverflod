# Sanningens Silverflod

*Allt genom lunden gröna, rinner det en flod* --
 [Träd, Gräs och Stenar, 1970](https://youtu.be/1sF2LdPyd7o)

### Constraint Handling Rules on top of DataScript.

*Experimental*

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

This project is related to
[eyvind](https://github.com/hraberg/eyvind), which is aimed to explore
(potential) distributed approaches, while this project leverages
[DataScript](https://github.com/tonsky/datascript) and focuses on
exploring usage, in-memory and concurrent execution.

The current implementation is pretty naive and single threaded. It's
also broken.


## References

### Intros to Constraint Handling Rules (CHR)

* https://dtai.cs.kuleuven.be/CHR/about.shtml
* http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/Papers/chr-lnai08.pdf
* http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/drafts/ruleML-keynote-chr-survey-15.pdf

### CHR Implementation Theory

* http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.132.9800&rep=rep1&type=pdf
* http://www.qatar.cmu.edu/~sllam/my_papers/published/SLLAM_tplp11.pdf
* https://lifeware.inria.fr/~tmartine/papers/martinez11chr.pdf

### Join Calculus

* http://www.qatar.cmu.edu/~sllam/my_papers/workshop/SLLAM_CHR_2008.pdf
* http://www.ccs.neu.edu/home/turon/scalable-joins.pdf

### CHR Implementations

* http://chrjs.net/
* https://github.com/takeoutweight/Clojure-CHR


## License

Copyright © 2015 Håkan Råberg

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
