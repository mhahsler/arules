# Find Closed Itemsets

Provides the generic function and the method `is.closed()` for finding
closed itemsets. Closed itemsets are used as a concise representation of
frequent itemsets. The closure of an itemset is its largest proper
superset which has the same support (is contained in exactly the same
transactions). An itemset is closed, if it is its own closure (Pasquier
et al. 1999).

## Usage

``` r
is.closed(x)

# S4 method for class 'itemsets'
is.closed(x)
```

## Arguments

- x:

  a set of itemsets.

## Value

a logical vector with the same length as `x` indicating for each element
in `x` if it is a closed itemset.

## Details

Closed frequent itemsets can also be mined directly using
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md) or
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md) with
target `"closed frequent itemsets"`.

## References

Nicolas Pasquier, Yves Bastide, Rafik Taouil, and Lotfi Lakhal (1999).
Discovering frequent closed itemsets for association rules. In
*Proceeding of the 7th International Conference on Database Theory*,
Lecture Notes In Computer Science (LNCS 1540), pages 398–416. Springer,
1999.

## See also

Other postprocessing:
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemsets-class`](http://michael.hahsler.net/arules/reference/itemsets-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler
