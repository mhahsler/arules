# Find Generator Itemsets

Provides the generic function and the method \`is.generator() for
finding generator itemsets. Generators are part of concise
representations for frequent itemsets. A generator in a set of itemsets
is an itemset that has no subset with the same support (Liu et al,
2008). Note that the empty set is by definition a generator, but it is
typically not stored in the itemsets in arules.

## Usage

``` r
is.generator(x)

# S4 method for class 'itemsets'
is.generator(x)
```

## Arguments

- x:

  a set of itemsets.

## Value

a logical vector with the same length as `x` indicating for each element
in `x` if it is a generator itemset.

## References

Yves Bastide, Niolas Pasquier, Rafik Taouil, Gerd Stumme, Lotfi Lakhal
(2000). Mining Minimal Non-redundant Association Rules Using Frequent
Closed Itemsets. In *International Conference on Computational Logic*,
Lecture Notes in Computer Science (LNCS 1861). pages 972–986.
[doi:10.1007/3-540-44957-4_65](https://doi.org/10.1007/3-540-44957-4_65)

Guimei Liu, Jinyan Li, Limsoon Wong (2008). A new concise representation
of frequent itemsets using generators and a positive border. *Knowledge
and Information Systems* 17(1):35-56.
[doi:10.1007/s10115-007-0111-5](https://doi.org/10.1007/s10115-007-0111-5)

## See also

Other postprocessing:
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
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
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
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

## Examples

``` r
# Example from Liu et al (2008)
trans_list <- list(
  t1 = c("a", "b", "c"),
  t2 = c("a", "b", "c", "d"),
  t3 = c("a", "d"),
  t4 = c("a", "c")
)

trans <- transactions(trans_list)
its <- apriori(trans, support = 1 / 4, target = "frequent itemsets")
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>          NA    0.1    1 none FALSE            TRUE       5    0.25      1
#>  maxlen            target  ext
#>      10 frequent itemsets TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 1 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[4 item(s), 4 transaction(s)] done [0.00s].
#> sorting and recoding items ... [4 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> sorting transactions ... done [0.00s].
#> writing ... [15 set(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].

is.generator(its)
#>       {d}       {b}       {c}       {a}     {b,d}     {c,d}     {a,d}     {b,c} 
#>      TRUE      TRUE      TRUE     FALSE      TRUE      TRUE     FALSE     FALSE 
#>     {a,b}     {a,c}   {b,c,d}   {a,b,d}   {a,c,d}   {a,b,c} {a,b,c,d} 
#>     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE 
```
