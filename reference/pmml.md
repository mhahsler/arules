# Read and Write PMML

Reads PMML AssociationModel files and writes PMML representations of
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
([itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
and
[rules](http://michael.hahsler.net/arules/reference/rules-class.md)).
Writing delegates to
[`pmml::pmml()`](https://rdrr.io/pkg/pmml/man/pmml.html), which
determines the PMML version (currently PMML 4.4).

## Usage

``` r
write.PMML(x, file)

read.PMML(file)
```

## Arguments

- x:

  a [rules](http://michael.hahsler.net/arules/reference/rules-class.md)
  or
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  object.

- file:

  path to a PMML file.

## References

PMML 4.4 - Association Rules.
<https://dmg.org/pmml/v4-4/AssociationRules.html>

## See also

[`pmml::pmml()`](https://rdrr.io/pkg/pmml/man/pmml.html).

Other import/export:
[`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md),
[`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md),
[`read`](http://michael.hahsler.net/arules/reference/read.md),
[`write()`](http://michael.hahsler.net/arules/reference/write.md)

## Author

Michael Hahsler

## Examples

``` r
data("Groceries")

rules <- apriori(Groceries, parameter = list(support = 0.001))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5   0.001      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 9 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
#> sorting and recoding items ... [157 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 done [0.01s].
#> writing ... [410 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
rules <- head(rules, by = "lift")
rules
#> set of 6 rules 

### save rules as PMML
write.PMML(rules, file = "rules.xml")
#> [1] "rules.xml"

### read rules back
rules2 <- read.PMML("rules.xml")
rules2
#> set of 6 rules 

### compare rules
inspect(rules[1])
#>     lhs                         rhs            support     confidence
#> [1] {liquor, red/blush wine} => {bottled beer} 0.001931876 0.9047619 
#>     coverage    lift     count
#> [1] 0.002135231 11.23527 19   
inspect(rules2[1])
#>     lhs                         rhs            support     confidence lift    
#> [1] {liquor, red/blush wine} => {bottled beer} 0.001931876 0.9047619  11.23527

### clean up
unlink("rules.xml")
```
