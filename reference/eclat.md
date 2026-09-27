# Mining Associations with Eclat

Mine frequent itemsets with the Eclat algorithm. This algorithm uses
simple intersection operations for equivalence class clustering along
with bottom-up lattice traversal.

## Usage

``` r
eclat(data, parameter = NULL, control = NULL, ...)
```

## Arguments

- data:

  object of class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).
  Any data structure which can be coerced into transactions (e.g., a
  logical matrix, a data.frame or a tibble) can also be specified and
  will be internally coerced to transactions. However, it is recommended
  to first create a transactions object using
  [`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  and then to check that items are correctly created.

- parameter:

  object of class
  [ECparameter](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  or named list (default values are: support 0.1 and maxlen 5)

- control:

  object of class
  [ECcontrol](http://michael.hahsler.net/arules/reference/AScontrol-classes.md)
  or named list for algorithmic controls.

- ...:

  Additional arguments are added for convenience to the parameter list.

## Value

Returns an object of class
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md).

## Details

Calls the C implementation of the Eclat algorithm by Christian Borgelt
for mining frequent itemsets.

Eclat can also return the transaction IDs for each found itemset using
`tidLists = TRUE` as a parameter and the result can be retrieved as a
[tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md)
object with method
[`tidLists()`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
for class
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md).
Note that storing transaction ID lists is very memory intensive,
creating transaction ID lists only works for minimum support values
which create a relatively small number of itemsets. See also
[`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md).

[`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)
can be used to generate rules from the found itemsets.

A weighted version of ECLAT is available as function
[`weclat()`](http://michael.hahsler.net/arules/reference/weclat.md).
This version can be used to perform weighted association rule mining
(WARM).

## References

Mohammed J. Zaki, Srinivasan Parthasarathy, Mitsunori Ogihara, and Wei
Li. (1997) *New algorithms for fast discovery of association rules*.
KDD'97: Proceedings of the Third International Conference on Knowledge
Discovery and Data Mining, August 1997, Pages 283-286.

Christian Borgelt (2003) Efficient Implementations of Apriori and Eclat.
*Workshop of Frequent Item Set Mining Implementations* (FIMI 2003,
Melbourne, FL, USA).

ECLAT Implementation: <https://borgelt.net/eclat.html>

## See also

Other mining algorithms:
[`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md),
[`AScontrol-classes`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md),
[`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md),
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md),
[`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md)

## Author

Michael Hahsler and Bettina Gruen

## Examples

``` r
data("Adult")
## Mine itemsets with minimum support of 0.1 and 5 or less items
itemsets <- eclat(Adult,
  parameter = list(supp = 0.1, maxlen = 5)
)
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.1      1      5 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 4884 
#> 
#> create itemset ... 
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [31 item(s)] done [0.00s].
#> creating bit matrix ... [31 row(s), 48842 column(s)] done [0.00s].
#> writing  ... [2143 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].
itemsets
#> set of 2143 itemsets 

## Create rules from the frequent itemsets
rules <- ruleInduction(itemsets, confidence = .9)
rules
#> set of 2729 rules 
```
