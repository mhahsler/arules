# Association Rule Induction from Itemsets

Induces association
[rules](http://michael.hahsler.net/arules/reference/rules-class.md) that
can be generated from supplied
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md),
optionally using a
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
data set to recount support.

## Usage

``` r
ruleInduction(x, ...)

# S4 method for class 'itemsets'
ruleInduction(
  x,
  transactions = NULL,
  confidence = 0.8,
  method = c("ptree", "apriori"),
  reduce = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  the set of
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  from which rules will be induced.

- ...:

  unused; unknown arguments produce a warning.

- transactions:

  the
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  used to mine `x`. This can be omitted for `method = "ptree"` only when
  `x` is a complete collection of frequent itemsets with support values.

- confidence:

  numeric value in `[0, 1]` giving the minimum confidence threshold.

- method:

  induction method: `"ptree"` or `"apriori"`.

- reduce:

  logical; remove unused items before counting to reduce memory use and
  potentially improve speed?

- verbose:

  logical; report progress and timing information?

## Value

A [rules](http://michael.hahsler.net/arules/reference/rules-class.md)
object containing all induced rules meeting the confidence threshold.
Its quality data includes support, confidence, and lift; methods that
recount transactions can include an `itemset` index identifying the
source itemset.

## Details

All rules that can be created using the supplied itemsets and that
surpass the specified minimum confidence threshold are returned.
`ruleInduction()` can be used to produce closed association rules
defined by Pei et al. (2000) as rules `X => Y` where both `X` and `Y`
are closed frequent itemsets. See the code example in the Example
section.

Rule induction implements two methods. The default is `"ptree"`.

- `"ptree"` **method without transactions:** No transactions need to be
  specified if `x` contains a complete set of frequent itemsets. The
  itemsets' support counts are stored in a ptree and then retrieved to
  create rules and calculate confidence. This is very fast, but fails if
  support values are missing or `x` is not a complete set of frequent
  itemsets.

- `"ptree"` **method with transactions:** If transactions are specified
  then all transactions are counted into a prefix tree and later
  retrieved to create rules from the itemsets and calculate confidence
  values. This is slower, but necessary if `x` is not a complete set of
  frequent itemsets. To improve speed, unused items are removed from the
  transaction data before creating the prefix tree (this behavior can be
  changed using the argument `reduce`). This might be slower for large
  transaction data sets. However, this is highly recommended as the
  items are also reordered to reduce the counting time.

- `"apriori"` **method (always needs transactions):** All association
  rules are mined from the transactions data set using
  [`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
  with the smallest support found in the itemsets. In a second step, all
  rules which cannot be generated from one of the itemsets are removed.
  This procedure is very slow, especially for itemsets with many
  elements or very low support.

## References

Michael Hahsler, Christian Buchta, and Kurt Hornik. Selective
association rule generation. *Computational Statistics,* 23(2):303-315,
April 2008.

Jian Pei, Jiawei Han, Runying Mao. CLOSET: An Efficient Algorithm for
Mining Frequent Closed Itemsets. *ACM SIGMOD Workshop on Research Issues
in Data Mining and Knowledge Discovery (DMKD 2000).*

## See also

Other postprocessing:
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md)

## Author

Christian Buchta and Michael Hahsler

## Examples

``` r
data("Adult")

## find all closed frequent itemsets
closed_is <- apriori(Adult, target = "closed frequent itemsets", support = 0.4)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>          NA    0.1    1 none FALSE            TRUE       5     0.4      1
#>  maxlen                   target  ext
#>      10 closed frequent itemsets TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 19536 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.03s].
#> sorting and recoding items ... [11 item(s)] done [0.00s].
#> creating transaction tree ... done [0.01s].
#> checking subsets of size 1 2 3 4 5 done [0.00s].
#> filtering closed item sets ... done [0.00s].
#> sorting transactions ... done [0.01s].
#> writing ... [99 set(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
closed_is
#> set of 99 itemsets 

## use rule induction to produce all closed association rules
closed_rules <- ruleInduction(closed_is, transactions = Adult, verbose = TRUE)
#> ruleInduction: using method ptree 
#> preparing ... 593 itemsets, created 203 (0.20) nodes [0.00s]
#> counting ... 48842 transactions, processed 9285411 (0.31) nodes [0.03s]
#> writing ... 247 rules, processed 1433 (0.70) nodes [0.00s]
#> searching done [0.038s].
#> postprocessing done [0s].

## inspect the resulting closed rules
summary(closed_rules)
#> set of 165 rules
#> 
#> rule length distribution (lhs + rhs):sizes
#>  2  3  4  5 
#> 40 74 43  8 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.000   3.000   3.000   3.115   4.000   5.000 
#> 
#> summary of quality measures:
#>     support         confidence          lift           itemset     
#>  Min.   :0.4013   Min.   :0.8209   Min.   :0.9594   Min.   :12.00  
#>  1st Qu.:0.4341   1st Qu.:0.8871   1st Qu.:0.9912   1st Qu.:47.00  
#>  Median :0.4994   Median :0.9129   Median :0.9984   Median :70.00  
#>  Mean   :0.5333   Mean   :0.9121   Mean   :1.0409   Mean   :65.33  
#>  3rd Qu.:0.5697   3rd Qu.:0.9471   3rd Qu.:1.0169   3rd Qu.:87.00  
#>  Max.   :0.8707   Max.   :0.9999   Max.   :2.4529   Max.   :99.00  
#> 
#> mining info:
#>   data ntransactions support confidence
#>  Adult         48842     0.4        0.8
#>                                                                       call
#>  apriori(data = Adult, target = "closed frequent itemsets", support = 0.4)
inspect(head(closed_rules, by = "lift"))
#>     lhs                                     rhs                                   support confidence     lift itemset
#> [1] {marital-status=Married-civ-spouse,                                                                              
#>      sex=Male}                           => {relationship=Husband}              0.4034028  0.9901503 2.452877      47
#> [2] {relationship=Husband}               => {marital-status=Married-civ-spouse} 0.4034233  0.9993914 2.181164      12
#> [3] {marital-status=Married-civ-spouse}  => {relationship=Husband}              0.4034233  0.8804683 2.181164      12
#> [4] {relationship=Husband,                                                                                           
#>      sex=Male}                           => {marital-status=Married-civ-spouse} 0.4034028  0.9993913 2.181164      47
#> [5] {relationship=Husband}               => {sex=Male}                          0.4036485  0.9999493 1.495851      13
#> [6] {marital-status=Married-civ-spouse,                                                                              
#>      relationship=Husband}               => {sex=Male}                          0.4034028  0.9999492 1.495851      47

## get rules from frequent itemsets. Here, transactions does not need to be
## specified for rule induction.
frequent_is <- eclat(Adult, support = 0.4)
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.4      1     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 19536 
#> 
#> create itemset ... 
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.03s].
#> sorting and recoding items ... [11 item(s)] done [0.00s].
#> creating bit matrix ... [11 row(s), 48842 column(s)] done [0.00s].
#> writing  ... [99 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].
assoc_rules <- ruleInduction(frequent_is)
assoc_rules
#> set of 165 rules 
inspect(head(assoc_rules))
#>     lhs                                     rhs                                   support confidence     lift
#> [1] {relationship=Husband,                                                                                   
#>      sex=Male}                           => {marital-status=Married-civ-spouse} 0.4034028  0.9993913 2.181164
#> [2] {marital-status=Married-civ-spouse,                                                                      
#>      sex=Male}                           => {relationship=Husband}              0.4034028  0.9901503 2.452877
#> [3] {marital-status=Married-civ-spouse,                                                                      
#>      relationship=Husband}               => {sex=Male}                          0.4034028  0.9999492 1.495851
#> [4] {relationship=Husband}               => {sex=Male}                          0.4036485  0.9999493 1.495851
#> [5] {relationship=Husband}               => {marital-status=Married-civ-spouse} 0.4034233  0.9993914 2.181164
#> [6] {marital-status=Married-civ-spouse}  => {relationship=Husband}              0.4034233  0.8804683 2.181164

## for itemsets that are not a complete set of frequent itemsets,
## transactions need to be specified.
some_is <- sample(frequent_is, 10)
some_rules <- ruleInduction(some_is, transactions = Adult)
some_rules
#> set of 19 rules 
```
