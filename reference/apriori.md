# Mining Associations with the Apriori Algorithm

Mine frequent itemsets, association rules or association hyperedges
using the Apriori algorithm.

## Usage

``` r
apriori(data, parameter = NULL, appearance = NULL, control = NULL, ...)
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
  [APparameter](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  or named list. The default behavior is to mine rules with minimum
  support of 0.1, minimum confidence of 0.8, maximum of 10 items
  (maxlen), and a maximal time for subset checking of 5 seconds
  (`maxtime`).

- appearance:

  object of class
  [APappearance](http://michael.hahsler.net/arules/reference/APappearance-class.md)
  or named list. With this argument item appearance can be restricted
  (implements rule templates). By default all items can appear
  unrestricted.

- control:

  object of class
  [APcontrol](http://michael.hahsler.net/arules/reference/AScontrol-classes.md)
  or named list. Controls the algorithmic performance of the mining
  algorithm (item sorting, report progress (verbose), etc.)

- ...:

  Additional arguments are for convenience added to the parameter list.

## Value

Returns an object of class
[rules](http://michael.hahsler.net/arules/reference/rules-class.md) or
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md).

## Details

The Apriori algorithm (Agrawal et al, 1993) employs level-wise search
for frequent itemsets. The used C implementation of Apriori by Christian
Borgelt (2003) includes some improvements (e.g., a prefix tree and item
sorting).

**Warning about automatic conversion of matrices or data.frames to
transactions.** It is preferred to create transactions manually before
calling `apriori()` to have control over item coding. This is especially
important when you are working with multiple datasets or several subsets
of the same dataset. To read about item coding, see
[itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md).

If a data.frame is specified as `x`, then the data is automatically
converted into transactions by discretizing numeric data using
[`discretizeDF()`](http://michael.hahsler.net/arules/reference/discretize.md)
and then coercion to transactions. The discretization may fail if the
data is not well behaved.

**Apriori only creates rules with one item in the RHS (Consequent).**
The default value in
[APparameter](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
for `minlen` is 1. This meains that rules with only one item (i.e., an
empty antecedent/LHS) like

\$\$\\\\ =\> \\beer\\\$\$

will be created. These rules mean that no matter what other items are
involved, the item in the RHS will appear with the probability given by
the rule's confidence (which equals the support). If you want to avoid
these rules then use the argument `parameter = list(minlen = 2)`.

**Notes on run time and memory usage:** If the minimum `support` is
chosen too low for the dataset, then the algorithm will try to create an
extremely large set of itemsets/rules. This will result in very long run
time and eventually the process will run out of memory. To prevent this,
the default maximal length of itemsets/rules is restricted to 10 items
(via the parameter element `maxlen = 10`) and the time for checking
subsets is limited to 5 seconds (via `maxtime = 5`). The output will
show if you hit these limits in the "checking subsets" line of the
output. The time limit is only checked when the subset size increases,
so it may run significantly longer than what you specify in maxtime.
Setting `maxtime = 0` disables the time limit.

Interrupting execution with `Control-C/Esc` is not recommended. Memory
cleanup will be prevented resulting in a memory leak. Also, interrupts
are only checked when the subset size increases, so it may take some
time till the execution actually stops.

## References

R. Agrawal, T. Imielinski, and A. Swami (1993) Mining association rules
between sets of items in large databases. In *Proceedings of the ACM
SIGMOD International Conference on Management of Data*, pages 207–216,
Washington D.C.
[doi:10.1145/170035.170072](https://doi.org/10.1145/170035.170072)

Christian Borgelt (2012) Frequent Item Set Mining. *Wiley
Interdisciplinary Reviews: Data Mining and Knowledge Discovery*
2(6):437-456. J. Wiley & Sons, Chichester, United Kingdom 2012.
[doi:10.1002/widm.1074](https://doi.org/10.1002/widm.1074)

Christian Borgelt and Rudolf Kruse (2002) Induction of Association
Rules: Apriori Implementation. *15th Conference on Computational
Statistics* (COMPSTAT 2002, Berlin, Germany) Physica Verlag, Heidelberg,
Germany.

Christian Borgelt (2003) Efficient Implementations of Apriori and Eclat.
*Workshop of Frequent Item Set Mining Implementations* (FIMI 2003,
Melbourne, FL, USA).

APRIORI Implementation: <https://borgelt.net/apriori.html>

## See also

Other mining algorithms:
[`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md),
[`AScontrol-classes`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md),
[`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md),
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md),
[`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md)

## Author

Michael Hahsler and Bettina Gruen

## Examples

``` r

## Example 1: Create transaction data and mine association rules
a_list <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("c", "e"),
  c("a", "b", "d", "e")
)

## Set transaction names
names(a_list) <- paste("Tr", c(1:5), sep = "")
a_list
#> $Tr1
#> [1] "a" "b" "c"
#> 
#> $Tr2
#> [1] "a" "b"
#> 
#> $Tr3
#> [1] "a" "b" "d"
#> 
#> $Tr4
#> [1] "c" "e"
#> 
#> $Tr5
#> [1] "a" "b" "d" "e"
#> 

## Use the constructor to create transactions
trans1 <- transactions(a_list)
trans1
#> transactions in sparse format with
#>  5 transactions (rows) and
#>  5 items (columns)

rules <- apriori(trans1)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 0 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[5 item(s), 5 transaction(s)] done [0.00s].
#> sorting and recoding items ... [5 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> writing ... [19 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
inspect(rules)
#>      lhs          rhs support confidence coverage lift count
#> [1]  {}        => {b} 0.8     0.8        1.0      1.00 4    
#> [2]  {}        => {a} 0.8     0.8        1.0      1.00 4    
#> [3]  {d}       => {b} 0.4     1.0        0.4      1.25 2    
#> [4]  {d}       => {a} 0.4     1.0        0.4      1.25 2    
#> [5]  {b}       => {a} 0.8     1.0        0.8      1.25 4    
#> [6]  {a}       => {b} 0.8     1.0        0.8      1.25 4    
#> [7]  {b, c}    => {a} 0.2     1.0        0.2      1.25 1    
#> [8]  {a, c}    => {b} 0.2     1.0        0.2      1.25 1    
#> [9]  {d, e}    => {b} 0.2     1.0        0.2      1.25 1    
#> [10] {b, e}    => {d} 0.2     1.0        0.2      2.50 1    
#> [11] {d, e}    => {a} 0.2     1.0        0.2      1.25 1    
#> [12] {a, e}    => {d} 0.2     1.0        0.2      2.50 1    
#> [13] {b, e}    => {a} 0.2     1.0        0.2      1.25 1    
#> [14] {a, e}    => {b} 0.2     1.0        0.2      1.25 1    
#> [15] {b, d}    => {a} 0.4     1.0        0.4      1.25 2    
#> [16] {a, d}    => {b} 0.4     1.0        0.4      1.25 2    
#> [17] {b, d, e} => {a} 0.2     1.0        0.2      1.25 1    
#> [18] {a, d, e} => {b} 0.2     1.0        0.2      1.25 1    
#> [19] {a, b, e} => {d} 0.2     1.0        0.2      2.50 1    

## Example 2: Mine association rules from an existing transactions dataset
##   using different minimum support and minimum confidence thresholds
data("Adult")

rules <- apriori(Adult,
  parameter = list(supp = 0.5, conf = 0.9, target = "rules")
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.9    0.1    1 none FALSE            TRUE       5     0.5      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 24421 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [9 item(s)] done [0.00s].
#> creating transaction tree ... done [0.02s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> writing ... [52 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
summary(rules)
#> set of 52 rules
#> 
#> rule length distribution (lhs + rhs):sizes
#>  1  2  3  4 
#>  2 13 24 13 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   2.000   3.000   2.923   3.250   4.000 
#> 
#> summary of quality measures:
#>     support         confidence        coverage           lift       
#>  Min.   :0.5084   Min.   :0.9031   Min.   :0.5406   Min.   :0.9844  
#>  1st Qu.:0.5415   1st Qu.:0.9155   1st Qu.:0.5875   1st Qu.:0.9937  
#>  Median :0.5974   Median :0.9229   Median :0.6293   Median :0.9997  
#>  Mean   :0.6436   Mean   :0.9308   Mean   :0.6915   Mean   :1.0036  
#>  3rd Qu.:0.7426   3rd Qu.:0.9494   3rd Qu.:0.7945   3rd Qu.:1.0057  
#>  Max.   :0.9533   Max.   :0.9583   Max.   :1.0000   Max.   :1.0586  
#>      count      
#>  Min.   :24832  
#>  1st Qu.:26447  
#>  Median :29178  
#>  Mean   :31433  
#>  3rd Qu.:36269  
#>  Max.   :46560  
#> 
#> mining info:
#>   data ntransactions support confidence
#>  Adult         48842     0.5        0.9
#>                                                                               call
#>  apriori(data = Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))

# since ... gets automatically added to parameter, we can also write the
#  same call shorter:
apriori(Adult, supp = 0.5, conf = 0.9, target = "rules")
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.9    0.1    1 none FALSE            TRUE       5     0.5      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 24421 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [9 item(s)] done [0.00s].
#> creating transaction tree ... done [0.02s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> writing ... [52 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
#> set of 52 rules 
```
