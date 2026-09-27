# Class APappearance — Specifying the appearance Argument of Apriori to Implement Rule Templates

Specifies the restrictions on the associations mined by
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md).
These restrictions can implement certain aspects of rule templates
described by Klemettinen (1994).

## Details

Note that appearance is only supported by the implementation of
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md).

## Slots

- `labels`:

  character vectors giving the labels of the items which can appear in
  the specified place (rhs, lhs or both for rules and items for
  itemsets). none specifies, that the items mentioned there cannot
  appear anywhere in the rule/itemset. Note that items cannot be
  specified in more than one place (i.e., you cannot specify an item in
  lhs and rhs, but have to specify it as both).

- `default`:

  one of `"both"`, `"lhs"`, `"rhs"`, `"none"`. Specified the default
  appearance for all items not explicitly mentioned in the other
  elements of the list. Leave unspecified and the code will guess the
  correct setting.

- `set`:

  used internally.

- `items`:

  used internally.

## Objects from the Class

If appearance restrictions are used, an appearance object will be
created automatically within the
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
function using the information in the named list of the function's
`appearance` argument. In this case, the item labels used in the list
will be automatically matched against the items in the used
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

Objects can also be created by calls of the form
`new("APappearance", ...)`. In this case, item IDs (column numbers of
the transactions incidence matrix) have to be used instead of labels.

## Coercions

- `as("NULL", "APappearance")`

- `as("list", "APappearance")`

## References

Christian Borgelt (2004) *Apriori — Finding Association Rules/Hyperedges
with the Apriori Algorithm.* <https://borgelt.net/apriori.html>

M. Klemettinen, H. Mannila, P. Ronkainen, H. Toivonen and A. I. Verkamo
(1994). Finding Interesting Rules from Large Sets of Discovered
Association Rules. In *Proceedings of the Third International Conference
on Information and Knowledge Management,* 401–407.

## See also

Other mining algorithms:
[`AScontrol-classes`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md),
[`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md),
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md),
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md),
[`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md)

## Author

Michael Hahsler and Bettina Gruen

## Examples

``` r
data("Adult")

## find only frequent itemsets which do not contain small or large income
is <- apriori(Adult,
  parameter = list(support = 0.1, target = "frequent"),
  appearance = list(none = c("income=small", "income=large"))
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>          NA    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen            target  ext
#>      10 frequent itemsets TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 4884 
#> 
#> set item appearances ...[2 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [29 item(s)] done [0.01s].
#> creating transaction tree ... done [0.02s].
#> checking subsets of size 1 2 3 4 5 6 7 8 9 done [0.05s].
#> sorting transactions ... done [0.01s].
#> writing ... [2066 set(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
itemFrequency(items(is))["income=small"]
#> income=small 
#>            0 
itemFrequency(items(is))["income=large"]
#> income=large 
#>            0 

## find itemsets that only contain small or large income, or young age
is <- apriori(Adult,
  parameter = list(support = 0.1, target = "frequent"),
  appearance = list(items = c("income=small", "income=large", "age=Young"))
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>          NA    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen            target  ext
#>      10 frequent itemsets TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 4884 
#> 
#> set item appearances ...[3 item(s)] done [0.00s].
#> set transactions ...[3 item(s), 48842 transaction(s)] done [0.01s].
#> sorting and recoding items ... [3 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 done [0.00s].
#> sorting transactions ... done [0.00s].
#> writing ... [4 set(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
inspect(head(is))
#>     items                     support   count
#> [1] {income=large}            0.1605381  7841
#> [2] {age=Young}               0.1971050  9627
#> [3] {income=small}            0.5061218 24720
#> [4] {age=Young, income=small} 0.1289259  6297

## find only rules with income-related variables in the right-hand-side.
incomeItems <- grep("^income=", itemLabels(Adult), value = TRUE)
incomeItems
#> [1] "income=small" "income=large"
rules <- apriori(Adult,
  parameter = list(support = 0.2, confidence = 0.5),
  appearance = list(rhs = incomeItems)
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.5    0.1    1 none FALSE            TRUE       5     0.2      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 9768 
#> 
#> set item appearances ...[2 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [18 item(s)] done [0.00s].
#> creating transaction tree ... done [0.02s].
#> checking subsets of size 1 2 3 4 5 6 7 done [0.01s].
#> writing ... [62 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
inspect(head(rules))
#>     lhs                               rhs            support   confidence
#> [1] {}                             => {income=small} 0.5061218 0.5061218 
#> [2] {marital-status=Never-married} => {income=small} 0.2086729 0.6323758 
#> [3] {hours-per-week=Full-time}     => {income=small} 0.3134802 0.5357805 
#> [4] {workclass=Private}            => {income=small} 0.3630687 0.5230048 
#> [5] {native-country=United-States} => {income=small} 0.4504115 0.5018936 
#> [6] {capital-gain=None}            => {income=small} 0.4849310 0.5286004 
#>     coverage  lift      count
#> [1] 1.0000000 1.0000000 24720
#> [2] 0.3299824 1.2494537 10192
#> [3] 0.5850907 1.0586000 15311
#> [4] 0.6941976 1.0333576 17733
#> [5] 0.8974243 0.9916459 21999
#> [6] 0.9173867 1.0444135 23685

## Note: For more complicated restrictions you have to mine all rules/itemsets and
## then filter the results afterwards.
```
