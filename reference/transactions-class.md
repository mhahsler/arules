# Class transactions — Binary Incidence Matrix for Transactions

The `transactions` class is a subclass of
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
and represents transaction data used for mining
[associations](http://michael.hahsler.net/arules/reference/associations-class.md).

## Usage

``` r
transactions(
  x,
  itemLabels = NULL,
  transactionInfo = NULL,
  format = "wide",
  cols = NULL
)

# S4 method for class 'transactions'
summary(object)

# S4 method for class 'transactions'
toLongFormat(from, cols = c("TID", "item"), decode = TRUE)

# S4 method for class 'transactions'
items(x)

transactionInfo(x)

# S4 method for class 'transactions'
transactionInfo(x)

transactionInfo(x) <- value

# S4 method for class 'transactions'
transactionInfo(x) <- value

# S4 method for class 'transactions'
dimnames(x)

# S4 method for class 'transactions,list'
dimnames(x) <- value
```

## Arguments

- x, object, from:

  the object

- itemLabels:

  a vector with labels for the items

- transactionInfo:

  a transaction information data.frame with one row per transaction.

- format:

  `"wide"` or `"long"` format? Format wide is a regular data.frame where
  each row contains an object. Format "long" is a data.frame with one
  column with transaction IDs and one with an item (see `cols` below).

- cols:

  a numeric or character vector of length two giving the index or names
  of the columns (fields) with the transaction and item ids in the long
  format.

- decode:

  translate item IDs to item labels?

- value:

  replacement value

## Details

Transactions store the presence of items in each individual transaction
as binary matrix where rows represent the transactions and columns
represent the items. `transactions` direct extends class
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
to store the sparse binary incidence matrix, item labels, and optionally
transaction IDs and user IDs. If you work with several transaction sets
at the same time, then the encoding (order of the items in the binary
matrix) in the different sets is important. See
[itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md)
to learn how to encode and recode transaction sets.

**Data Preparation**

Data typically starts as a data.frame or a matrix and needs to be
prepared before it can be converted into `transactions` (see coercion
methods in the Methods Section and the Example Section below for details
on the needed format).

Columns need to represent items which is different depending on the data
type of the column:

- **Continuous variables:** Continuous variables cannot directly be
  represented as items and need to be discretized first. An item
  resulting from discretization might be `age>18` and the column
  contains only `TRUE` or `FALSE`. Alternatively, it can be a factor
  with levels `age<=18`, `50=>age>18` and `age>50`. These will be
  automatically converted into 3 items, one for each level.
  Discretization is described in functions
  [`discretize()`](http://michael.hahsler.net/arules/reference/discretize.md)
  and
  [`discretizeDF()`](http://michael.hahsler.net/arules/reference/discretize.md).

- **Logical variables:** A logical variable describing a person could be
  `tall` indicating if the person is tall using the values `TRUE` and
  `FALSE`. The fact that the person is tall would be encoded in the
  transaction containing the item `tall` while not tall persons would
  not have this item. Therefore, for logical variables, the `TRUE` value
  is converted into an item with the name of the variable and for the
  `FALSE` values no item is created.

- **Factors:** Columns with nominal values (i.e.,
  [factor](https://rdrr.io/r/base/factor.html),
  [ordered](https://rdrr.io/r/base/factor.html)) are translated into a
  series of binary items (one for each level constructed as
  `variable name = level`). Items cannot represent order and this
  ordered factors lose the order information. Note that nominal
  variables need to be encoded as factors (and not characters or
  numbers). This can be done with

  `data[,"a_nominal_var"] <- factor(data[,"a_nominal_var"])`.

  Complete examples for how to prepare data can be found in the man
  pages for
  [Income](http://michael.hahsler.net/arules/reference/Income.md) and
  [Adult](http://michael.hahsler.net/arules/reference/Adult.md).

## Functions

- `summary(transactions)`: produce a summary

- `toLongFormat(transactions)`: convert the transactions to long format
  (a data.frame with two columns, tid and item). Column names can be
  specified as a character vector of length 2 called `cols`.

- `items(transactions)`: get the transactions as an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)

- `transactionInfo(transactions)`: get the transaction info data.frame

- `transactionInfo(transactions) <- value`: replace the transaction info
  data.frame

- `dimnames(transactions)`: get the dimnames

- `dimnames(x = transactions) <- value`: set the dimnames

## Slots

Slots are inherited from
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

## Objects from the Class

Objects are created by:

- coercion from objects of other classes. `itemLabels` and
  `transactionInfo` are by default created from information in `x`
  (e.g., from row and column names).

- the constructor function `transactions()`

- by calling `new("transactions", ...)`.

See Examples Section for creating transactions from data.

## Coercions

- `as("transactions", "matrix")`

- `as("matrix", "transactions")`

- `as("list", "transactions")`

- `as("transactions", "list")`

- `as("data.frame", "transactions")`

- `as("transactions", "data.frame")`

- `as("ngCMatrix", "transactions")`

## See also

Superclass:
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`image`](http://michael.hahsler.net/arules/reference/image.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md),
[`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md),
[`itemMatrix-class`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md),
[`tidLists-class`](http://michael.hahsler.net/arules/reference/tidLists-class.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
## Example 1: creating transactions form a list (each element is a transaction)
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
## Note: S4 coercion does the same trans1 <- as(a_list, "transactions")
trans1 <- transactions(a_list)
trans1
#> transactions in sparse format with
#>  5 transactions (rows) and
#>  5 items (columns)

## Analyze the transactions
summary(trans1)
#> transactions as itemMatrix in sparse format with
#>  5 rows (elements/itemsets/transactions) and
#>  5 columns (items) and a density of 0.56 
#> 
#> most frequent items:
#>       a       b       c       d       e (Other) 
#>       4       4       2       2       2       0 
#> 
#> element (itemset/transaction) length distribution:
#> sizes
#> 2 3 4 
#> 2 2 1 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     2.0     2.0     3.0     2.8     3.0     4.0 
#> 
#> includes extended item information - examples:
#>   labels
#> 1      a
#> 2      b
#> 3      c
#> 
#> includes extended transaction information - examples:
#>   transactionID
#> 1           Tr1
#> 2           Tr2
#> 3           Tr3
image(trans1)


## Example 2: creating transactions from a 0-1 matrix with 5 transactions (rows) and
##            5 items (columns)
a_matrix <- matrix(
  c(
    1, 1, 1, 0, 0,
    1, 1, 0, 0, 0,
    1, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    1, 1, 0, 1, 1
  ),
  ncol = 5
)

## Set item names (columns) and transaction labels (rows)
colnames(a_matrix) <- c("a", "b", "c", "d", "e")
rownames(a_matrix) <- paste("Tr", c(1:5), sep = "")

a_matrix
#>     a b c d e
#> Tr1 1 1 1 0 1
#> Tr2 1 1 1 0 1
#> Tr3 1 0 0 1 0
#> Tr4 0 0 1 0 1
#> Tr5 0 0 0 1 1

## Create transactions
trans2 <- transactions(a_matrix)
trans2
#> transactions in sparse format with
#>  5 transactions (rows) and
#>  5 items (columns)
inspect(trans2)
#>     items        transactionID
#> [1] {a, b, c, e} Tr1          
#> [2] {a, b, c, e} Tr2          
#> [3] {a, d}       Tr3          
#> [4] {c, e}       Tr4          
#> [5] {d, e}       Tr5          

## Example 3: creating transactions from data.frame (wide format)
a_df <- data.frame(
  age = as.factor(c(6, 8, NA, 9, 16)),
  grade = as.factor(c("A", "C", "F", NA, "C")),
  pass = c(TRUE, TRUE, FALSE, TRUE, TRUE)
)
## Note: factors are translated differently than logicals and NAs are ignored
a_df
#>    age grade  pass
#> 1    6     A  TRUE
#> 2    8     C  TRUE
#> 3 <NA>     F FALSE
#> 4    9  <NA>  TRUE
#> 5   16     C  TRUE

## Create transactions
trans3 <- transactions(a_df)
inspect(trans3)
#>     items                   transactionID
#> [1] {age=6, grade=A, pass}  1            
#> [2] {age=8, grade=C, pass}  2            
#> [3] {grade=F}               3            
#> [4] {age=9, pass}           4            
#> [5] {age=16, grade=C, pass} 5            

## Note that coercing the transactions back to a data.frame does not recreate the
## original data.frame, but represents the transactions as sets of items
as(trans3, "data.frame")
#>                   items transactionID
#> 1  {age=6,grade=A,pass}             1
#> 2  {age=8,grade=C,pass}             2
#> 3             {grade=F}             3
#> 4          {age=9,pass}             4
#> 5 {age=16,grade=C,pass}             5

## Example 4: creating transactions from a data.frame with
## transaction IDs and items (long format)
a_df3 <- data.frame(
  TID =  c(1, 1, 2, 2, 2, 3),
  item = c("a", "b", "a", "b", "c", "b")
)
a_df3
#>   TID item
#> 1   1    a
#> 2   1    b
#> 3   2    a
#> 4   2    b
#> 5   2    c
#> 6   3    b
trans4 <- transactions(a_df3, format = "long", cols = c("TID", "item"))
trans4
#> transactions in sparse format with
#>  3 transactions (rows) and
#>  3 items (columns)
inspect(trans4)
#>     items     transactionID
#> [1] {a, b}    1            
#> [2] {a, b, c} 2            
#> [3] {b}       3            

## convert transactions back into long format.
toLongFormat(trans4)
#>   TID item
#> 1   1    a
#> 2   1    b
#> 3   2    a
#> 4   2    b
#> 5   2    c
#> 6   3    b

## Example 5: create transactions from a dataset with numeric variables
## using discretization.
data(iris)

irisDisc <- discretizeDF(iris)
head(irisDisc)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1    [4.3,5.4)   [3.2,4.4]     [1,2.63) [0.1,0.867)  setosa
#> 2    [4.3,5.4)   [2.9,3.2)     [1,2.63) [0.1,0.867)  setosa
#> 3    [4.3,5.4)   [3.2,4.4]     [1,2.63) [0.1,0.867)  setosa
#> 4    [4.3,5.4)   [2.9,3.2)     [1,2.63) [0.1,0.867)  setosa
#> 5    [4.3,5.4)   [3.2,4.4]     [1,2.63) [0.1,0.867)  setosa
#> 6    [5.4,6.3)   [3.2,4.4]     [1,2.63) [0.1,0.867)  setosa

trans5 <- transactions(irisDisc)
trans5
#> transactions in sparse format with
#>  150 transactions (rows) and
#>  15 items (columns)
inspect(head(trans5))
#>     items                      transactionID
#> [1] {Sepal.Length=[4.3,5.4),                
#>      Sepal.Width=[3.2,4.4],                 
#>      Petal.Length=[1,2.63),                 
#>      Petal.Width=[0.1,0.867),               
#>      Species=setosa}                       1
#> [2] {Sepal.Length=[4.3,5.4),                
#>      Sepal.Width=[2.9,3.2),                 
#>      Petal.Length=[1,2.63),                 
#>      Petal.Width=[0.1,0.867),               
#>      Species=setosa}                       2
#> [3] {Sepal.Length=[4.3,5.4),                
#>      Sepal.Width=[3.2,4.4],                 
#>      Petal.Length=[1,2.63),                 
#>      Petal.Width=[0.1,0.867),               
#>      Species=setosa}                       3
#> [4] {Sepal.Length=[4.3,5.4),                
#>      Sepal.Width=[2.9,3.2),                 
#>      Petal.Length=[1,2.63),                 
#>      Petal.Width=[0.1,0.867),               
#>      Species=setosa}                       4
#> [5] {Sepal.Length=[4.3,5.4),                
#>      Sepal.Width=[3.2,4.4],                 
#>      Petal.Length=[1,2.63),                 
#>      Petal.Width=[0.1,0.867),               
#>      Species=setosa}                       5
#> [6] {Sepal.Length=[5.4,6.3),                
#>      Sepal.Width=[3.2,4.4],                 
#>      Petal.Length=[1,2.63),                 
#>      Petal.Width=[0.1,0.867),               
#>      Species=setosa}                       6

## Note, creating transactions without discretizing numeric variables will apply the
## default discretization and also create a warning.


## Example 6: create transactions manually (with the same item coding as in trans5)
trans6 <- transactions(
  list(
    c("Sepal.Length=[4.3,5.4)", "Species=setosa"),
    c("Sepal.Length=[4.3,5.4)", "Species=setosa")
  ),
  itemLabels = trans5
)
trans6
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  15 items (columns)

inspect(trans6)
#>     items                                   
#> [1] {Sepal.Length=[4.3,5.4), Species=setosa}
#> [2] {Sepal.Length=[4.3,5.4), Species=setosa}
```
