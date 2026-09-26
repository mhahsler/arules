# Class tidLists — Transaction ID Lists for Items/Itemsets

Class to represent transaction ID lists and associated methods.

## Usage

``` r
tidLists(x)

# S4 method for class 'tidLists'
summary(object, maxsum = 6, ...)

# S4 method for class 'tidLists'
dim(x)

# S4 method for class 'tidLists'
dimnames(x)

# S4 method for class 'tidLists,list'
dimnames(x) <- value

# S4 method for class 'tidLists'
length(x)

# S4 method for class 'tidLists'
t(x)

# S4 method for class 'tidLists'
transactionInfo(x)

# S4 method for class 'tidLists'
transactionInfo(x) <- value

# S4 method for class 'tidLists'
itemInfo(object)

# S4 method for class 'tidLists'
itemInfo(object) <- value

# S4 method for class 'tidLists'
itemLabels(object)

# S4 method for class 'tidLists'
labels(object)
```

## Arguments

- x, object:

  the object

- maxsum:

  maximum numbers of itemsets shown in the summary

- ...:

  further arguments

- value:

  replacement value

## Details

Transaction ID lists contains a set of lists. Each list is associated
with an item/itemset and stores the IDs of the transactions which
support the item/itemset.

`tidLists` uses the class
[Matrix::ngCMatrix](https://rdrr.io/pkg/Matrix/man/nsparseMatrix-class.html)
to efficiently store the transaction ID lists as a sparse matrix. Each
column in the matrix represents one transaction ID list.

`tidLists` can be used for different purposes. For some operations
(e.g., support counting) it is efficient to coerce a
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
database into `tidLists` where each list contains the transaction IDs
for an item (and the support is given by the length of the list).

The implementation of the Eclat mining algorithm (which uses transaction
ID list intersection) can also produce transaction ID lists for the
found itemsets as part of the returned
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
object. These lists can then be used for further computation.

## Functions

- `summary(tidLists)`: create a summary

- `dim(tidLists)`: get dimensions. The rows represent the itemsets and
  the columns are the transactions.

- `dimnames(tidLists)`: get dimnames

- `dimnames(x = tidLists) <- value`: replace dimnames

- `length(tidLists)`: get the number of itemsets.

- `t(tidLists)`: this object is not transposable.
  [`t()`](https://rdrr.io/r/base/t.html) results in an error.

- `transactionInfo(tidLists)`: get the transaction info data.frame

- `transactionInfo(tidLists) <- value`: replace the the transaction info
  data.frame

- `itemInfo(tidLists)`: get the item info data.frame

- `itemInfo(tidLists) <- value`: replace the item info data.frame

- `itemLabels(tidLists)`: get the item labels

- `labels(tidLists)`: convert the tid lists into a text representation.

## Slots

- `data`:

  an object of class
  [Matrix::ngCMatrix](https://rdrr.io/pkg/Matrix/man/nsparseMatrix-class.html).

- `itemInfo`:

  a data.frame

- `transactionInfo`:

  a data.frame

## Objects from the Class

Objects are created

- as part of the
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  mined by
  [`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md) with
  `tidLists = TRUE` in the
  [ECparameter](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  object.

- by
  [`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md).

- by coercion from an object of class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

- by calls of the form `new("tidLists", ...)`.

## Coercions

- `as("tidLists", "list")`

- `as("list", "tidLists")`

- `as("tidLists", "ngCMatrix")`

- `as("tidLists", "transactions")`

- `as("transactions", "tidLists")`

- `as("tidLists", "itemMatrix")`

- `as("itemMatrix", "tidLists")`

## See also

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
[`transactions-class`](http://michael.hahsler.net/arules/reference/transactions-class.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
## Create transaction data set.
data <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("b", "e"),
  c("b", "c", "e"),
  c("a", "d", "e"),
  c("a", "c"),
  c("a", "b", "d"),
  c("c", "e"),
  c("a", "b", "d", "e")
)
data <- as(data, "transactions")
data
#> transactions in sparse format with
#>  10 transactions (rows) and
#>  5 items (columns)

## convert transactions to transaction ID lists
tl <- as(data, "tidLists")
tl
#> tidLists in sparse format with
#>  5 items/itemsets (rows) and
#>  10 transactions (columns)

inspect(tl)
#>   items transactionIDs  
#> 1 a     {1,2,3,6,7,8,10}
#> 2 b     {1,2,3,4,5,8,10}
#> 3 c     {1,5,7,9}       
#> 4 d     {3,6,8,10}      
#> 5 e     {4,5,6,9,10}    
dim(tl)
#> [1]  5 10
dimnames(tl)
#> [[1]]
#> [1] "a" "b" "c" "d" "e"
#> 
#> [[2]]
#> NULL
#> 

## inspect visually
image(tl)


## mine itemsets with transaction ID lists
f <- eclat(data, parameter = list(support = 0, tidLists = TRUE))
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>      TRUE       0      1     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 0 
#> 
#> create itemset ... 
#> set transactions ...[5 item(s), 10 transaction(s)] done [0.00s].
#> sorting and recoding items ... [5 item(s)] done [0.00s].
#> creating bit matrix ... [5 row(s), 10 column(s)] done [0.00s].
#> writing  ... [21 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].
tl2 <- tidLists(f)
inspect(tl2)
#>    items     transactionIDs  
#> 1  {b,c,e}   {5}             
#> 2  {a,b,c}   {1}             
#> 3  {a,c}     {1,7}           
#> 4  {b,c}     {1,5}           
#> 5  {c,e}     {5,9}           
#> 6  {a,b,d,e} {10}            
#> 7  {a,d,e}   {6,10}          
#> 8  {b,d,e}   {10}            
#> 9  {a,b,d}   {3,8,10}        
#> 10 {a,d}     {3,6,8,10}      
#> 11 {b,d}     {3,8,10}        
#> 12 {d,e}     {6,10}          
#> 13 {a,b,e}   {10}            
#> 14 {a,e}     {6,10}          
#> 15 {b,e}     {4,5,10}        
#> 16 {a,b}     {1,2,3,8,10}    
#> 17 {a}       {1,2,3,6,7,8,10}
#> 18 {b}       {1,2,3,4,5,8,10}
#> 19 {e}       {4,5,6,9,10}    
#> 20 {d}       {3,6,8,10}      
#> 21 {c}       {1,5,7,9}       
```
