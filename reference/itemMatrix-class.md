# Class itemMatrix — Sparse Binary Incidence Matrix to Represent Sets of Items

The `itemMatrix` class is the basic building block for
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md),
and
[associations](http://michael.hahsler.net/arules/reference/associations-class.md).
The class contains a sparse Matrix representation of a set of itemsets
and the corresponding item labels.

## Usage

``` r
# S4 method for class 'itemMatrix'
summary(object, maxsum = 6, ...)

# S4 method for class 'itemMatrix'
dim(x)

nitems(x, ...)

# S4 method for class 'itemMatrix'
nitems(x)

# S4 method for class 'itemMatrix'
length(x)

toLongFormat(from, ...)

# S4 method for class 'itemMatrix'
toLongFormat(from, cols = c("ID", "item"), decode = TRUE)

# S4 method for class 'itemMatrix'
labels(object, itemSep = ",", setStart = "{", setEnd = "}")

itemLabels(object, ...)

itemLabels(object) <- value

# S4 method for class 'itemMatrix'
itemLabels(object)

# S4 method for class 'itemMatrix'
itemLabels(object) <- value

itemInfo(object)

itemInfo(object) <- value

# S4 method for class 'itemMatrix'
itemInfo(object)

# S4 method for class 'itemMatrix'
itemInfo(object) <- value

itemsetInfo(object)

itemsetInfo(object) <- value

# S4 method for class 'itemMatrix'
itemsetInfo(object)

# S4 method for class 'itemMatrix'
itemsetInfo(object) <- value

# S4 method for class 'itemMatrix'
dimnames(x)

# S4 method for class 'itemMatrix,list'
dimnames(x) <- value
```

## Arguments

- object, x, from:

  the object.

- maxsum:

  integer, how many items should be shown for the summary?

- ...:

  further parameters

- cols:

  columns for the long format.

- decode:

  decode item IDs to item labels.

- itemSep:

  item separator symbol.

- setStart:

  set start symbol.

- setEnd:

  set end symbol.

- value:

  replacement value

## Details

**Representation**

Sets of itemsets are represented as a compressed sparse binary matrix.
Conceptually, columns represent items and rows are the
sets/transactions. In the compressed form, each itemset is a vector of
column indices (called item IDs) representing the items.

**Warning:** Ideally, we would store the matrix as a row-oriented sparse
matrix (`ngRMatrix`), but the Matrix package provides better support for
column-oriented sparse classes
([Matrix::ngCMatrix](https://rdrr.io/pkg/Matrix/man/nsparseMatrix-class.html)).
The matrix is therefore internally stored in transposed form.

**Working with several `itemMatrix` objects**

If you work with several `itemMatrix` objects at the same time (e.g.,
several transaction sets, lhs and rhs of a rule, etc.), then the
encoding (itemLabes and order of the items in the binary matrix) in the
different itemMatrices is important and needs to conform. See
[itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md)
to learn how to encode and recode `itemMatrix` objects.

## Functions

- `summary(itemMatrix)`: show a summary.

- `dim(itemMatrix)`: returns the number of rows (itemsets) and columns
  (items in the encoding).

- `nitems(itemMatrix)`: returns the number of items in the encoding.

- `length(itemMatrix)`: returns the number of itemsets (rows) in the
  matrix.

- `toLongFormat(itemMatrix)`: convert the sets to long format (a
  data.frame with two columns, ID and item). Column names can be
  specified as a character vector of length 2 called `cols`.

- `labels(itemMatrix)`: returns labels for the itemsets. The following
  arguments can be used to customize the representation of the labels:
  `itemSep`, `setStart` and `setEnd`.

- `itemLabels(itemMatrix)`: returns the item labels used for encoding as
  a character vector.

- `itemLabels(itemMatrix) <- value`: replaces the item labels used for
  encoding.

- `itemInfo(itemMatrix)`: returns the whole item/column information
  data.frame including labels.

- `itemInfo(itemMatrix) <- value`: replaces the item/column info by a
  data.frame.

- `itemsetInfo(itemMatrix)`: returns the item set/row information
  data.frame.

- `itemsetInfo(itemMatrix) <- value`: replaces the item set/row info by
  a data.frame.

- `dimnames(itemMatrix)`: returns a list with the dimname vectors.

- `dimnames(x = itemMatrix) <- value`: replace the dimnames.

## Slots

- `data`:

  a sparse matrix of class
  [Matrix::ngCMatrix](https://rdrr.io/pkg/Matrix/man/nsparseMatrix-class.html)
  representing the itemsets. **Warning:** the matrix is stored in
  transposed form for efficiency reasons!.

- `itemInfo`:

  a data.frame

- `itemsetInfo`:

  a data.frame

## Objects from the Class

Objects can be created by calls of the form `new("itemMatrix", ...)`.
However, most of the time objects will be created by coercion from a
matrix, list or data.frame.

## Coercions

- `as("matrix", "itemMatrix")`

- `as("itemMatrix", "matrix")`

- `as("list", "itemMatrix")`

- `as("itemMatrix", "list")`

- `as("itemMatrix", "ngCMatrix")`

- `as("ngCMatrix", "itemMatrix")`

**Warning:** the `ngCMatrix` representation is transposed!

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
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md),
[`tidLists-class`](http://michael.hahsler.net/arules/reference/tidLists-class.md),
[`transactions-class`](http://michael.hahsler.net/arules/reference/transactions-class.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
set.seed(1234)

## Generate a logical matrix with 5000 random itemsets for 20 items
m <- matrix(runif(5000 * 20) > 0.8,
  ncol = 20,
  dimnames = list(NULL, paste("item", c(1:20), sep = ""))
)
head(m)
#>      item1 item2 item3 item4 item5 item6 item7 item8 item9 item10 item11 item12
#> [1,] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE   TRUE  FALSE  FALSE
#> [2,] FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  FALSE  FALSE  FALSE
#> [3,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  FALSE  FALSE  FALSE
#> [4,] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE  FALSE  FALSE  FALSE
#> [5,]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  FALSE  FALSE  FALSE
#> [6,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  FALSE  FALSE  FALSE
#>      item13 item14 item15 item16 item17 item18 item19 item20
#> [1,]   TRUE  FALSE   TRUE  FALSE  FALSE   TRUE  FALSE   TRUE
#> [2,]  FALSE  FALSE  FALSE  FALSE   TRUE  FALSE  FALSE  FALSE
#> [3,]  FALSE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
#> [4,]  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
#> [5,]  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE   TRUE
#> [6,]  FALSE  FALSE  FALSE  FALSE   TRUE   TRUE  FALSE  FALSE

## Coerce the logical matrix into an itemMatrix object
imatrix <- as(m, "itemMatrix")
imatrix
#> itemMatrix in sparse format with
#>  5000 rows (elements/transactions) and
#>  20 columns (items)

## An itemMatrix contains a set of itemsets (each row is an itemset).
## The length of the set is the number of rows.
length(imatrix)
#> [1] 5000

## The sparese matrix also has regular matrix  dimensions.
dim(imatrix)
#> [1] 5000   20
nrow(imatrix)
#> [1] 5000
ncol(imatrix)
#> [1] 20

## Subsetting: Get first 5 elements (rows) of the itemMatrix. This can be done in
## several ways.
imatrix[1:5] ### get elements 1:5
#> itemMatrix in sparse format with
#>  5 rows (elements/transactions) and
#>  20 columns (items)
imatrix[1:5, ] ### Matrix subsetting for rows 1:5
#> itemMatrix in sparse format with
#>  5 rows (elements/transactions) and
#>  20 columns (items)
head(imatrix, n = 5) ### head()
#> itemMatrix in sparse format with
#>  5 rows (elements/transactions) and
#>  20 columns (items)

## Get first 5 elements (rows) of the itemMatrix as list.
as(imatrix[1:5], "list")
#> $`1`
#> [1] "item7"  "item10" "item13" "item15" "item18" "item20"
#> 
#> $`2`
#> [1] "item5"  "item7"  "item17"
#> 
#> $`3`
#> [1] "item15"
#> 
#> $`4`
#> [1] "item3" "item8"
#> 
#> $`5`
#> [1] "item1"  "item8"  "item20"
#> 

## Get first 5 elements (rows) of the itemMatrix as matrix.
as(imatrix[1:5], "matrix")
#>   item1 item2 item3 item4 item5 item6 item7 item8 item9 item10 item11 item12
#> 1 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE   TRUE  FALSE  FALSE
#> 2 FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  FALSE  FALSE  FALSE
#> 3 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  FALSE  FALSE  FALSE
#> 4 FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE  FALSE  FALSE  FALSE
#> 5  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  FALSE  FALSE  FALSE
#>   item13 item14 item15 item16 item17 item18 item19 item20
#> 1   TRUE  FALSE   TRUE  FALSE  FALSE   TRUE  FALSE   TRUE
#> 2  FALSE  FALSE  FALSE  FALSE   TRUE  FALSE  FALSE  FALSE
#> 3  FALSE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
#> 4  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
#> 5  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE   TRUE

## Get first 5 elements (rows) of the itemMatrix as sparse ngCMatrix.
## **Warning:** For efficiency reasons, the ngCMatrix is transposed! You
## can transpose it again to get the expected format.
as(imatrix[1:5], "ngCMatrix")
#> 20 x 5 sparse Matrix of class "ngCMatrix"
#>        1 2 3 4 5
#> item1  . . . . |
#> item2  . . . . .
#> item3  . . . | .
#> item4  . . . . .
#> item5  . | . . .
#> item6  . . . . .
#> item7  | | . . .
#> item8  . . . | |
#> item9  . . . . .
#> item10 | . . . .
#> item11 . . . . .
#> item12 . . . . .
#> item13 | . . . .
#> item14 . . . . .
#> item15 | . | . .
#> item16 . . . . .
#> item17 . | . . .
#> item18 | . . . .
#> item19 . . . . .
#> item20 | . . . |
t(as(imatrix[1:5], "ngCMatrix"))
#> 5 x 20 sparse Matrix of class "ngCMatrix"
#>   [[ suppressing 20 column names ‘item1’, ‘item2’, ‘item3’ ... ]]
#>                                          
#> 1 . . . . . . | . . | . . | . | . . | . |
#> 2 . . . . | . | . . . . . . . . . | . . .
#> 3 . . . . . . . . . . . . . . | . . . . .
#> 4 . . | . . . . | . . . . . . . . . . . .
#> 5 | . . . . . . | . . . . . . . . . . . |

## Get labels for the first 5 itemsets (first default and then with
## custom formating)
labels(imatrix[1:5])
#> [1] "{item7,item10,item13,item15,item18,item20}"
#> [2] "{item5,item7,item17}"                      
#> [3] "{item15}"                                  
#> [4] "{item3,item8}"                             
#> [5] "{item1,item8,item20}"                      
labels(imatrix[1:5], itemSep = " + ", setStart = "", setEnd = "")
#> [1] "item7 + item10 + item13 + item15 + item18 + item20"
#> [2] "item5 + item7 + item17"                            
#> [3] "item15"                                            
#> [4] "item3 + item8"                                     
#> [5] "item1 + item8 + item20"                            

## Create itemsets manually from an itemMatrix. Itemsets contain items in the form of
## an itemMatrix and additional quality measures (not supplied in the example).
is <- new("itemsets", items = imatrix)
is
#> set of 5000 itemsets 
inspect(head(is, n = 3))
#>     items                                          
#> [1] {item7, item10, item13, item15, item18, item20}
#> [2] {item5, item7, item17}                         
#> [3] {item15}                                       


## Create rules manually. I use imatrix[4:6] for the lhs of the rules and
## imatrix[1:3] for the rhs. Rhs and lhs cannot share items so I use
## itemSetdiff here. I also assign missing values for the quality measures support
## and confidence.
rules <- new("rules",
  lhs = itemSetdiff(imatrix[4:6], imatrix[1:3]),
  rhs = imatrix[1:3],
  quality = data.frame(
    support = c(NA, NA, NA),
    confidence = c(NA, NA, NA)
  )
)
rules
#> set of 3 rules 
inspect(rules)
#>     lhs          rhs       support confidence
#> [1] {item3,                                  
#>      item8}   => {item7,                     
#>                   item10,                    
#>                   item13,                    
#>                   item15,                    
#>                   item18,                    
#>                   item20}       NA         NA
#> [2] {item1,                                  
#>      item8,                                  
#>      item20}  => {item5,                     
#>                   item7,                     
#>                   item17}       NA         NA
#> [3] {item9,                                  
#>      item17,                                 
#>      item18}  => {item15}       NA         NA

## Manually create a itemMatrix with an item encoding that matches imatrix (20 items in order
## item1, item2, ..., item20)
itemset_list <- list(
  c("item1", "item2"),
  c("item3")
)

imatrix_new <- encode(itemset_list, itemLabels = imatrix)
imatrix_new
#> itemMatrix in sparse format with
#>  2 rows (elements/transactions) and
#>  20 columns (items)
compatible(imatrix_new, imatrix)
#> [1] TRUE
```
