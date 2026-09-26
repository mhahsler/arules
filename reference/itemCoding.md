# Item Coding — Conversion between Item Labels and Column IDs

The order in which items are stored in an
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
is called the *item coding*. The following generic functions and methods
are used to translate between the representation in the itemMatrix
format (used in transactions, rules and itemsets), item labels and
numeric item IDs (i.e., the column numbers in the itemMatrix
representation).

## Usage

``` r
decode(x, ...)

# S4 method for class 'numeric'
decode(x, itemLabels)

# S4 method for class 'list'
decode(x, itemLabels)

encode(x, ...)

# S4 method for class 'character'
encode(x, itemLabels, itemMatrix = TRUE)

# S4 method for class 'numeric'
encode(x, itemLabels, itemMatrix = TRUE)

# S4 method for class 'list'
encode(x, itemLabels, itemMatrix = TRUE)

recode(x, ...)

# S4 method for class 'itemMatrix'
recode(x, itemLabels = NULL, match = NULL)

# S4 method for class 'itemsets'
recode(x, itemLabels = NULL, match = NULL)

# S4 method for class 'rules'
recode(x, itemLabels = NULL, match = NULL)

compatible(x, y)

# S4 method for class 'itemMatrix'
compatible(x, y)

# S4 method for class 'associations'
compatible(x, y)
```

## Arguments

- x:

  a vector or a list of vectors of character strings (for `encode()` or
  of numeric (for `decode()`), or an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  (for `recode()`).

- ...:

  further arguments.

- itemLabels:

  a vector of character strings used for coding where the position of an
  item label in the vector gives the item's column ID. Alternatively, a
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md)
  object can be specified and the item labels or these objects are used.

- itemMatrix:

  return an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  otherwise an object of the same class as `x` is returned.

- match:

  deprecated: used `itemLabels` instead.

- y:

  an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md)
  to compare item coding to `x`.

## Value

`recode()` always returns an object of the same class as `x`.

For `encode()` with `itemMatrix = TRUE` an object of class
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
is returned. Otherwise the result is of the same type as `x`, e.g., a
list or a vector.

## Details

**Item coding compatibility:** When working with several datasets or
different subsets of the same dataset, combining or compare the found
itemsets or rules requires a compatible item coding. That is, the sparse
matrices representing the items (the itemMatrix objects) have columns
for the same items in exactly the same order. The coercion to
transactions with
[`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
or `as(x, "transactions")` will create the item coding by adding items
in the order they are encountered in the dataset. This can lead to
different item codings (different order, missing items) for even only
slightly different datasets or versions of a dataset. Method
`compatible()` can be used to check if two sets have the same item
coding.

**Defining a common item coding:** When working with many sets, then
first a common item coding should be defined by creating a vector with
all possible item labels and then specify them as `itemLabels` to create
transactions with
[`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md).
Compatible
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
objects can be created using `encode()`.

**Recoding and Decoding:** Two incompatible objects can be made
compatible using `recode()`. Recode one object by specifying the other
object in `itemLabels`.

`decode()` converts from the column IDs used in the itemMatrix
representation to item labels. `decode()` is used by
[`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md).

## See also

[`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md),
[associations](http://michael.hahsler.net/arules/reference/associations-class.md),
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)

Other preprocessing:
[`discretize()`](http://michael.hahsler.net/arules/reference/discretize.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md)

## Author

Michael Hahsler

## Examples

``` r
data("Adult")

## Example 1: Manual decoding
## Extract the item coding as a vector of item labels.
iLabels <- itemLabels(Adult)
head(iLabels)
#> [1] "age=Young"             "age=Middle-aged"       "age=Senior"           
#> [4] "age=Old"               "workclass=Federal-gov" "workclass=Local-gov"  

## get undecoded list (itemIDs)
list <- LIST(Adult[1:5], decode = FALSE)
list
#> [[1]]
#>  [1]   2  11  26  33  36  51  60  62  64  66  70 111 114
#> 
#> [[2]]
#>  [1]   3  10  26  31  39  50  60  62  63  66  69 111 114
#> 
#> [[3]]
#>  [1]   2   8  21  29  41  51  60  62  63  66  70 111 114
#> 
#> [[4]]
#>  [1]   3   8  19  31  41  50  58  62  63  66  70 111 114
#> 
#> [[5]]
#>  [1]   2   8  26  31  45  55  58  61  63  66  70  77 114
#> 

## decode itemIDs by replacing them with the appropriate item label
decode(list, itemLabels = iLabels)
#> [[1]]
#>  [1] "age=Middle-aged"              "workclass=State-gov"         
#>  [3] "education=Bachelors"          "marital-status=Never-married"
#>  [5] "occupation=Adm-clerical"      "relationship=Not-in-family"  
#>  [7] "race=White"                   "sex=Male"                    
#>  [9] "capital-gain=Low"             "capital-loss=None"           
#> [11] "hours-per-week=Full-time"     "native-country=United-States"
#> [13] "income=small"                
#> 
#> [[2]]
#>  [1] "age=Senior"                        "workclass=Self-emp-not-inc"       
#>  [3] "education=Bachelors"               "marital-status=Married-civ-spouse"
#>  [5] "occupation=Exec-managerial"        "relationship=Husband"             
#>  [7] "race=White"                        "sex=Male"                         
#>  [9] "capital-gain=None"                 "capital-loss=None"                
#> [11] "hours-per-week=Part-time"          "native-country=United-States"     
#> [13] "income=small"                     
#> 
#> [[3]]
#>  [1] "age=Middle-aged"              "workclass=Private"           
#>  [3] "education=HS-grad"            "marital-status=Divorced"     
#>  [5] "occupation=Handlers-cleaners" "relationship=Not-in-family"  
#>  [7] "race=White"                   "sex=Male"                    
#>  [9] "capital-gain=None"            "capital-loss=None"           
#> [11] "hours-per-week=Full-time"     "native-country=United-States"
#> [13] "income=small"                
#> 
#> [[4]]
#>  [1] "age=Senior"                        "workclass=Private"                
#>  [3] "education=11th"                    "marital-status=Married-civ-spouse"
#>  [5] "occupation=Handlers-cleaners"      "relationship=Husband"             
#>  [7] "race=Black"                        "sex=Male"                         
#>  [9] "capital-gain=None"                 "capital-loss=None"                
#> [11] "hours-per-week=Full-time"          "native-country=United-States"     
#> [13] "income=small"                     
#> 
#> [[5]]
#>  [1] "age=Middle-aged"                   "workclass=Private"                
#>  [3] "education=Bachelors"               "marital-status=Married-civ-spouse"
#>  [5] "occupation=Prof-specialty"         "relationship=Wife"                
#>  [7] "race=Black"                        "sex=Female"                       
#>  [9] "capital-gain=None"                 "capital-loss=None"                
#> [11] "hours-per-week=Full-time"          "native-country=Cuba"              
#> [13] "income=small"                     
#> 


## Example 2: Manually create an itemMatrix using iLabels as the common item coding
data <- list(
  c("income=small", "age=Young"),
  c("income=large", "age=Middle-aged")
)

# Option a: encode to match the item coding in Adult
iM <- encode(data, itemLabels = Adult)
iM
#> itemMatrix in sparse format with
#>  2 rows (elements/transactions) and
#>  115 columns (items)
inspect(iM)
#>     items                          
#> [1] {age=Young, income=small}      
#> [2] {age=Middle-aged, income=large}
compatible(iM, Adult)
#> [1] TRUE

# Option b: coercion plus recode to make it compatible to Adult
#           (note: the coding has 115 item columns after recode)
iM <- as(data, "itemMatrix")
iM
#> itemMatrix in sparse format with
#>  2 rows (elements/transactions) and
#>  4 columns (items)
compatible(iM, Adult)
#> [1] FALSE

iM <- recode(iM, itemLabels = Adult)
iM
#> itemMatrix in sparse format with
#>  2 rows (elements/transactions) and
#>  115 columns (items)
compatible(iM, Adult)
#> [1] TRUE


## Example 3: use recode to make itemMatrices compatible
## select first 100 transactions and all education-related items
sub <- Adult[1:100, itemInfo(Adult)$variables == "education"]
itemLabels(sub)
#>  [1] "education=Preschool"    "education=1st-4th"      "education=5th-6th"     
#>  [4] "education=7th-8th"      "education=9th"          "education=10th"        
#>  [7] "education=11th"         "education=12th"         "education=HS-grad"     
#> [10] "education=Prof-school"  "education=Assoc-acdm"   "education=Assoc-voc"   
#> [13] "education=Some-college" "education=Bachelors"    "education=Masters"     
#> [16] "education=Doctorate"   
image(sub)


## After choosing only a subset of items (columns), the item coding is now
## no longer compatible with the Adult dataset
compatible(sub, Adult)
#> [1] FALSE

## recode to match Adult again
sub.recoded <- recode(sub, itemLabels = Adult)
image(sub.recoded)



## Example 4: manually create 2 new transaction for the Adult data set
##            Note: check itemLabels(Adult) to see the available labels for items
twoTransactions <- as(
  encode(list(
    c("age=Young", "relationship=Unmarried"),
    c("age=Senior")
  ), itemLabels = Adult),
  "transactions"
)

twoTransactions
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  115 items (columns)
inspect(twoTransactions)
#>     items                              
#> [1] {age=Young, relationship=Unmarried}
#> [2] {age=Senior}                       

## the same using the transactions constructor function instead
twoTransactions <- transactions(
  list(
    c("age=Young", "relationship=Unmarried"),
    c("age=Senior")
  ),
  itemLabels = Adult
)

twoTransactions
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  115 items (columns)
inspect(twoTransactions)
#>     items                              
#> [1] {age=Young, relationship=Unmarried}
#> [2] {age=Senior}                       

## Example 5: Use a common item coding

# Creation of transactions separately will produce different item codings
trans1 <- transactions(
  list(
    c("age=Young", "relationship=Unmarried"),
    c("age=Senior")
  )
)
trans1
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  3 items (columns)

trans2 <- transactions(
  list(
    c("age=Middle-aged", "relationship=Married"),
    c("relationship=Unmarried", "age=Young")
  )
)
trans2
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  4 items (columns)

compatible(trans1, trans2)
#> [1] FALSE

# produce common item coding (all item labels in the two sets)
commonItemLabels <- union(itemLabels(trans1), itemLabels(trans2))
commonItemLabels
#> [1] "age=Senior"             "age=Young"              "relationship=Unmarried"
#> [4] "age=Middle-aged"        "relationship=Married"  

trans1 <- recode(trans1, itemLabels = commonItemLabels)
trans1
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  5 items (columns)
trans2 <- recode(trans2, itemLabels = commonItemLabels)
trans2
#> transactions in sparse format with
#>  2 transactions (rows) and
#>  5 items (columns)

compatible(trans1, trans2)
#> [1] TRUE


## Example 6: manually create a rule using the item coding in Adult
## and calculate interest measures
aRule <- new("rules",
  lhs = encode(list(c("age=Young", "relationship=Unmarried")),
    itemLabels = Adult
  ),
  rhs = encode(list(c("income=small")),
    itemLabels = Adult
  )
)

## shorter version using the rules constructor
aRule <- rules(
  lhs = list(c("age=Young", "relationship=Unmarried")),
  rhs = list(c("income=small")),
  itemLabels = Adult
)

quality(aRule) <- interestMeasure(aRule,
  measure = c("support", "confidence", "lift"), transactions = Adult
)

inspect(aRule)
#>     lhs                                    rhs            support    
#> [1] {age=Young, relationship=Unmarried} => {income=small} 0.006940748
#>     confidence lift    
#> [1] 0.6608187  1.305652
```
