# List Representation for Objects Based on Class itemMatrix

Provides the generic function `LIST()` and the methods to create a list
representation from objects of the classes
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md),
and
[tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md).

## Usage

``` r
LIST(from, ...)

# S4 method for class 'itemMatrix'
LIST(from, decode = TRUE)

# S4 method for class 'transactions'
LIST(from, decode = TRUE)

# S4 method for class 'tidLists'
LIST(from, decode = TRUE)
```

## Arguments

- from:

  the object to be converted into a list.

- ...:

  further arguments.

- decode:

  a logical controlling whether the items/transactions are decoded from
  the column numbers internally used by
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  to the names stored in the object `from`. The default behavior is to
  decode.

## Value

a list primitive.

## Details

Using `LIST()` with `decode = TRUE` is equivalent to the standard
coercion `as(x, "list")`. `LIST` returns the object `from` as a list of
vectors. Each vector represents one row of the
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
(e.g., items in a transaction).

## See also

Other import/export:
[`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md),
[`pmml`](http://michael.hahsler.net/arules/reference/pmml.md),
[`read`](http://michael.hahsler.net/arules/reference/read.md),
[`write()`](http://michael.hahsler.net/arules/reference/write.md)

## Author

Michael Hahsler

## Examples

``` r
data(Adult)

### default coercion (same as as(Adult[1:5], "list"))
LIST(Adult[1:5])
#> $`1`
#>  [1] "age=Middle-aged"              "workclass=State-gov"         
#>  [3] "education=Bachelors"          "marital-status=Never-married"
#>  [5] "occupation=Adm-clerical"      "relationship=Not-in-family"  
#>  [7] "race=White"                   "sex=Male"                    
#>  [9] "capital-gain=Low"             "capital-loss=None"           
#> [11] "hours-per-week=Full-time"     "native-country=United-States"
#> [13] "income=small"                
#> 
#> $`2`
#>  [1] "age=Senior"                        "workclass=Self-emp-not-inc"       
#>  [3] "education=Bachelors"               "marital-status=Married-civ-spouse"
#>  [5] "occupation=Exec-managerial"        "relationship=Husband"             
#>  [7] "race=White"                        "sex=Male"                         
#>  [9] "capital-gain=None"                 "capital-loss=None"                
#> [11] "hours-per-week=Part-time"          "native-country=United-States"     
#> [13] "income=small"                     
#> 
#> $`3`
#>  [1] "age=Middle-aged"              "workclass=Private"           
#>  [3] "education=HS-grad"            "marital-status=Divorced"     
#>  [5] "occupation=Handlers-cleaners" "relationship=Not-in-family"  
#>  [7] "race=White"                   "sex=Male"                    
#>  [9] "capital-gain=None"            "capital-loss=None"           
#> [11] "hours-per-week=Full-time"     "native-country=United-States"
#> [13] "income=small"                
#> 
#> $`4`
#>  [1] "age=Senior"                        "workclass=Private"                
#>  [3] "education=11th"                    "marital-status=Married-civ-spouse"
#>  [5] "occupation=Handlers-cleaners"      "relationship=Husband"             
#>  [7] "race=Black"                        "sex=Male"                         
#>  [9] "capital-gain=None"                 "capital-loss=None"                
#> [11] "hours-per-week=Full-time"          "native-country=United-States"     
#> [13] "income=small"                     
#> 
#> $`5`
#>  [1] "age=Middle-aged"                   "workclass=Private"                
#>  [3] "education=Bachelors"               "marital-status=Married-civ-spouse"
#>  [5] "occupation=Prof-specialty"         "relationship=Wife"                
#>  [7] "race=Black"                        "sex=Female"                       
#>  [9] "capital-gain=None"                 "capital-loss=None"                
#> [11] "hours-per-week=Full-time"          "native-country=Cuba"              
#> [13] "income=small"                     
#> 

### coercion without item decoding
LIST(Adult[1:5], decode = FALSE)
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
```
