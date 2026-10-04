# Class itemsets — A Set of Itemsets

The `itemsets` class represents a set of itemsets and the associated
quality measures.

## Usage

``` r
itemsets(items, itemLabels = NULL, quality = data.frame())

# S4 method for class 'itemsets'
summary(object, ...)

# S4 method for class 'itemsets'
length(x)

# S4 method for class 'itemsets'
nitems(x)

# S4 method for class 'itemsets'
labels(object, ...)

# S4 method for class 'itemsets'
itemLabels(object)

# S4 method for class 'itemsets'
itemLabels(object) <- value

# S4 method for class 'itemsets'
itemInfo(object)

# S4 method for class 'itemsets'
items(x)

# S4 method for class 'itemsets'
items(x) <- value

# S4 method for class 'itemsets'
tidLists(x)
```

## Arguments

- items:

  an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  or an object that can be converted using
  [`encode()`](http://michael.hahsler.net/arules/reference/itemCoding.md).

- itemLabels:

  item labels used for
  [`encode()`](http://michael.hahsler.net/arules/reference/itemCoding.md).

- quality:

  a data.frame with quality information (one row per itemset).

- object, x:

  the object

- ...:

  further argments

- value:

  replacement value

## Details

Itemsets are usually created by calling an association rule mining
algorithm like
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md).
To create itemsets manually, the itemMatrix for the items of the
itemsets can be created using
[itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md).
An example is in the Example section below.

Mined itemsets sets contain several interest measures accessible with
the
[`quality()`](http://michael.hahsler.net/arules/reference/associations-class.md)
method. Additional measures can be calculated via
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md).

## Functions

- `summary(itemsets)`: create a summary

- `length(itemsets)`: get the number of itemsets.

- `nitems(itemsets)`: get the number of items (columns) in the current
  encoding.

- `labels(itemsets)`: get the itemset labels.

- `itemLabels(itemsets)`: get the item labels.

- `itemLabels(itemsets) <- value`: replace the item labels.

- `itemInfo(itemsets)`: get item info data.frame.

- `items(itemsets)`: get items as an itemMatrix.

- `items(itemsets) <- value`: with a different itemMatrix.

- `tidLists(itemsets)`: get tidLists stored in the object (if any).

## Slots

- `items`:

  an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  object representing the itemsets.

- `tidLists`:

  a
  [tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  or `NULL`.

- `quality`:

  a data.frame with quality information

- `info`:

  a list with mining information.

## Objects from the Class

Objects are the result of calling the functions
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
(e.g., with `target = "frequent itemsets"` in the parameter list) or
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md).

Objects can also be created by calls of the form `new("itemsets", ...)`
or by using the constructor function `itemsets()`.

## Coercions

- `as("itemsets", "data.frame")`

## See also

Superclass:
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
data("Adult")

## Mine frequent itemsets with Eclat.
fsets <- eclat(Adult, parameter = list(supp = 0.5))
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.5      1     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 24421 
#> 
#> create itemset ... 
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.03s].
#> sorting and recoding items ... [9 item(s)] done [0.00s].
#> creating bit matrix ... [9 row(s), 48842 column(s)] done [0.00s].
#> writing  ... [49 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].

## Display the 5 itemsets with the highest support.
fsets.top5 <- sort(fsets)[1:5]
inspect(fsets.top5)
#>     items                                  support   count
#> [1] {capital-loss=None}                    0.9532779 46560
#> [2] {capital-gain=None}                    0.9173867 44807
#> [3] {native-country=United-States}         0.8974243 43832
#> [4] {capital-gain=None, capital-loss=None} 0.8706646 42525
#> [5] {race=White}                           0.8550428 41762

## Get the itemsets as a list
as(items(fsets.top5), "list")
#> [[1]]
#> [1] "capital-loss=None"
#> 
#> [[2]]
#> [1] "capital-gain=None"
#> 
#> [[3]]
#> [1] "native-country=United-States"
#> 
#> [[4]]
#> [1] "capital-gain=None" "capital-loss=None"
#> 
#> [[5]]
#> [1] "race=White"
#> 

## Get the itemsets as a binary matrix
as(items(fsets.top5), "matrix")
#>      age=Young age=Middle-aged age=Senior age=Old workclass=Federal-gov
#> [1,]     FALSE           FALSE      FALSE   FALSE                 FALSE
#> [2,]     FALSE           FALSE      FALSE   FALSE                 FALSE
#> [3,]     FALSE           FALSE      FALSE   FALSE                 FALSE
#> [4,]     FALSE           FALSE      FALSE   FALSE                 FALSE
#> [5,]     FALSE           FALSE      FALSE   FALSE                 FALSE
#>      workclass=Local-gov workclass=Never-worked workclass=Private
#> [1,]               FALSE                  FALSE             FALSE
#> [2,]               FALSE                  FALSE             FALSE
#> [3,]               FALSE                  FALSE             FALSE
#> [4,]               FALSE                  FALSE             FALSE
#> [5,]               FALSE                  FALSE             FALSE
#>      workclass=Self-emp-inc workclass=Self-emp-not-inc workclass=State-gov
#> [1,]                  FALSE                      FALSE               FALSE
#> [2,]                  FALSE                      FALSE               FALSE
#> [3,]                  FALSE                      FALSE               FALSE
#> [4,]                  FALSE                      FALSE               FALSE
#> [5,]                  FALSE                      FALSE               FALSE
#>      workclass=Without-pay education=Preschool education=1st-4th
#> [1,]                 FALSE               FALSE             FALSE
#> [2,]                 FALSE               FALSE             FALSE
#> [3,]                 FALSE               FALSE             FALSE
#> [4,]                 FALSE               FALSE             FALSE
#> [5,]                 FALSE               FALSE             FALSE
#>      education=5th-6th education=7th-8th education=9th education=10th
#> [1,]             FALSE             FALSE         FALSE          FALSE
#> [2,]             FALSE             FALSE         FALSE          FALSE
#> [3,]             FALSE             FALSE         FALSE          FALSE
#> [4,]             FALSE             FALSE         FALSE          FALSE
#> [5,]             FALSE             FALSE         FALSE          FALSE
#>      education=11th education=12th education=HS-grad education=Prof-school
#> [1,]          FALSE          FALSE             FALSE                 FALSE
#> [2,]          FALSE          FALSE             FALSE                 FALSE
#> [3,]          FALSE          FALSE             FALSE                 FALSE
#> [4,]          FALSE          FALSE             FALSE                 FALSE
#> [5,]          FALSE          FALSE             FALSE                 FALSE
#>      education=Assoc-acdm education=Assoc-voc education=Some-college
#> [1,]                FALSE               FALSE                  FALSE
#> [2,]                FALSE               FALSE                  FALSE
#> [3,]                FALSE               FALSE                  FALSE
#> [4,]                FALSE               FALSE                  FALSE
#> [5,]                FALSE               FALSE                  FALSE
#>      education=Bachelors education=Masters education=Doctorate
#> [1,]               FALSE             FALSE               FALSE
#> [2,]               FALSE             FALSE               FALSE
#> [3,]               FALSE             FALSE               FALSE
#> [4,]               FALSE             FALSE               FALSE
#> [5,]               FALSE             FALSE               FALSE
#>      marital-status=Divorced marital-status=Married-AF-spouse
#> [1,]                   FALSE                            FALSE
#> [2,]                   FALSE                            FALSE
#> [3,]                   FALSE                            FALSE
#> [4,]                   FALSE                            FALSE
#> [5,]                   FALSE                            FALSE
#>      marital-status=Married-civ-spouse marital-status=Married-spouse-absent
#> [1,]                             FALSE                                FALSE
#> [2,]                             FALSE                                FALSE
#> [3,]                             FALSE                                FALSE
#> [4,]                             FALSE                                FALSE
#> [5,]                             FALSE                                FALSE
#>      marital-status=Never-married marital-status=Separated
#> [1,]                        FALSE                    FALSE
#> [2,]                        FALSE                    FALSE
#> [3,]                        FALSE                    FALSE
#> [4,]                        FALSE                    FALSE
#> [5,]                        FALSE                    FALSE
#>      marital-status=Widowed occupation=Adm-clerical occupation=Armed-Forces
#> [1,]                  FALSE                   FALSE                   FALSE
#> [2,]                  FALSE                   FALSE                   FALSE
#> [3,]                  FALSE                   FALSE                   FALSE
#> [4,]                  FALSE                   FALSE                   FALSE
#> [5,]                  FALSE                   FALSE                   FALSE
#>      occupation=Craft-repair occupation=Exec-managerial
#> [1,]                   FALSE                      FALSE
#> [2,]                   FALSE                      FALSE
#> [3,]                   FALSE                      FALSE
#> [4,]                   FALSE                      FALSE
#> [5,]                   FALSE                      FALSE
#>      occupation=Farming-fishing occupation=Handlers-cleaners
#> [1,]                      FALSE                        FALSE
#> [2,]                      FALSE                        FALSE
#> [3,]                      FALSE                        FALSE
#> [4,]                      FALSE                        FALSE
#> [5,]                      FALSE                        FALSE
#>      occupation=Machine-op-inspct occupation=Other-service
#> [1,]                        FALSE                    FALSE
#> [2,]                        FALSE                    FALSE
#> [3,]                        FALSE                    FALSE
#> [4,]                        FALSE                    FALSE
#> [5,]                        FALSE                    FALSE
#>      occupation=Priv-house-serv occupation=Prof-specialty
#> [1,]                      FALSE                     FALSE
#> [2,]                      FALSE                     FALSE
#> [3,]                      FALSE                     FALSE
#> [4,]                      FALSE                     FALSE
#> [5,]                      FALSE                     FALSE
#>      occupation=Protective-serv occupation=Sales occupation=Tech-support
#> [1,]                      FALSE            FALSE                   FALSE
#> [2,]                      FALSE            FALSE                   FALSE
#> [3,]                      FALSE            FALSE                   FALSE
#> [4,]                      FALSE            FALSE                   FALSE
#> [5,]                      FALSE            FALSE                   FALSE
#>      occupation=Transport-moving relationship=Husband
#> [1,]                       FALSE                FALSE
#> [2,]                       FALSE                FALSE
#> [3,]                       FALSE                FALSE
#> [4,]                       FALSE                FALSE
#> [5,]                       FALSE                FALSE
#>      relationship=Not-in-family relationship=Other-relative
#> [1,]                      FALSE                       FALSE
#> [2,]                      FALSE                       FALSE
#> [3,]                      FALSE                       FALSE
#> [4,]                      FALSE                       FALSE
#> [5,]                      FALSE                       FALSE
#>      relationship=Own-child relationship=Unmarried relationship=Wife
#> [1,]                  FALSE                  FALSE             FALSE
#> [2,]                  FALSE                  FALSE             FALSE
#> [3,]                  FALSE                  FALSE             FALSE
#> [4,]                  FALSE                  FALSE             FALSE
#> [5,]                  FALSE                  FALSE             FALSE
#>      race=Amer-Indian-Eskimo race=Asian-Pac-Islander race=Black race=Other
#> [1,]                   FALSE                   FALSE      FALSE      FALSE
#> [2,]                   FALSE                   FALSE      FALSE      FALSE
#> [3,]                   FALSE                   FALSE      FALSE      FALSE
#> [4,]                   FALSE                   FALSE      FALSE      FALSE
#> [5,]                   FALSE                   FALSE      FALSE      FALSE
#>      race=White sex=Female sex=Male capital-gain=None capital-gain=Low
#> [1,]      FALSE      FALSE    FALSE             FALSE            FALSE
#> [2,]      FALSE      FALSE    FALSE              TRUE            FALSE
#> [3,]      FALSE      FALSE    FALSE             FALSE            FALSE
#> [4,]      FALSE      FALSE    FALSE              TRUE            FALSE
#> [5,]       TRUE      FALSE    FALSE             FALSE            FALSE
#>      capital-gain=High capital-loss=None capital-loss=Low capital-loss=High
#> [1,]             FALSE              TRUE            FALSE             FALSE
#> [2,]             FALSE             FALSE            FALSE             FALSE
#> [3,]             FALSE             FALSE            FALSE             FALSE
#> [4,]             FALSE              TRUE            FALSE             FALSE
#> [5,]             FALSE             FALSE            FALSE             FALSE
#>      hours-per-week=Part-time hours-per-week=Full-time hours-per-week=Over-time
#> [1,]                    FALSE                    FALSE                    FALSE
#> [2,]                    FALSE                    FALSE                    FALSE
#> [3,]                    FALSE                    FALSE                    FALSE
#> [4,]                    FALSE                    FALSE                    FALSE
#> [5,]                    FALSE                    FALSE                    FALSE
#>      hours-per-week=Workaholic native-country=Cambodia native-country=Canada
#> [1,]                     FALSE                   FALSE                 FALSE
#> [2,]                     FALSE                   FALSE                 FALSE
#> [3,]                     FALSE                   FALSE                 FALSE
#> [4,]                     FALSE                   FALSE                 FALSE
#> [5,]                     FALSE                   FALSE                 FALSE
#>      native-country=China native-country=Columbia native-country=Cuba
#> [1,]                FALSE                   FALSE               FALSE
#> [2,]                FALSE                   FALSE               FALSE
#> [3,]                FALSE                   FALSE               FALSE
#> [4,]                FALSE                   FALSE               FALSE
#> [5,]                FALSE                   FALSE               FALSE
#>      native-country=Dominican-Republic native-country=Ecuador
#> [1,]                             FALSE                  FALSE
#> [2,]                             FALSE                  FALSE
#> [3,]                             FALSE                  FALSE
#> [4,]                             FALSE                  FALSE
#> [5,]                             FALSE                  FALSE
#>      native-country=El-Salvador native-country=England native-country=France
#> [1,]                      FALSE                  FALSE                 FALSE
#> [2,]                      FALSE                  FALSE                 FALSE
#> [3,]                      FALSE                  FALSE                 FALSE
#> [4,]                      FALSE                  FALSE                 FALSE
#> [5,]                      FALSE                  FALSE                 FALSE
#>      native-country=Germany native-country=Greece native-country=Guatemala
#> [1,]                  FALSE                 FALSE                    FALSE
#> [2,]                  FALSE                 FALSE                    FALSE
#> [3,]                  FALSE                 FALSE                    FALSE
#> [4,]                  FALSE                 FALSE                    FALSE
#> [5,]                  FALSE                 FALSE                    FALSE
#>      native-country=Haiti native-country=Holand-Netherlands
#> [1,]                FALSE                             FALSE
#> [2,]                FALSE                             FALSE
#> [3,]                FALSE                             FALSE
#> [4,]                FALSE                             FALSE
#> [5,]                FALSE                             FALSE
#>      native-country=Honduras native-country=Hong native-country=Hungary
#> [1,]                   FALSE               FALSE                  FALSE
#> [2,]                   FALSE               FALSE                  FALSE
#> [3,]                   FALSE               FALSE                  FALSE
#> [4,]                   FALSE               FALSE                  FALSE
#> [5,]                   FALSE               FALSE                  FALSE
#>      native-country=India native-country=Iran native-country=Ireland
#> [1,]                FALSE               FALSE                  FALSE
#> [2,]                FALSE               FALSE                  FALSE
#> [3,]                FALSE               FALSE                  FALSE
#> [4,]                FALSE               FALSE                  FALSE
#> [5,]                FALSE               FALSE                  FALSE
#>      native-country=Italy native-country=Jamaica native-country=Japan
#> [1,]                FALSE                  FALSE                FALSE
#> [2,]                FALSE                  FALSE                FALSE
#> [3,]                FALSE                  FALSE                FALSE
#> [4,]                FALSE                  FALSE                FALSE
#> [5,]                FALSE                  FALSE                FALSE
#>      native-country=Laos native-country=Mexico native-country=Nicaragua
#> [1,]               FALSE                 FALSE                    FALSE
#> [2,]               FALSE                 FALSE                    FALSE
#> [3,]               FALSE                 FALSE                    FALSE
#> [4,]               FALSE                 FALSE                    FALSE
#> [5,]               FALSE                 FALSE                    FALSE
#>      native-country=Outlying-US(Guam-USVI-etc) native-country=Peru
#> [1,]                                     FALSE               FALSE
#> [2,]                                     FALSE               FALSE
#> [3,]                                     FALSE               FALSE
#> [4,]                                     FALSE               FALSE
#> [5,]                                     FALSE               FALSE
#>      native-country=Philippines native-country=Poland native-country=Portugal
#> [1,]                      FALSE                 FALSE                   FALSE
#> [2,]                      FALSE                 FALSE                   FALSE
#> [3,]                      FALSE                 FALSE                   FALSE
#> [4,]                      FALSE                 FALSE                   FALSE
#> [5,]                      FALSE                 FALSE                   FALSE
#>      native-country=Puerto-Rico native-country=Scotland native-country=South
#> [1,]                      FALSE                   FALSE                FALSE
#> [2,]                      FALSE                   FALSE                FALSE
#> [3,]                      FALSE                   FALSE                FALSE
#> [4,]                      FALSE                   FALSE                FALSE
#> [5,]                      FALSE                   FALSE                FALSE
#>      native-country=Taiwan native-country=Thailand
#> [1,]                 FALSE                   FALSE
#> [2,]                 FALSE                   FALSE
#> [3,]                 FALSE                   FALSE
#> [4,]                 FALSE                   FALSE
#> [5,]                 FALSE                   FALSE
#>      native-country=Trinadad&Tobago native-country=United-States
#> [1,]                          FALSE                        FALSE
#> [2,]                          FALSE                        FALSE
#> [3,]                          FALSE                         TRUE
#> [4,]                          FALSE                        FALSE
#> [5,]                          FALSE                        FALSE
#>      native-country=Vietnam native-country=Yugoslavia income=small income=large
#> [1,]                  FALSE                     FALSE        FALSE        FALSE
#> [2,]                  FALSE                     FALSE        FALSE        FALSE
#> [3,]                  FALSE                     FALSE        FALSE        FALSE
#> [4,]                  FALSE                     FALSE        FALSE        FALSE
#> [5,]                  FALSE                     FALSE        FALSE        FALSE

## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed
as(items(fsets.top5), "ngCMatrix")
#> 115 x 5 sparse Matrix of class "ngCMatrix"
#>                                                    
#> age=Young                                 . . . . .
#> age=Middle-aged                           . . . . .
#> age=Senior                                . . . . .
#> age=Old                                   . . . . .
#> workclass=Federal-gov                     . . . . .
#> workclass=Local-gov                       . . . . .
#> workclass=Never-worked                    . . . . .
#> workclass=Private                         . . . . .
#> workclass=Self-emp-inc                    . . . . .
#> workclass=Self-emp-not-inc                . . . . .
#> workclass=State-gov                       . . . . .
#> workclass=Without-pay                     . . . . .
#> education=Preschool                       . . . . .
#> education=1st-4th                         . . . . .
#> education=5th-6th                         . . . . .
#> education=7th-8th                         . . . . .
#> education=9th                             . . . . .
#> education=10th                            . . . . .
#> education=11th                            . . . . .
#> education=12th                            . . . . .
#> education=HS-grad                         . . . . .
#> education=Prof-school                     . . . . .
#> education=Assoc-acdm                      . . . . .
#> education=Assoc-voc                       . . . . .
#> education=Some-college                    . . . . .
#> education=Bachelors                       . . . . .
#> education=Masters                         . . . . .
#> education=Doctorate                       . . . . .
#> marital-status=Divorced                   . . . . .
#> marital-status=Married-AF-spouse          . . . . .
#> marital-status=Married-civ-spouse         . . . . .
#> marital-status=Married-spouse-absent      . . . . .
#> marital-status=Never-married              . . . . .
#> marital-status=Separated                  . . . . .
#> marital-status=Widowed                    . . . . .
#> occupation=Adm-clerical                   . . . . .
#> occupation=Armed-Forces                   . . . . .
#> occupation=Craft-repair                   . . . . .
#> occupation=Exec-managerial                . . . . .
#> occupation=Farming-fishing                . . . . .
#> occupation=Handlers-cleaners              . . . . .
#> occupation=Machine-op-inspct              . . . . .
#> occupation=Other-service                  . . . . .
#> occupation=Priv-house-serv                . . . . .
#> occupation=Prof-specialty                 . . . . .
#> occupation=Protective-serv                . . . . .
#> occupation=Sales                          . . . . .
#> occupation=Tech-support                   . . . . .
#> occupation=Transport-moving               . . . . .
#> relationship=Husband                      . . . . .
#> relationship=Not-in-family                . . . . .
#> relationship=Other-relative               . . . . .
#> relationship=Own-child                    . . . . .
#> relationship=Unmarried                    . . . . .
#> relationship=Wife                         . . . . .
#> race=Amer-Indian-Eskimo                   . . . . .
#> race=Asian-Pac-Islander                   . . . . .
#> race=Black                                . . . . .
#> race=Other                                . . . . .
#> race=White                                . . . . |
#> sex=Female                                . . . . .
#> sex=Male                                  . . . . .
#> capital-gain=None                         . | . | .
#> capital-gain=Low                          . . . . .
#> capital-gain=High                         . . . . .
#> capital-loss=None                         | . . | .
#> capital-loss=Low                          . . . . .
#> capital-loss=High                         . . . . .
#> hours-per-week=Part-time                  . . . . .
#> hours-per-week=Full-time                  . . . . .
#> hours-per-week=Over-time                  . . . . .
#> hours-per-week=Workaholic                 . . . . .
#> native-country=Cambodia                   . . . . .
#> native-country=Canada                     . . . . .
#> native-country=China                      . . . . .
#> native-country=Columbia                   . . . . .
#> native-country=Cuba                       . . . . .
#> native-country=Dominican-Republic         . . . . .
#> native-country=Ecuador                    . . . . .
#> native-country=El-Salvador                . . . . .
#> native-country=England                    . . . . .
#> native-country=France                     . . . . .
#> native-country=Germany                    . . . . .
#> native-country=Greece                     . . . . .
#> native-country=Guatemala                  . . . . .
#> native-country=Haiti                      . . . . .
#> native-country=Holand-Netherlands         . . . . .
#> native-country=Honduras                   . . . . .
#> native-country=Hong                       . . . . .
#> native-country=Hungary                    . . . . .
#> native-country=India                      . . . . .
#> native-country=Iran                       . . . . .
#> native-country=Ireland                    . . . . .
#> native-country=Italy                      . . . . .
#> native-country=Jamaica                    . . . . .
#> native-country=Japan                      . . . . .
#> native-country=Laos                       . . . . .
#> native-country=Mexico                     . . . . .
#> native-country=Nicaragua                  . . . . .
#> native-country=Outlying-US(Guam-USVI-etc) . . . . .
#> native-country=Peru                       . . . . .
#> native-country=Philippines                . . . . .
#> native-country=Poland                     . . . . .
#> native-country=Portugal                   . . . . .
#> native-country=Puerto-Rico                . . . . .
#> native-country=Scotland                   . . . . .
#> native-country=South                      . . . . .
#> native-country=Taiwan                     . . . . .
#> native-country=Thailand                   . . . . .
#> native-country=Trinadad&Tobago            . . . . .
#> native-country=United-States              . . | . .
#> native-country=Vietnam                    . . . . .
#> native-country=Yugoslavia                 . . . . .
#> income=small                              . . . . .
#> income=large                              . . . . .

## Manually create itemsets with the item coding in the Adult dataset
## and calculate some interest measures
twoitemsets <- itemsets(
  items = list(
    c("age=Young", "relationship=Unmarried"),
    c("age=Old")
  ), itemLabels = Adult
)

quality(twoitemsets) <- data.frame(support = interestMeasure(twoitemsets,
  measure = c("support"), transactions = Adult
))

inspect(twoitemsets)
#>     items                               support   
#> [1] {age=Young, relationship=Unmarried} 0.01050326
#> [2] {age=Old}                           0.03691495
```
