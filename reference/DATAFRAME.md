# Data.frame Representation for arules Objects

Provides the generic function `DATAFRAME()` and the methods to create a
data.frame representation from some arules objects. These methods are
used for the coercion to a data.frame, but offer more control over the
coercion process (item separators, etc.).

## Usage

``` r
DATAFRAME(from, ...)

# S4 method for class 'rules'
DATAFRAME(from, separate = TRUE, ...)

# S4 method for class 'itemsets'
DATAFRAME(from, ...)

# S4 method for class 'itemMatrix'
DATAFRAME(from, ...)
```

## Arguments

- from:

  the object to be converted into a data.frame.

- ...:

  further arguments are passed on to the
  [`labels()`](https://rdrr.io/r/base/labels.html) method defined for
  the object in `from`.

- separate:

  logical; separate LHS and RHS in separate columns? (only for rules)

## Value

a data.frame.

## Details

Using `DATAFRAME()` is equivalent to the standard coercion
`as(x, "data.frame")`. However, for rules, the argument
`separate = TRUE` will produce separate columns for the LHS and the RHS
of the rule.

Furthermore, the arguments `itemSep`, `setStart`, `setEnd` (and
`ruleSep` for `separate = FALSE`) will be passed on to the
[`labels()`](https://rdrr.io/r/base/labels.html) method for the object
specified in `from`.

## See also

Other import/export:
[`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md),
[`pmml`](http://michael.hahsler.net/arules/reference/pmml.md),
[`read`](http://michael.hahsler.net/arules/reference/read.md),
[`write()`](http://michael.hahsler.net/arules/reference/write.md)

## Author

Michael Hahsler

## Examples

``` r
data(Adult)

DATAFRAME(head(Adult))
#>                                                                                                                                                                                                                                                                      items
#> 1      {age=Middle-aged,workclass=State-gov,education=Bachelors,marital-status=Never-married,occupation=Adm-clerical,relationship=Not-in-family,race=White,sex=Male,capital-gain=Low,capital-loss=None,hours-per-week=Full-time,native-country=United-States,income=small}
#> 2 {age=Senior,workclass=Self-emp-not-inc,education=Bachelors,marital-status=Married-civ-spouse,occupation=Exec-managerial,relationship=Husband,race=White,sex=Male,capital-gain=None,capital-loss=None,hours-per-week=Part-time,native-country=United-States,income=small}
#> 3         {age=Middle-aged,workclass=Private,education=HS-grad,marital-status=Divorced,occupation=Handlers-cleaners,relationship=Not-in-family,race=White,sex=Male,capital-gain=None,capital-loss=None,hours-per-week=Full-time,native-country=United-States,income=small}
#> 4             {age=Senior,workclass=Private,education=11th,marital-status=Married-civ-spouse,occupation=Handlers-cleaners,relationship=Husband,race=Black,sex=Male,capital-gain=None,capital-loss=None,hours-per-week=Full-time,native-country=United-States,income=small}
#> 5                {age=Middle-aged,workclass=Private,education=Bachelors,marital-status=Married-civ-spouse,occupation=Prof-specialty,relationship=Wife,race=Black,sex=Female,capital-gain=None,capital-loss=None,hours-per-week=Full-time,native-country=Cuba,income=small}
#> 6        {age=Middle-aged,workclass=Private,education=Masters,marital-status=Married-civ-spouse,occupation=Exec-managerial,relationship=Wife,race=White,sex=Female,capital-gain=None,capital-loss=None,hours-per-week=Full-time,native-country=United-States,income=small}
#>   transactionID
#> 1             1
#> 2             2
#> 3             3
#> 4             4
#> 5             5
#> 6             6
DATAFRAME(head(Adult), setStart = "", itemSep = " + ", setEnd = "")
#>                                                                                                                                                                                                                                                                                            items
#> 1      age=Middle-aged + workclass=State-gov + education=Bachelors + marital-status=Never-married + occupation=Adm-clerical + relationship=Not-in-family + race=White + sex=Male + capital-gain=Low + capital-loss=None + hours-per-week=Full-time + native-country=United-States + income=small
#> 2 age=Senior + workclass=Self-emp-not-inc + education=Bachelors + marital-status=Married-civ-spouse + occupation=Exec-managerial + relationship=Husband + race=White + sex=Male + capital-gain=None + capital-loss=None + hours-per-week=Part-time + native-country=United-States + income=small
#> 3         age=Middle-aged + workclass=Private + education=HS-grad + marital-status=Divorced + occupation=Handlers-cleaners + relationship=Not-in-family + race=White + sex=Male + capital-gain=None + capital-loss=None + hours-per-week=Full-time + native-country=United-States + income=small
#> 4             age=Senior + workclass=Private + education=11th + marital-status=Married-civ-spouse + occupation=Handlers-cleaners + relationship=Husband + race=Black + sex=Male + capital-gain=None + capital-loss=None + hours-per-week=Full-time + native-country=United-States + income=small
#> 5                age=Middle-aged + workclass=Private + education=Bachelors + marital-status=Married-civ-spouse + occupation=Prof-specialty + relationship=Wife + race=Black + sex=Female + capital-gain=None + capital-loss=None + hours-per-week=Full-time + native-country=Cuba + income=small
#> 6        age=Middle-aged + workclass=Private + education=Masters + marital-status=Married-civ-spouse + occupation=Exec-managerial + relationship=Wife + race=White + sex=Female + capital-gain=None + capital-loss=None + hours-per-week=Full-time + native-country=United-States + income=small
#>   transactionID
#> 1             1
#> 2             2
#> 3             3
#> 4             4
#> 5             5
#> 6             6

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
#> creating transaction tree ... done [0.01s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> writing ... [52 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
rules <- head(rules, by = "conf")


### default coercions (same as as(rules, "data.frame"))
DATAFRAME(rules)
#>                                                            LHS
#> 4                                   {hours-per-week=Full-time}
#> 8                                          {workclass=Private}
#> 29            {workclass=Private,native-country=United-States}
#> 16                {capital-gain=None,hours-per-week=Full-time}
#> 27                              {workclass=Private,race=White}
#> 44 {workclass=Private,race=White,native-country=United-States}
#>                    RHS   support confidence  coverage     lift count
#> 4  {capital-loss=None} 0.5606650  0.9582531 0.5850907 1.005219 27384
#> 8  {capital-loss=None} 0.6639982  0.9564974 0.6941976 1.003377 32431
#> 29 {capital-loss=None} 0.5897179  0.9554818 0.6171942 1.002312 28803
#> 16 {capital-loss=None} 0.5191638  0.9550659 0.5435895 1.001876 25357
#> 27 {capital-loss=None} 0.5674829  0.9549683 0.5942427 1.001773 27717
#> 44 {capital-loss=None} 0.5181401  0.9535418 0.5433848 1.000277 25307

DATAFRAME(rules, separate = TRUE)
#>                                                            LHS
#> 4                                   {hours-per-week=Full-time}
#> 8                                          {workclass=Private}
#> 29            {workclass=Private,native-country=United-States}
#> 16                {capital-gain=None,hours-per-week=Full-time}
#> 27                              {workclass=Private,race=White}
#> 44 {workclass=Private,race=White,native-country=United-States}
#>                    RHS   support confidence  coverage     lift count
#> 4  {capital-loss=None} 0.5606650  0.9582531 0.5850907 1.005219 27384
#> 8  {capital-loss=None} 0.6639982  0.9564974 0.6941976 1.003377 32431
#> 29 {capital-loss=None} 0.5897179  0.9554818 0.6171942 1.002312 28803
#> 16 {capital-loss=None} 0.5191638  0.9550659 0.5435895 1.001876 25357
#> 27 {capital-loss=None} 0.5674829  0.9549683 0.5942427 1.001773 27717
#> 44 {capital-loss=None} 0.5181401  0.9535418 0.5433848 1.000277 25307
DATAFRAME(rules, separate = TRUE, setStart = "", itemSep = " + ", setEnd = "")
#>                                                              LHS
#> 4                                       hours-per-week=Full-time
#> 8                                              workclass=Private
#> 29              workclass=Private + native-country=United-States
#> 16                  capital-gain=None + hours-per-week=Full-time
#> 27                                workclass=Private + race=White
#> 44 workclass=Private + race=White + native-country=United-States
#>                  RHS   support confidence  coverage     lift count
#> 4  capital-loss=None 0.5606650  0.9582531 0.5850907 1.005219 27384
#> 8  capital-loss=None 0.6639982  0.9564974 0.6941976 1.003377 32431
#> 29 capital-loss=None 0.5897179  0.9554818 0.6171942 1.002312 28803
#> 16 capital-loss=None 0.5191638  0.9550659 0.5435895 1.001876 25357
#> 27 capital-loss=None 0.5674829  0.9549683 0.5942427 1.001773 27717
#> 44 capital-loss=None 0.5181401  0.9535418 0.5433848 1.000277 25307
```
