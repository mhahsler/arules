# Getting Frequency/Support for Single Items

Provides the generic function `itemFrequency()` and methods to get the
frequency/support for all single items in an objects based on
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).
For example, it is used to get the single item support from an object of
class
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
without mining.

## Usage

``` r
itemFrequency(x, ...)

# S4 method for class 'itemMatrix'
itemFrequency(x, type = c("relative", "absolute"), weighted = FALSE)

# S4 method for class 'tidLists'
itemFrequency(x, type = c("relative", "absolute"))
```

## Arguments

- x:

  an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  or
  [tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md).

- ...:

  further arguments are passed on.

- type:

  a character string specifying if `"relative"` frequency/support or
  `"absolute"` frequency/support (item counts) is returned. (default:
  `"relative"`).

- weighted:

  should support be weighted by transactions weights stored as column
  `"weight"` in transactionInfo?

## Value

`itemFrequency` returns a named numeric vector. Each element is the
frequency/support of the corresponding item in object `x`. The items
appear in the vector in the same order as in the binary matrix in `x`.

## See also

[`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md)

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
[`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md),
[`itemMatrix-class`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[`itemwiseSetOps`](http://michael.hahsler.net/arules/reference/itemwiseSetOps.md),
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
data("Adult")
itemFrequency(Adult, type = "relative")
#>                                 age=Young 
#>                              1.971050e-01 
#>                           age=Middle-aged 
#>                              5.051185e-01 
#>                                age=Senior 
#>                              2.608616e-01 
#>                                   age=Old 
#>                              3.691495e-02 
#>                     workclass=Federal-gov 
#>                              2.931903e-02 
#>                       workclass=Local-gov 
#>                              6.420703e-02 
#>                    workclass=Never-worked 
#>                              2.047418e-04 
#>                         workclass=Private 
#>                              6.941976e-01 
#>                    workclass=Self-emp-inc 
#>                              3.470374e-02 
#>                workclass=Self-emp-not-inc 
#>                              7.907129e-02 
#>                       workclass=State-gov 
#>                              4.055935e-02 
#>                     workclass=Without-pay 
#>                              4.299578e-04 
#>                       education=Preschool 
#>                              1.699357e-03 
#>                         education=1st-4th 
#>                              5.057123e-03 
#>                         education=5th-6th 
#>                              1.042136e-02 
#>                         education=7th-8th 
#>                              1.955284e-02 
#>                             education=9th 
#>                              1.547848e-02 
#>                            education=10th 
#>                              2.843864e-02 
#>                            education=11th 
#>                              3.709922e-02 
#>                            education=12th 
#>                              1.345154e-02 
#>                         education=HS-grad 
#>                              3.231645e-01 
#>                     education=Prof-school 
#>                              1.707547e-02 
#>                      education=Assoc-acdm 
#>                              3.277917e-02 
#>                       education=Assoc-voc 
#>                              4.219729e-02 
#>                    education=Some-college 
#>                              2.227182e-01 
#>                       education=Bachelors 
#>                              1.643053e-01 
#>                         education=Masters 
#>                              5.439990e-02 
#>                       education=Doctorate 
#>                              1.216166e-02 
#>                   marital-status=Divorced 
#>                              1.358052e-01 
#>          marital-status=Married-AF-spouse 
#>                              7.575447e-04 
#>         marital-status=Married-civ-spouse 
#>                              4.581917e-01 
#>      marital-status=Married-spouse-absent 
#>                              1.285779e-02 
#>              marital-status=Never-married 
#>                              3.299824e-01 
#>                  marital-status=Separated 
#>                              3.132550e-02 
#>                    marital-status=Widowed 
#>                              3.107981e-02 
#>                   occupation=Adm-clerical 
#>                              1.148806e-01 
#>                   occupation=Armed-Forces 
#>                              3.071127e-04 
#>                   occupation=Craft-repair 
#>                              1.251382e-01 
#>                occupation=Exec-managerial 
#>                              1.246059e-01 
#>                occupation=Farming-fishing 
#>                              3.050653e-02 
#>              occupation=Handlers-cleaners 
#>                              4.242251e-02 
#>              occupation=Machine-op-inspct 
#>                              6.187298e-02 
#>                  occupation=Other-service 
#>                              1.007944e-01 
#>                occupation=Priv-house-serv 
#>                              4.954752e-03 
#>                 occupation=Prof-specialty 
#>                              1.263667e-01 
#>                occupation=Protective-serv 
#>                              2.012612e-02 
#>                          occupation=Sales 
#>                              1.126899e-01 
#>                   occupation=Tech-support 
#>                              2.960567e-02 
#>               occupation=Transport-moving 
#>                              4.821670e-02 
#>                      relationship=Husband 
#>                              4.036690e-01 
#>                relationship=Not-in-family 
#>                              2.576266e-01 
#>               relationship=Other-relative 
#>                              3.083412e-02 
#>                    relationship=Own-child 
#>                              1.552148e-01 
#>                    relationship=Unmarried 
#>                              1.049302e-01 
#>                         relationship=Wife 
#>                              4.772532e-02 
#>                   race=Amer-Indian-Eskimo 
#>                              9.622866e-03 
#>                   race=Asian-Pac-Islander 
#>                              3.110028e-02 
#>                                race=Black 
#>                              9.592154e-02 
#>                                race=Other 
#>                              8.312518e-03 
#>                                race=White 
#>                              8.550428e-01 
#>                                sex=Female 
#>                              3.315180e-01 
#>                                  sex=Male 
#>                              6.684820e-01 
#>                         capital-gain=None 
#>                              9.173867e-01 
#>                          capital-gain=Low 
#>                              4.801196e-02 
#>                         capital-gain=High 
#>                              3.460137e-02 
#>                         capital-loss=None 
#>                              9.532779e-01 
#>                          capital-loss=Low 
#>                              2.387290e-02 
#>                         capital-loss=High 
#>                              2.284919e-02 
#>                  hours-per-week=Part-time 
#>                              1.210638e-01 
#>                  hours-per-week=Full-time 
#>                              5.850907e-01 
#>                  hours-per-week=Over-time 
#>                              2.595307e-01 
#>                 hours-per-week=Workaholic 
#>                              3.431473e-02 
#>                   native-country=Cambodia 
#>                              5.732771e-04 
#>                     native-country=Canada 
#>                              3.726301e-03 
#>                      native-country=China 
#>                              2.497850e-03 
#>                   native-country=Columbia 
#>                              1.740305e-03 
#>                       native-country=Cuba 
#>                              2.825437e-03 
#>         native-country=Dominican-Republic 
#>                              2.108841e-03 
#>                    native-country=Ecuador 
#>                              9.213382e-04 
#>                native-country=El-Salvador 
#>                              3.173498e-03 
#>                    native-country=England 
#>                              2.600221e-03 
#>                     native-country=France 
#>                              7.780189e-04 
#>                    native-country=Germany 
#>                              4.217682e-03 
#>                     native-country=Greece 
#>                              1.003235e-03 
#>                  native-country=Guatemala 
#>                              1.801728e-03 
#>                      native-country=Haiti 
#>                              1.535564e-03 
#>         native-country=Holand-Netherlands 
#>                              2.047418e-05 
#>                   native-country=Honduras 
#>                              4.094836e-04 
#>                       native-country=Hong 
#>                              6.142255e-04 
#>                    native-country=Hungary 
#>                              3.890095e-04 
#>                      native-country=India 
#>                              3.091601e-03 
#>                       native-country=Iran 
#>                              1.207977e-03 
#>                    native-country=Ireland 
#>                              7.575447e-04 
#>                      native-country=Italy 
#>                              2.149789e-03 
#>                    native-country=Jamaica 
#>                              2.170263e-03 
#>                      native-country=Japan 
#>                              1.883625e-03 
#>                       native-country=Laos 
#>                              4.709062e-04 
#>                     native-country=Mexico 
#>                              1.947095e-02 
#>                  native-country=Nicaragua 
#>                              1.003235e-03 
#> native-country=Outlying-US(Guam-USVI-etc) 
#>                              4.709062e-04 
#>                       native-country=Peru 
#>                              9.418124e-04 
#>                native-country=Philippines 
#>                              6.039884e-03 
#>                     native-country=Poland 
#>                              1.781254e-03 
#>                   native-country=Portugal 
#>                              1.371770e-03 
#>                native-country=Puerto-Rico 
#>                              3.767249e-03 
#>                   native-country=Scotland 
#>                              4.299578e-04 
#>                      native-country=South 
#>                              2.354531e-03 
#>                     native-country=Taiwan 
#>                              1.330822e-03 
#>                   native-country=Thailand 
#>                              6.142255e-04 
#>            native-country=Trinadad&Tobago 
#>                              5.528029e-04 
#>              native-country=United-States 
#>                              8.974243e-01 
#>                    native-country=Vietnam 
#>                              1.760780e-03 
#>                 native-country=Yugoslavia 
#>                              4.709062e-04 
#>                              income=small 
#>                              5.061218e-01 
#>                              income=large 
#>                              1.605381e-01 
```
