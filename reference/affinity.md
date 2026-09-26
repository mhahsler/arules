# Computing Affinity Between Items

Provides the generic function `affinity()` and methods to compute and
return a similarity matrix with the affinities between items for a set
itemsets stored in a matrix or in
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
via its superclass
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

## Usage

``` r
affinity(x)

# S4 method for class 'matrix'
affinity(x)

# S4 method for class 'itemMatrix'
affinity(x)
```

## Arguments

- x:

  a matrix or an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  or
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  containing itemsets.

## Value

returns an object of class
[ar_similarity](http://michael.hahsler.net/arules/reference/proximity-classes.md)
which represents the affinities between items in `x`.

## Details

Affinity between the two items \\i\\ and \\j\\ is defined by Aggarwal et
al. (2002) as \$\$A(i,j) = \frac{supp(\\i,j\\)}{supp(\\i\\) +
supp(\\j\\) - supp(\\i,j\\)},\$\$ where \\supp(.)\\ is the support
measure. Note that affinity is equivalent to the Jaccard similarity
between items.

## References

Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu (2002) Finding
localized associations in market basket data, *IEEE Trans. on Knowledge
and Data Engineering,* 14(1):51–62.

## See also

Other proximity classes and functions:
[`dissimilarity()`](http://michael.hahsler.net/arules/reference/dissimilarity.md),
[`predict()`](http://michael.hahsler.net/arules/reference/predict.md),
[`proximity-classes`](http://michael.hahsler.net/arules/reference/proximity-classes.md)

## Author

Michael Hahsler

## Examples

``` r
data("Adult")

## choose a sample, calculate affinities
s <- sample(Adult, 500)
s
#> transactions in sparse format with
#>  500 transactions (rows) and
#>  115 items (columns)

a <- affinity(s)
image(a)
```
