# Computing Transaction Weights With HITS

Compute the hub transaction weights for a collection of
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
using the HITS (hubs and authorities) algorithm.

## Usage

``` r
hits(
  data,
  iter = 16L,
  tol = NULL,
  type = c("normed", "relative", "absolute"),
  verbose = FALSE
)
```

## Arguments

- data:

  an object of or coercible to class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

- iter:

  an integer value specifying the maximum number of iterations to use.

- tol:

  convergence tolerance (default `FLT_EPSILON`).

- type:

  a string value specifying the norming of the hub weights. For
  `"normed"` scale the weights to unit length (L2 norm), and for
  `"relative"` to unit sum.

- verbose:

  a logical specifying if progress and runtime information should be
  displayed.

## Value

A `numeric` vector with transaction weights for `data`.

## Details

Model a collection of
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
as a bipartite graph of hubs (transactions) and authorities (items) with
unit arcs and free node weights. That is, a transaction weight is the
sum of the (normalized) weights of the items and vice versa. The weights
are estimated by iterating the model to a steady-state using a builtin
convergence tolerance of `FLT_EPSILON` for (the change in) the norm of
the vector of authorities.

## References

K. Sun and F. Bai (2008). Mining Weighted Association Rules without
Preassigned Weights. *IEEE Transactions on Knowledge and Data
Engineering*, 4 (30), 489–495.

## See also

Other weighted association mining functions:
[`SunBai`](http://michael.hahsler.net/arules/reference/SunBai.md),
[`weclat()`](http://michael.hahsler.net/arules/reference/weclat.md)

## Author

Christian Buchta

## Examples

``` r
data(SunBai)

## calculate transaction weigths
w <- hits(SunBai)
w
#>       100       200       300       400       500       600 
#> 0.5176528 0.4362571 0.2321374 0.1476262 0.5440458 0.4123691 

## add transaction weight to the dataset
transactionInfo(SunBai)[["weight"]] <- w
transactionInfo(SunBai)
#>   transactionID    weight
#> 1           100 0.5176528
#> 2           200 0.4362571
#> 3           300 0.2321374
#> 4           400 0.1476262
#> 5           500 0.5440458
#> 6           600 0.4123691

## calulate regular item frequencies
itemFrequency(SunBai, weighted = FALSE)
#>         A         B         C         D         E         F         G         H 
#> 0.6666667 0.3333333 0.5000000 0.1666667 0.1666667 0.3333333 0.5000000 0.3333333 

## calulate weighted item frequencies
itemFrequency(SunBai, weighted = TRUE)
#>         A         B         C         D         E         F         G         H 
#> 0.5719366 0.3274066 0.6541039 0.2260405 0.2260405 0.4280634 0.6081302 0.4176323 
```
