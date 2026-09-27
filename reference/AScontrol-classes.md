# Classes AScontrol, APcontrol, ECcontrol — Specifying the control Argument of Apriori and Eclat

The `AScontrol` class holds the algorithmic parameters for the used
mining algorithms. `APcontrol` and `ECcontrol` directly extend
`AScontrol` with additional slots for parameters only suitable for the
algorithms Apriori (`APcontrol`) and Eclat (`ECcontrol`).

## Usage

``` r
# S4 method for class 'NULL,APcontrol'
coerce(from, to = "APcontrol", strict = TRUE)
```

## Arguments

- from:

  object to coerce.

- to:

  target class for the coercion.

- strict:

  logical; if `TRUE`, the returned object must be strictly from the
  target class.

## Slots

- `sort`:

  an integer scalar indicating how to sort items with respect to their
  frequency: (default: 2)

  - 1: ascending

  - -1: descending

  - 0: do not sort

  - 2: ascending

  - -2: descending with respect to transaction size sum

- `verbose`:

  a logical indicating whether to report progress

- `filter`:

  a numeric scalar indicating how to filter unused items from
  transactions (default: 0.1)

  - \\=0\\: do not filter items with respect to. usage in sets

  - \\\<0\\: fraction of removed items for filtering

  - \\\>0\\: take execution times ratio into account

- `tree`:

  a logical indicating whether to organize transactions as a prefix tree
  (default: `TRUE`)

- `heap`:

  a logical indicating whether to use heapsort instead of quicksort to
  sort the transactions (default: `TRUE`)

- `memopt`:

  a logical indicating whether to minimize memory usage instead of
  maximize speed (default: `FALSE`)

- `load`:

  a logical indicating whether to load transactions into memory
  (default: `TRUE`)

- `sparse`:

  a numeric value for the threshold for sparse representation (default:
  7)

## Available Slots by Subclass

- `APcontrol`: `filter`, `tree`, `heap`, `memopt`, `load`, `sort`,
  `verbose`

- `ECcontrol`: `sparse`, `sort`, `verbose`

## Objects from the Class

A suitable default control object will be automatically created by the
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md) or
the [`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md)
function. By specifying a named list (names equal to slots) as the
`control` argument for
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md) or
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md),
default values can be replaced with the values in the list.

Objects can also be created via coercion.

## Coercions

- `as("NULL", "APcontrol")`

- `as("list", "APcontrol")`

- `as("NULL", "ECcontrol")`

- `as("list", "ECcontrol")`

## References

Christian Borgelt (2004) *Apriori — Finding Association Rules/Hyperedges
with the Apriori Algorithm*. <https://borgelt.net/apriori.html>

## See also

Other mining algorithms:
[`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md),
[`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md),
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md),
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md),
[`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md)

## Author

Michael Hahsler and Bettina Gruen
