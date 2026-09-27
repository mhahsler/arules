# Classes ASparameter, APparameter, ECparameter — Specifying the parameter Argument of APRIORI and ECLAT

The `ASparameter` class holds the mining parameters (e.g., minimum
support) for the used mining algorithms. `APparameter` and `ECparameter`
directly extend `ASparameter` with additional slots for parameters only
suitable for
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
(`APparameter`) or
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md)
(`ECparameter`).

## Slots

- `support`:

  a numeric value for the minimal support of an item set (default:
  \\0.1\\)

- `minlen`:

  an integer value for the minimal number of items per item set
  (default: 1 item)

- `maxlen`:

  an integer value for the maximal number of items per item set
  (default: 10 items)

- `target`:

  a character string indicating the type of association mined. Partial
  names are matched. Available targets are:

  - `"frequent itemsets"`

  - `"maximally frequent itemsets"`

  - `"generator frequent itemsets"`

  - `"closed frequent itemsets"`

  - `"rules"` only available for
    [apriori](http://michael.hahsler.net/arules/reference/apriori.md);
    use
    [ruleInduction](http://michael.hahsler.net/arules/reference/ruleInduction.md)
    for [eclat](http://michael.hahsler.net/arules/reference/eclat.md).

  - `"hyperedgesets"` only available for
    [apriori](http://michael.hahsler.net/arules/reference/apriori.md);
    see references for the definition of association hyperedgesets.

- `ext`:

  a logical indicating whether to report coverage (i.e., LHS-support) as
  an extended quality measure (default: `TRUE`)

- `confidence`:

  a numeric value for the minimal confidence of rules/association
  hyperedges (default: \\0.8\\). For frequent itemsets it is set to
  `NA`.

- `smax`:

  a numeric value for the maximal support of
  itemsets/rules/hyperedgesets (default: 1)

- `arem`:

  a character string indicating the used additional rule evaluation
  measure (default: `"none"`) given by one of

  - `"none"`: no additional evaluation measure

  - `"diff"`: absolute confidence difference

  - `"quot"`: difference of confidence quotient to 1

  - `"aimp"`: absolute difference of improvement to 1

  - `"info"`: information difference to prior

  - `"chi2"`: normalized \\\chi^2\\ measure

  **Note:** The measure is only reported if `aval` is set to `TRUE`. Use
  `minval` to set minimum thresholds on the measures.

- `aval`:

  a logical indicating whether to return the additional rule evaluation
  measure selected with `arem`.

- `minval`:

  a numeric value for the minimal value of additional evaluation measure
  selected with `arem` (default: \\0.1\\)

- `originalSupport`:

  a logical indicating whether to use the original definition of minimum
  support (support of the LHS and RHS of the rule). If set to `FALSE`
  then the support of the LHS (also called coverage of the rule) is
  returned as support. The minimum support threshold is applied to this
  support. (default: `TRUE`)

- `maxtime`:

  Time limit in seconds for checking subsets. `maxtime = 0` disables the
  time limit. (default: 5 seconds)

- `tidLists`:

  a logical indicating whether
  [`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md)
  should return also a list of supporting transactions IDs. (default:
  `FALSE`)

## Available Slots by Subclass

- `APparameter`: `confidence`, `minval`, `smax`, `arem`, `aval`,
  `originalSupport`, `maxtime`, `support`, `minlen`, `maxlen`, `target`,
  `ext`

- `ECparameter`: `tidLists`, `support`, `minlen`, `maxlen`, `target`,
  `ext`

## Objects from the Class

A suitable default parameter object will be automatically created by
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md) or
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md). By
specifying a named list (names equal to slots) as `parameter` argument
for
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md) or
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md), the
default values can be replaced with the values in the list.

Objects can also be created via coercion.

## Coercions

- `as("NULL", "APparameter")`

- `as("list", "APparameter")`

- `as("NULL", "ECparameter")`

- `as("list", "ECparameter")`

## References

Christian Borgelt (2004) *Apriori — Finding Association Rules/Hyperedges
with the Apriori Algorithm*. <https://borgelt.net/apriori.html>

## See also

Other mining algorithms:
[`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md),
[`AScontrol-classes`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md),
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md),
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md),
[`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md)

## Author

Michael Hahsler and Bettina Gruen
