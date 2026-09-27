# Interface to Mining Algorithms from fim4r

Interfaces the algorithms implemented in fim4r. The algorithms include:
Apriori, Eclat, FPgrowth, Carpenter, IsTa, RElim and SaM.

## Usage

``` r
fim4r(
  transactions,
  method = NULL,
  target = "frequent",
  support = 0.1,
  confidence = 0.8,
  originalSupport = TRUE,
  appear = NULL,
  report = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- transactions:

  a
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  object

- method:

  the algorithm to be used. One of:

  - `"apriori"`, `"eclat"`, `"fpgrowth"` can mine itemsets and rules.

  - `"relim"`, `"sam"` can mine itemsets.

  - `"carpenter"`, `"ista"` can only mine closed itemset.

- target:

  the target type. One of: `"frequent"`, `"closed"`, `"maximal"`,
  `"generators"` or `"rules"`.

- support:

  a numeric value for the minimal support in the range \\\[0,1\]\\.

- confidence:

  a numeric value for the minimal confidence of rules in the range
  \\\[0,1\]\\.

- originalSupport:

  logical; Use the support threshold on the support of the whole rule
  (LHS and RHS). If `FALSE`, then LHS support (i.e., coverage) is used
  by the support threshold.

- appear:

  Specify item appearance in rules (only for apriori, eclat, fpgrowth
  and the target `"rules"`) Specify a list with two vectors (item labels
  and appearance modifiers) of the same length. Appearance modifiers
  are:

  - `"-"` (item may not appear in a rule),

  - `"a"` (item may only appear a rule antecedent/LHS),

  - `"c"` (item may only appear a rule consequent/RHS),

  - `"x"` (item may appear anywhere).

- report:

  cannot be used via the interface.

- verbose:

  logical; print used parameters?

- ...:

  further arguments are passed on to the function the `fim4r::fim4r`
  function for the given method. Examples are: `zmin`, `zmax`, `wgts`.

## Value

An object of class
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
or [rules](http://michael.hahsler.net/arules/reference/rules-class.md).

## Details

**Installation:** The package fim4r is not available via CRAN. If
needed, the `fim4r()` function downloads and installs the current
version of the package automatically. Your system needs to have build
tools installed.

Build tools: You need to be able to install source packages. For Windows
users this means that you need to install the
[RTools](https://cran.r-project.org/bin/windows/Rtools/) with a version
matching your R version.

**Additional Notes:**

- Support and confidence are specified here in the range \\\[0,1\]\\.
  This is different from the use in `fim4r` package where `support` and
  `confidence` have the range \\\[0, 100\]\\. `arules::fim4r()`
  automatically converts support and confidence internally.

- `fim4r` methods also return the empty itemset while `arules` methods
  do not.

- See `? fim4r::fim4r` for help on additional available arguments. This
  is only available after package `fim4r` is installed.

- Algorithm descriptions and references can be found on the [fim4r web
  page](https://borgelt.net/fim4r.html) in the References Section.

## References

Christian Borgelt, fimi4r: Frequent Item Set Mining and Association Rule
Induction for R. <https://borgelt.net/fim4r.html>

## See also

Other mining algorithms:
[`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md),
[`AScontrol-classes`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md),
[`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md),
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md),
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(Adult)

# list available algorithms
fim4r()

# mine association rules with FPgrowth
r <- fim4r(Adult,
  method = "fpgrowth",
  target = "rules", support = .7, confidence = .8
)
r
inspect(head(r, by = "lift"))

# mine closed itemsets with Carpenter or IsTa
fim4r(Adult,
  method = "carpenter",
  target = "closed", support = .7
)
fim4r(Adult,
  method = "ista",
  target = "closed", support = .7
)

# mine frequent itemset of length 2 (zmin and zmax = 2)
freq_2 <- fim4r(Adult,
  method = "relim", target = "frequent", support = .7,
  zmin = 2, zmax = 2
)
inspect(freq_2)

# mine maximal frequent itemsets
mfis <- fim4r(Adult, method = "sam", target = "maximal", support = .7)
inspect(mfis)

# Examples for how to use item appearance with apriori, eclat,
#   fpgrowth in fim4r. We first mine all rules.
inspect(fim4r(Adult,
  method = "fpgrowth",
  target = "rules", support = .8
))

# ignore item "capital-gain=None"
inspect(fim4r(Adult,
  method = "fpgrowth",
  target = "rules", support = .8,
  appear = list(c("capital-gain=None"), c("-"))
))

# "capital-gain=None" cannot appear in consequent (antecedent only)
inspect(fim4r(Adult,
  method = "fpgrowth",
  target = "rules", support = .8,
  appear = list(c("capital-gain=None"), c("a"))
))

# "capital-gain=None" cannot appear in the antecedent
inspect(fim4r(Adult,
  method = "fpgrowth",
  target = "rules", support = .8,
  appear = list(c("capital-gain=None"), c("c"))
))

# restrict the consequent to the item "capital-gain=None".
# That is, "" = all items can only appear in the antecedent with the
# exception that "capital-gain=None" can only appear in the consequent.
inspect(fim4r(Adult,
  method = "fpgrowth",
  target = "rules", support = .8,
  appear = list(c("", "capital-gain=None"), c("a", "c"))
))
} # }
```
