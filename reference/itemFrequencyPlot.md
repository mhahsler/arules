# Creating a Item Frequencies/Support Bar Plot

Provides the generic function `itemFrequencyPlot()` and the method to
create an item frequency bar plot for inspecting the item frequency
distribution for objects based on
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
(e.g.,
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md),
or items in
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
and
[rules](http://michael.hahsler.net/arules/reference/rules-class.md)).

## Usage

``` r
itemFrequencyPlot(x, ...)

# S4 method for class 'itemMatrix'
itemFrequencyPlot(
  x,
  type = c("relative", "absolute"),
  weighted = FALSE,
  support = NULL,
  topN = NULL,
  population = NULL,
  popCol = "black",
  popLwd = 1,
  lift = FALSE,
  horiz = FALSE,
  names = TRUE,
  cex.names = graphics::par("cex.axis"),
  xlab = NULL,
  ylab = NULL,
  mai = NULL,
  ...
)
```

## Arguments

- x:

  the object to be plotted.

- ...:

  further arguments are passed on (see
  [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html) from
  possible arguments).

- type:

  a character string indicating whether item frequencies should be
  displayed relative of absolute.

- weighted:

  should support be weighted by transactions weights stored as column
  `"weight"` in transactionInfo?

- support:

  a numeric value. Only display items which have a support of at least
  `support`. If no population is given, support is calculated from `x`
  otherwise from the population. Support is interpreted relative or
  absolute according to the setting of `type`.

- topN:

  a integer value. Only plot the `topN` items with the highest item
  frequency or lift (if `lift = TRUE`). The items are plotted ordered by
  descending support.

- population:

  object of same class as `x`; if `x` is a segment of a population, the
  population mean frequency for each item can be shown as a line in the
  plot.

- popCol:

  plotting color for population.

- popLwd:

  line width for population.

- lift:

  a logical indicating whether to plot the lift ratio between instead of
  frequencies. The lift ratio is gives how many times an item is more
  frequent in `x` than in `population`.

- horiz:

  a logical. If `horiz = FALSE` (default), the bars are drawn
  vertically. If `TRUE`, the bars are drawn horizontally.

- names:

  a logical indicating if the names (bar labels) should be displayed?

- cex.names:

  a numeric value for the expansion factor for axis names (bar labels).

- xlab:

  a character string with the label for the x axis (use an empty string
  to force no label).

- ylab:

  a character string with the label for the y axis (see xlab).

- mai:

  a numerical vector giving the plots margin sizes in inches (see \`?
  par').

## Value

A numeric vector with the midpoints of the drawn bars; useful for adding
to the graph.

## See also

[`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md)

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
[`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md),
[`itemMatrix-class`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
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
data(Adult)

## the following example compares the item frequencies
## of people with a large income (boxes) with the average in the data set
Adult.largeIncome <- Adult[Adult %in% "income=large"]

## simple plot
itemFrequencyPlot(Adult.largeIncome)


## plot with the averages of the population plotted as a line
## (for first 72 variables/items)
itemFrequencyPlot(Adult.largeIncome[, 1:72],
  population = Adult[, 1:72]
)


## plot lift ratio (frequency in x / frequency in population)
## for items with a support of 20% in the population
itemFrequencyPlot(Adult.largeIncome,
  population = Adult, support = 0.2,
  lift = TRUE, horiz = TRUE
)

```
