# Class associations — A Set of Associations

The `associations` class is a virtual class which is extended to
represent mining result (e.g., sets of
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
or [rules](http://michael.hahsler.net/arules/reference/rules-class.md)).
The class defines some common methods for its subclasses.

## Usage

``` r
# S4 method for class 'associations'
quality(x)

# S4 method for class 'associations'
quality(x) <- value

# S4 method for class 'associations'
info(x)

# S4 method for class 'associations'
info(x) <- value

# S4 method for class 'associations'
head(x, n = 6L, by = NULL, decreasing = TRUE, ...)

# S4 method for class 'associations'
tail(x, n = 6L, by = NULL, decreasing = TRUE, ...)

# S4 method for class 'associations'
items(x)

# S4 method for class 'associations'
length(x)

# S4 method for class 'associations'
labels(object)

# S3 method for class 'associations'
plot(x, ...)

# S3 method for class 'itemMatrix'
plot(x, ...)
```

## Arguments

- x, object:

  the object.

- value:

  the replacement value.

- n:

  number of elements

- by:

  sort by this interest measure

- decreasing:

  sort in decreasing order?

- ...:

  further arguments.

## Details

The implementations of `associations` store itemsets (e.g., the LHS and
RHS of a rule) as objects of class
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
(i.e., sparse binary matrices). Quality measures (e.g., support) are
stored in a data.frame accessible via method `quality()`.

See Sections Functions and See Also to see all available methods.

**Note:** Associations can store multisets with duplicated elements.
Duplicated elements can result from combining several sets of
associations. Use
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md) to
remove duplicate associations.

## Functions

- `quality(associations)`: returns the quality data.frame.

- `quality(associations) <- value`: replaces the quality data.frame. The
  lengths of the vectors in the data.frame have to equal the number of
  associations in the set.

- `info(associations)`: returns the info list.

- `info(associations) <- value`: replaces the info list.

- `head(associations)`: returns the first n associations.

- `tail(associations)`: returns the last n associations.

- `items(associations)`: dummy method. This method has to be implemented
  by all subclasses of associations and return the items which make up
  each association as an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- `length(associations)`: dummy method. This method has to be
  implemented by all subclasses of associations and return the number of
  elements in the association.

- `labels(associations)`: dummy method. This method has to be
  implemented by all subclasses of associations and return a vector of
  length(object) of labels for the elements in the association.

## Slots

- `quality`:

  a data.frame

- `info`:

  a list

## Objects from the Class

A virtual class: No objects may be created from it.

## See also

Subclasses:
[rules](http://michael.hahsler.net/arules/reference/rules-class.md),
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
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
[`itemsets-class`](http://michael.hahsler.net/arules/reference/itemsets-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler
