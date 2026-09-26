# Classes dist, ar_cross_dissimilarity and ar_similarity — Proximity Matrices

Simple classes to represent proximity matrices.

## Details

For compatibility with clustering functions in `R`, we represent
dissimilarities as the `S3` class `dist`. For cross-dissimilarities and
similarities, we provide the `S4` classes `ar_cross_dissimilarities` and
`ar_similarities`.

## Objects from the Class

`dist` objects are the result of calling the method
[`dissimilarity()`](http://michael.hahsler.net/arules/reference/dissimilarity.md)
with one argument or any `R` function returning a `S3 dist` object.

`ar_cross_dissimilarity` objects are the result of calling the method
[`dissimilarity()`](http://michael.hahsler.net/arules/reference/dissimilarity.md)
with two arguments, by calls of the form `new("similarity", ...)`, or by
coercion from matrix.

`ar_similarity` objects are the result of calling the method
[`affinity()`](http://michael.hahsler.net/arules/reference/affinity.md),
by calls of the form `new("similarity", ...)`, or by coercion from
matrix.

## See also

[`stats::dist()`](https://rdrr.io/r/stats/dist.html), `proxy::dist()`

Other proximity classes and functions:
[`affinity()`](http://michael.hahsler.net/arules/reference/affinity.md),
[`dissimilarity()`](http://michael.hahsler.net/arules/reference/dissimilarity.md),
[`predict()`](http://michael.hahsler.net/arules/reference/predict.md)

## Author

Michael Hahsler
