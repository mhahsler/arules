# Write Transactions or Associations to a File

Provides the generic function `write()` and the methods to write
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
or
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
to a file.

## Usage

``` r
write(x, file = "", ...)

# S4 method for class 'transactions'
write(
  x,
  file = "",
  format = c("basket", "single"),
  sep = " ",
  quote = TRUE,
  ...
)

# S4 method for class 'associations'
write(x, file = "", sep = " ", quote = TRUE, ...)
```

## Arguments

- x:

  the
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md)
  ([rules](http://michael.hahsler.net/arules/reference/rules-class.md),
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md),
  etc.) object.

- file:

  either a character string naming a file or a connection open for
  writing. '""' indicates output to the console.

- ...:

  further arguments passed on to
  [`write.table()`](https://rdrr.io/r/utils/write.table.html). Use
  `fileEncoding` to set the encoding used for writing the file.

- format:

  format to write transactions.

- sep:

  the field separator string. Values within each row of x are separated
  by this string. Use `quote = TRUE` and `sep = ","` for saving data as
  in csv format.

- quote:

  a logical value. Quote fields?

## Details

For associations
([rules](http://michael.hahsler.net/arules/reference/rules-class.md) and
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md))
`write()` first uses coercion to data.frame to obtain a printable form
of `x` and then uses
[`utils::write.table()`](https://rdrr.io/r/utils/write.table.html) to
write the data to disk. This is just a method to export the rules in
human-readable form. These exported associations cannot be read back in
as rules. To save and load associations in compact form, use
[`save()`](https://rdrr.io/r/base/save.html) and
[`load()`](https://rdrr.io/r/base/load.html) from the base package.
Alternatively, association can be written to disk in PMML (Predictive
Model Markup Language) via
[`write.PMML()`](http://michael.hahsler.net/arules/reference/pmml.md).
This requires package pmml.

Transactions can be saved in *basket* (one line per transaction) or in
*single* (one line per item) format.

## See also

Other import/export:
[`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md),
[`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md),
[`pmml`](http://michael.hahsler.net/arules/reference/pmml.md),
[`read`](http://michael.hahsler.net/arules/reference/read.md)

## Author

Michael Hahsler

## Examples

``` r
data("Epub")

## write the formated transactions to screen (basket format)
write(head(Epub))
#> "doc_154"
#> "doc_3d6"
#> "doc_16f"
#> "doc_11d" "doc_1a7" "doc_f4"
#> "doc_83"
#> "doc_11d"

## write the formated transactions to screen (single format)
write(head(Epub), format = "single")
#> "session_4795" "doc_154"
#> "session_4797" "doc_3d6"
#> "session_479a" "doc_16f"
#> "session_47b7" "doc_11d"
#> "session_47b7" "doc_1a7"
#> "session_47b7" "doc_f4"
#> "session_47bb" "doc_83"
#> "session_47c2" "doc_11d"

## write the formated result to file in CSV format
write(Epub, file = "data.csv", format = "single", sep = ",")

## write rules in CSV format
rules <- apriori(Epub, parameter = list(support = 0.0005, conf = 0.8))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5   5e-04      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 7 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[936 item(s), 15729 transaction(s)] done [0.00s].
#> sorting and recoding items ... [687 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 done [0.00s].
#> writing ... [374 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
write(rules, file = "data.csv", sep = ",")

unlink("data.csv") # tidy up
```
