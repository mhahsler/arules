# Read Transaction Data

Reads transaction data from a file and creates a
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
object.

## Usage

``` r
read.transactions(
  file,
  format = c("basket", "single"),
  header = FALSE,
  sep = "",
  cols = NULL,
  rm.duplicates = FALSE,
  quote = "\"'",
  skip = 0,
  encoding = "unknown"
)
```

## Arguments

- file:

  the file name or a connection.

- format:

  a character string indicating the format of the data set. One of
  `"basket"` or `"single"`, can be abbreviated.

- header:

  a logical value indicating whether the file contains the names of the
  variables as its first line.

- sep:

  a character string specifying how fields are separated in the data
  file. The default (`""`) splits at whitespaces.

- cols:

  For the *single* format, `cols` is a numeric or character vector of
  length two giving the numbers or names of the columns (fields) with
  the transaction and item ids, respectively. If character, the first
  line of `file` is assumed to be a header with column names. For the
  *basket* format, `cols` can be a numeric scalar giving the number of
  the column (field) with the transaction ids. If `cols = NULL`, the
  data do not contain transaction ids.

- rm.duplicates:

  a logical value specifying if duplicate items should be removed from
  the transactions.

- quote:

  a list of characters used as quotes when reading.

- skip:

  number of lines to skip in the file before start reading data.

- encoding:

  character string indicating the encoding which is passed to
  [`readLines()`](https://rdrr.io/r/base/readLines.html) or
  [`scan()`](https://rdrr.io/r/base/scan.html) (see
  [Encoding](https://rdrr.io/r/base/Encoding.html) for character
  encoding).

## Value

Returns an object of class
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

## Details

For *basket* format, each line in the transaction data file represents a
transaction where the items (item labels) are separated by the
characters specified by `sep`. For *single* format, each line
corresponds to a single item, containing at least ids for the
transaction and the item.

## See also

Other import/export:
[`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md),
[`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md),
[`pmml`](http://michael.hahsler.net/arules/reference/pmml.md),
[`write()`](http://michael.hahsler.net/arules/reference/write.md)

## Author

Michael Hahsler and Kurt Hornik

## Examples

``` r
## create a demo file using basket format for the example
data <- paste(
  "# this is some test data",
  "item1, item2",
  "item1",
  "item2, item3",
  sep = "\n"
)
cat(data)
#> # this is some test data
#> item1, item2
#> item1
#> item2, item3
write(data, file = "demo_basket.txt")

## read demo data (skip the comment in the first line)
tr <- read.transactions("demo_basket.txt", format = "basket", sep = ",", skip = 1)
inspect(tr)
#>     items         
#> [1] {item1, item2}
#> [2] {item1}       
#> [3] {item2, item3}
## make always sure that the items were properly separated
itemLabels(tr)
#> [1] "item1" "item2" "item3"

## create a demo file using single format for the example
## column 1 contains the transaction ID and column 2 contains one item
data <- paste(
  "trans1 item1",
  "trans2 item1",
  "trans2 item2",
  sep = "\n"
)
cat(data)
#> trans1 item1
#> trans2 item1
#> trans2 item2
write(data, file = "demo_single.txt")

## read demo data
tr <- read.transactions("demo_single.txt", format = "single", cols = c(1, 2))
inspect(tr)
#>     items          transactionID
#> [1] {item1}        trans1       
#> [2] {item1, item2} trans2       

## create a demo file using single format with column headers
data <- paste(
  "item_id;trans_id",
  "item1;trans1",
  "item1;trans2",
  "item2;trans2",
  sep = "\n"
)
cat(data)
#> item_id;trans_id
#> item1;trans1
#> item1;trans2
#> item2;trans2
write(data, file = "demo_single.txt")

## read demo data
tr <- read.transactions("demo_single.txt",
  format = "single",
  header = TRUE, sep = ";", cols = c("trans_id", "item_id")
)
inspect(tr)
#>     items          transactionID
#> [1] {item1}        trans1       
#> [2] {item1, item2} trans2       

## tidy up
unlink("demo_basket.txt")
unlink("demo_single.txt")
```
