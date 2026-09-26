# The Epub Transactions Data Set

The `Epub` data set contains the download history of documents from the
electronic publication platform of the Vienna University of Economics
and Business Administration. The data was recorded between Jan 2003 and
Dec 2008.

## Format

Object of class
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
with 15729 transactions and 936 items. Item labels are document IDs of
the form `"doc_11d"`. Session IDs and time stamps for transactions are
also provided as transaction information.

## Source

Provided by Michael Hahsler from the custom information system ePub-WU
(which has since been replaced by eprint).

## Author

Michael Hahsler

## Examples

``` r
data(Epub)
inspect(head(Epub))
#>     items                      transactionID TimeStamp          
#> [1] {doc_154}                  session_4795  2003-01-02 01:59:00
#> [2] {doc_3d6}                  session_4797  2003-01-02 12:46:01
#> [3] {doc_16f}                  session_479a  2003-01-02 15:50:38
#> [4] {doc_11d, doc_1a7, doc_f4} session_47b7  2003-01-02 23:55:50
#> [5] {doc_83}                   session_47bb  2003-01-03 02:27:44
#> [6] {doc_11d}                  session_47c2  2003-01-03 15:18:04
```
