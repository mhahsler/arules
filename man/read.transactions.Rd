\name{read.transactions}
\alias{read.transactions}
\title{Read Transaction Data}
\description{
  Reads a transaction data file from disk and creates a
  \code{\linkS4class{transactions}} object.
}
\usage{
read.transactions(file, format = c("basket", "single"), 
                  header = FALSE, sep = "", 
                  cols = NULL, rm.duplicates = FALSE, 
                  quote = "\"'", skip = 0, 
                  encoding = "unknown")
}
\arguments{
  \item{file}{the file name or connection.}
  \item{format}{a character string indicating the format of the data
    set.  One of \code{"basket"} or \code{"single"}, can be
    abbreviated.}
  \item{header}{a logical value indicating whether the file contains the names of the variables as its first line.}
  \item{sep}{a character string specifying how fields are separated in
    the data file. The default (\code{""}) splits at whitespaces. }
  \item{cols}{For the \sQuote{single} format, \code{cols} is a 
    numeric or character vector of length two giving the numbers or
    names of the columns (fields) with the transaction and item ids,
    respectively. If character, the first line of \code{file} is assumed
    to be a header with column names.
    For the \sQuote{basket} format, \code{cols} can be a numeric scalar
    giving the number of the column (field) with the transaction ids.
    If \code{cols = NULL}, the data do not contain transaction ids.}
  \item{rm.duplicates}{a logical value specifying if duplicate items should
    be removed from the transactions.}
  \item{quote}{ a list of characters used as quotes when reading. }
  \item{skip}{ number of lines to skip in the file before start reading data.}
  \item{encoding}{character string indicating the encoding which is passed to
    \code{readLines} or \code{scan} (see \code{Encoding}).}
}
\details{
  For \sQuote{basket} format, each line in the transaction data file
  represents a transaction where the items (item labels) are separated
  by the characters specified by \code{sep}.  For \sQuote{single}
  format, each line corresponds to a single item, containing at least
  ids for the transaction and the item.
}
\value{
  Returns an object of class
  \code{\linkS4class{transactions}}.
}
\seealso{
  \code{\link{transactions-class}}
}
\author{Michael Hahsler and Kurt Hornik}
\examples{
## create a demo file using basket format for the example
data <- paste(
  "# this is some test data", 
  "item1, item2", 
  "item1", 
  "item2, item3", 
  sep="\n")
cat(data)
write(data, file = "demo_basket")

## read demo data (skip first line)
tr <- read.transactions("demo_basket", format = "basket", sep=",", skip = 1)
inspect(tr)


## create a demo file using single format for the example
## column 1 contains the transaction ID and column 2 contains one item
data <- paste(
  "trans1 item1", 
  "trans2 item1",
  "trans2 item2", 
  sep ="\n")
cat(data)
write(data, file = "demo_single")

## read demo data
tr <- read.transactions("demo_single", format = "single", cols = c(1,2))
inspect(tr)

## create a demo file using single format with column headers
data <- paste(
  "item_id;trans_id"
  "item1;trans1", 
  "item1;trans2",
  "item2;trans2", 
  sep ="\n")
cat(data)
write(data, file = "demo_single")

## read demo data
tr <- read.transactions("demo_single", format = "single", 
  header = TRUE, sep = ";", cols = c("trans_id","item_id"))
inspect(tr)

## tidy up
unlink("demo_basket")
unlink("demo_single")
}
\keyword{file}
