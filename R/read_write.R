#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' Read Transaction Data
#'
#' Reads transaction data from a file and creates a
#' [transactions] object.
#'
#' For _basket_ format, each line in the transaction data file
#' represents a transaction where the items (item labels) are separated by the
#' characters specified by `sep`.  For _single_ format, each line
#' corresponds to a single item, containing at least ids for the transaction
#' and the item.
#'
#' @name read
#' @family import/export
#' 
#' @param file the file name or a connection.
#' @param format a character string indicating the format of the data set.  One
#' of `"basket"` or `"single"`, can be abbreviated.
#' @param header a logical value indicating whether the file contains the names
#' of the variables as its first line.
#' @param sep a character string specifying how fields are separated in the
#' data file. The default (`""`) splits at whitespaces.
#' @param cols For the _single_ format, `cols` is a numeric or
#' character vector of length two giving the numbers or names of the columns
#' (fields) with the transaction and item ids, respectively. If character, the
#' first line of `file` is assumed to be a header with column names.  For
#' the _basket_ format, `cols` can be a numeric scalar giving the
#' number of the column (field) with the transaction ids.  If 
#' `cols = NULL`, the data do not contain transaction ids.
#' @param rm.duplicates a logical value specifying if duplicate items should be
#' removed from the transactions.
#' @param quote a list of characters used as quotes when reading.
#' @param skip number of lines to skip in the file before start reading data.
#' @param encoding character string indicating the encoding which is passed to
#' [readLines()] or [scan()] (see [Encoding] for character encoding).
#' @return Returns an object of class [transactions].
#' @author Michael Hahsler and Kurt Hornik
#' @keywords file
#' @examples
#' ## create a demo file using basket format for the example
#' data <- paste(
#'   "# this is some test data",
#'   "item1, item2",
#'   "item1",
#'   "item2, item3",
#'   sep="\n")
#' cat(data)
#' write(data, file = "demo_basket.txt")
#'
#' ## read demo data (skip the comment in the first line)
#' tr <- read.transactions("demo_basket.txt", format = "basket", sep = ",", skip = 1)
#' inspect(tr)
#' ## make always sure that the items were properly separated
#' itemLabels(tr)
#'
#' ## create a demo file using single format for the example
#' ## column 1 contains the transaction ID and column 2 contains one item
#' data <- paste(
#'   "trans1 item1",
#'   "trans2 item1",
#'   "trans2 item2",
#'   sep ="\n")
#' cat(data)
#' write(data, file = "demo_single.txt")
#'
#' ## read demo data
#' tr <- read.transactions("demo_single.txt", format = "single", cols = c(1,2))
#' inspect(tr)
#'
#' ## create a demo file using single format with column headers
#' data <- paste(
#'   "item_id;trans_id",
#'   "item1;trans1",
#'   "item1;trans2",
#'   "item2;trans2",
#'   sep ="\n")
#' cat(data)
#' write(data, file = "demo_single.txt")
#'
#' ## read demo data
#' tr <- read.transactions("demo_single.txt", format = "single",
#'   header = TRUE, sep = ";", cols = c("trans_id", "item_id"))
#' inspect(tr)
#'
#' ## tidy up
#' unlink("demo_basket.txt")
#' unlink("demo_single.txt")
#' @export read.transactions
read.transactions <-
  function(file,
    format = c("basket", "single"),
    header = FALSE,
    sep = "",
    cols = NULL,
    rm.duplicates = FALSE,
    quote = "\"'",
    skip = 0,
    encoding = "unknown") {
    format <- match.arg(format)
    
    if (is.character(file)) {
      file <- file(file, "r")
      on.exit(close(file))
    }
    
    if (format == "basket") {
      data <- lapply(
        readLines(file, encoding = encoding),
        FUN = function(l)
          scan(
            text = l,
            what = 'character',
            sep = sep,
            quote = quote,
            quiet = TRUE,
            encoding = encoding
          )
      )
      
      if (header)
        skip <- skip + 1
      
      ## skip
      if (skip > 0)
        data <- data[-seq_len(skip)]
      
      if (!is.null(cols)) {
        if (!(is(cols, "numeric") && (length(cols) == 1)))
          stop("'cols' must be a numeric scalar for 'basket'.")
        cols <- as(cols, "integer")
        names(data) <- sapply(data, "[", cols)
        data <- lapply(data, "[",-cols)
      }
      
      ## remove leading and trailing white spaces
      data <- lapply(data, function(x)
        trimws(x))
      
      ## remove items with length(label) == 0
      data <- lapply(data, function(x)
        x[nchar(x) > 0])
      
      if (rm.duplicates)
        data <- .rm.duplicates(data)
      
      return(as(data, "transactions"))
    }
    
    ## If format is "single", have lines with TIDs and IIDs in the
    ## columns specified by 'cols'.
    
    ## If cols is a character vector of length 2 we assume the file
    ## has a header with colnames (added by F. Leisch)
    if (header) {
      colnames <- scan(
        file = file,
        what = "",
        sep = sep,
        quote = quote,
        quiet = TRUE,
        skip = skip,
        nlines = 1,
        encoding = encoding
      )
      if (is(cols, "character")) {
        cols <- match(cols, colnames)
        if (any(is.na(cols)))
          stop("'cols' does not match entries in header of file.")
      }
      
      # connection continues at the current position
      skip <- 0
    }
    
    ## Else we get the numbers of the columns directly
    if (length(cols) != 2)
      stop("'cols' must be a vector of length 2 for 'single'.")
    if (!is(cols, "numeric"))
      stop("'cols' must be a numeric (character is only allowed for header = TRUE).")
    
    cols <- as(cols, "integer")
    ## Thanks to BDR for indicating how to only read in the relevant
    ## columns.
    what <- vector("list", length = max(cols))
    what[cols] <- ""
    entries <- scan(
      file = file,
      sep = sep,
      quote = quote,
      what = what,
      flush = TRUE,
      quiet = TRUE,
      skip = skip,
      encoding = encoding
    )
    
    tids <- factor(entries[[cols[1]]])
    items <- factor(entries[[cols[2]]])
    
    ## Note: rm.duplicates is automatically done
    
    ngT <- new(
      "ngTMatrix",
      i = as.integer(items) - 1L,
      j = as.integer(tids) - 1L,
      Dim = c(length(levels(items)), length(levels(tids))),
      Dimnames = list(levels(items), NULL)
    )
    
    trans <- as(as(ngT, "CsparseMatrix"), "transactions")
    transactionInfo(trans) <-
      data.frame(transactionID = levels(tids))
    
    return(trans)
  }

.rm.duplicates <- function(x) {
  n <- sapply(x, length, USE.NAMES = FALSE)
  x <- lapply(x, unique)
  n <- n - sapply(x, length, USE.NAMES = FALSE)
  if (any(n)) {
    n <- table(items = n)[-1]
    cat("distribution of transactions with duplicates:\n")
    print(n)
  }
  x
}

#' @rdname write
setGeneric("write",
  function(x, file = "", ...) base::write(x, file, ...))

#' Write Transactions or Associations to a File
#'
#' Provides the generic function `write()` and the methods to write
#' [transactions] or [associations] to a file.
#'
#' For associations ([rules] and [itemsets]) `write()` first uses coercion to
#' data.frame to obtain a printable form of `x` and then uses
#' [utils::write.table()] to write the data to disk.
#'
#' Transactions can be saved in _basket_ (one line per transaction) or in _single_
#' (one line per item) format.
#'
#' Note: To save and load associations in compact form, use [save()] and
#' [load()] from the \pkg{base} package.  Alternatively, association can be
#' written to disk in PMML (Predictive Model Markup Language) via
#' [write.PMML()]. This requires package \pkg{pmml}.
#'
#' @name write
#' @family import/export
#' 
#' @aliases write write.csv
#' @param x the [transactions] or [associations] ([rules], [itemsets], etc.) object.
#' @param file either a character string naming a file or a connection open for
#' writing.  '""' indicates output to the console.
#' @param format format to write transactions.
#' @param sep the field separator string. Values within each row of x are
#' separated by this string. Use `quote = TRUE` and `sep = ","` for
#' saving data as in csv format.
#' @param quote a logical value. Quote fields?
#' @param \dots further arguments passed on to [write.table()]. 
#' Use `fileEncoding` to set the encoding used for
#' writing the file.
#' @author Michael Hahsler
#' @keywords file
#' @examples
#' data("Epub")
#'
#' ## write the formated transactions to screen (basket format)
#' write(head(Epub))
#'
#' ## write the formated transactions to screen (single format)
#' write(head(Epub), format="single")
#'
#' ## write the formated result to file in CSV format
#' write(Epub, file = "data.csv", format = "single", sep = ",")
#'
#' ## write rules in CSV format
#' rules <- apriori(Epub, parameter=list(support = 0.0005, conf = 0.8))
#' write(rules, file = "data.csv", sep = ",")
#'
#' unlink("data.csv") # tidy up
NULL

#' @rdname write
setMethod("write", signature(x = "transactions"),
  function(x,
    file = "",
    format = c("basket", "single"),
    sep = " ",
    quote = TRUE,
    ...) {
    format <- match.arg(format)
    if (format == "basket") {
      l <- LIST(x)
      
      ## quotes?
      if (quote)
        l <- lapply(
          l,
          FUN = function(s)
            sprintf('"%s"', s)
        )
      
      dat <- unlist(list(lapply(l, paste, collapse = sep)))
      write(dat, file = file, ...)
    } else {
      ## format single
      
      l <- LIST(x)
      dat <-
        data.frame(
          transactionID = rep(labels(l), lapply(l, length)),
          item = unlist(l),
          row.names = NULL
        )
      utils::write.table(
        dat,
        file = file,
        sep = sep,
        quote = quote,
        row.names = FALSE,
        col.names = FALSE,
        ...
      )
    }
    invisible(dat)
  })

#' @rdname write
setMethod("write", signature(x = "associations"),
  function(x,
    file = "",
    sep = " ",
    quote = TRUE,
    ...)
    utils::write.table(
      as(x, "data.frame"),
      file = file,
      sep = sep,
      quote = quote,
      ...
    ))

