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



##***************************************************************
## read/write functions

## for convenience [ceeboo 2007]
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

read.transactions <-
  function(file, format = c("basket", "single"), 
    header = FALSE, sep = "", cols = NULL, 
    rm.duplicates = FALSE, quote = "\"'", skip = 0, 
    encoding="unknown") {
    
    format <- match.arg(format)
    
    if (format == "basket") {
      data <- lapply(readLines(file, encoding = encoding), 
        FUN = function(l) scan(text = l, what = 'character',
          sep = sep, quote = quote, 
          quiet = TRUE, encoding = encoding))
      
      if(header) skip <- skip + 1
      
      ## skip
      if(skip>0) data <- data[-seq_len(skip)]
      
      if (!is.null(cols)) {
        if (!(is(cols, "numeric") && (length(cols) == 1)))
          stop("'cols' must be a numeric scalar for 'basket'.")
        cols <- as(cols, "integer")
        names(data) <- sapply(data, "[", cols)
        data <- lapply(data, "[", -cols)
      }
      
      ## remove leading and trailing white spaces
      data <- lapply(data, function(x) trimws(x))
      
      ## remove items with length(label) == 0
      data <- lapply(data, function(x) x[nchar(x)>0])
      
      if (rm.duplicates)
        data <- .rm.duplicates(data)
      
      return(as(data, "transactions"))   
    }
    
    ## If format is "single", have lines with TIDs and IIDs in the
    ## columns specified by 'cols'.
    
    ## If cols is a character vector of length 2 we assume the file
    ## has a header with colnames (added by F. Leisch)
    if(header) {
      colnames <- scan(file = file, what = "", sep = sep, quote = quote,
        quiet = TRUE, skip = skip, nlines = 1, encoding = encoding)
      skip <- skip + 1
      if(is(cols, "character")){
        cols <- match(cols, colnames)
        if(any(is.na(cols)))
          stop("'cols' does not match entries in header of file.")
      }
    }
    
    ## Else we get the numbers of the columns directly
    if(length(cols) != 2) 
      stop("'cols' must be a vector of length 2 for 'single'.")
    if(!is(cols, "numeric"))
      stop("'cols' must be a numeric (character is only allowed for header = TRUE).")
    
    cols <- as(cols, "integer")
    ## Thanks to BDR for indicating how to only read in the relevant
    ## columns.
    what <- vector("list", length = max(cols))
    what[cols] <- ""
    entries <- scan(file = file, 
      sep = sep, quote = quote, what = what, flush = TRUE,
      quiet = TRUE, skip = skip, encoding = encoding)
    
    tids <- factor(entries[[cols[1]]])
    items <- factor(entries[[cols[2]]])
    
    ## Note: rm.duplicates is automatically done
    
    ngT <- new("ngTMatrix", 
      i = as.integer(items) - 1L, 
      j = as.integer(tids) - 1L, 
      Dim = c(length(levels(items)), length(levels(tids))), 
      Dimnames = list(levels(items), NULL))
    
    trans <- as(as(ngT, "ngCMatrix"), "transactions")
    transactionInfo(trans) <- data.frame(transactionID = levels(tids))
    
    return(trans)
  }

## write transactions and associations
### FIXME: Quote does not work for basket format!

setMethod("write", signature(x = "transactions"),
  function(x, file = "", format = c("basket", "single"), 
    sep = " ", quote = TRUE, ...) { 
    
    format <- match.arg(format)
    if (format == "basket") {
      l <- LIST(x)
      
      ## quotes?
      if(quote) l <- lapply(l, FUN = function(s) sprintf('"%s"', s))
      
      dat <- unlist(list(lapply(l, paste, collapse = sep)))
      write(dat, file = file, ...)
    } else { ## format single 
      
      l <- LIST(x)
      dat <- data.frame(transactionID = rep(labels(l),lapply(l, length)), 
        item = unlist(l), row.names = NULL)
      write.table(dat, file = file, sep = sep, quote = quote, 
        row.names = FALSE, col.names = FALSE, ...)
    }
    invisible(dat)
  }
)

setMethod("write", signature(x = "associations"),
  function(x, file = "", sep= " ", quote = TRUE, ...) 
    write.table(as(x, "data.frame"), file = file, 
      sep = sep, quote = quote, ...)
)
