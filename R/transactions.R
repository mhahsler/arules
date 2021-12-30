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

#' Class transactions --- Binary Incidence Matrix for Transactions
#'
#' The `transactions` class is a subclass of [itemMatrix] and 
#' represents transaction data used for mining [associations].
#'
#' Transactions are a direct extension of class [itemMatrix]
#' to store a binary incidence matrix, item labels, and optionally transaction
#' IDs and user IDs.
#' Transactions are represented as sparse binary matrices of class
#' [itemMatrix]. If you work with several transaction sets at the
#' same time, then the encoding (order of the items in the binary matrix) in
#' the different sets is important. See [itemCoding] to learn how
#' to encode and recode transaction sets.
#'
#' Note that you will need to prepare your data first (see coercion methods in
#' the Methods Section and the Example Section below for details on the needed
#' format).
#'
#' - **Continuous variables:** Association rule mining can only use items and
#'   does not work with continuous variables. Continuous variables need to be
#'   discretized first. An item resulting from discretization might be
#'   `age>18` and the column contains only `TRUE` or `FALSE`.
#'   Alternatively it can be a factor with levels `age<=18`,
#'   `50=>age>18` and `age>50`. These will be automatically converted
#'   into 3 items, one for each level. Have a look at the function
#'   [discretize()] for automatic discretization.
#'
#' - **Logical variables:** A logical variable describing a person could be
#'   `tall` indicating if the person is tall using the values `TRUE`
#'   and `FALSE`. The fact that the person is tall would be encoded in the
#'   transaction containing the item `tall` while not tall persons would not
#'   have this item. Therefore, for logical variables, the `TRUE` value is
#'   converted into an item with the name of the variable and for the
#'   `FALSE` values no item is created.
#'
#' - **Factors:** The function also can convert columns with nominal values
#'   (i.e., factors) into a series of binary items (one for each level
#'   constructed as `variable name = level`). Note that nominal variables
#'   need to be encoded as factors (and not characters or numbers). This can be
#'   done with
#'
#'   `data[,"a_nominal_var"] <- factor(data[,"a_nominal_var"])`.
#'
#'   Complete examples for how to prepare data can be found in the man pages for
#'   [Income] and [Adult].
#'
#' @include itemMatrix.R associations.R
#' @name transactions-class
#' @aliases transactions 
#' @family itemMatrix and transactions functions
#'
#' @param x,object,from the object
#' @param itemLabels a vector with labels for the items
#' @param transactionInfo a transaction information data.frame with one row per transaction.
#' @param format `"wide"` or `"long"` format? 
#'   Format wide is a regular data.frame where each row contains an object. 
#'   Format "long" is a data.frame with one column with transaction IDs and one with an 
#'   item (see `cols` below). 
#' @param cols a numeric or character vector of length two giving the index or names of 
#'   the columns (fields) with the transaction and item ids in the long format.
#' @param decode translate item IDs to item labels?
#' @param value replacement value
#'
#' @section Slots:
#' Slots are inherited from [itemMatrix].
#'
#' @section Objects from the Class: 
#' Objects are created by: 
#' 
#' - coercion from objects of other classes. `itemLabels` and `transactionInfo` are 
#'   by default created from information in `x` (e.g., from row and column names).
#' 
#' - the constructor function `transactions()` 
#' 
#' - by calling `new("transactions", ...)`.
#'
#' See Examples Section for creating transactions from data.
#' 
#' @seealso 
#' Superclass: [itemMatrix]
#'
#' @author Michael Hahsler
#' @keywords classes
#' @examples
#' ## Example 1: creating transactions form a list (each element is a transaction)
#' a_list <- list(
#'       c("a","b","c"),
#'       c("a","b"),
#'       c("a","b","d"),
#'       c("c","e"),
#'       c("a","b","d","e")
#'       )
#'
#' ## Set transaction names
#' names(a_list) <- paste("Tr", c(1:5), sep = "")
#' a_list
#'
#' ## Use the constructor to create transactions
#' ## Note: S4 coercion does the same trans1 <- as(a_list, "transactions")
#' trans1 <- transactions(a_list)
#' trans1
#'
#' ## Analyze the transactions
#' summary(trans1)
#' image(trans1)
#'
#' ## Example 2: creating transactions from a 0-1 matrix with 5 transactions (rows) and
#' ##            5 items (columns)
#' a_matrix <- matrix(
#'   c(1, 1, 1, 0, 0,
#' 	   1, 1, 0, 0, 0,
#' 	   1, 1, 0, 1, 0,
#' 	   0, 0, 1, 0, 1,
#' 	   1, 1, 0, 1, 1), ncol = 5)
#'
#' ## Set item names (columns) and transaction labels (rows)
#' colnames(a_matrix) <- c("a", "b", "c", "d", "e")
#' rownames(a_matrix) <- paste("Tr", c(1:5), sep = "")
#'
#' a_matrix
#'
#' ## Create transactions
#' trans2 <- transactions(a_matrix)
#' trans2
#' inspect(trans2)
#'
#' ## Example 3: creating transactions from data.frame (wide format)
#' a_df <- data.frame(
#' 	age   = as.factor(c( 6,   8,   NA, 9,   16)),
#' 	grade = as.factor(c("A", "C", "F", NA, "C")),
#'   pass  = c(TRUE, TRUE, FALSE, TRUE, TRUE))
#' ## Note: factors are translated differently than logicals and NAs are ignored
#' a_df
#'
#' ## Create transactions
#' trans3 <- transactions(a_df)
#' inspect(trans3)
#'
#' ## Note that coercing the transactions back to a data.frame does not recreate the
#' ## original data.frame, but represents the transactions as sets of items
#' as(trans3, "data.frame")
#'
#' ## Example 4: creating transactions from a data.frame with
#' ## transaction IDs and items (long format)
#' a_df3 <- data.frame(
#'   TID =  c( 1,   1,   2,   2,   2,   3 ),
#'   item = c("a", "b", "a", "b", "c", "b")
#' )
#' a_df3
#' trans4 <- transactions(a_df3, format = "long", cols = c("TID", "item"))
#' trans4
#' inspect(trans4)
#'
#' ## convert transactions back into long format.
#' toLongFormat(trans4)
#'
#' ## Example 5: create transactions from a dataset with numeric variables
#' ## using discretization.
#' data(iris)
#'
#' irisDisc <- discretizeDF(iris)
#' head(irisDisc)
#'
#' trans5 <- transactions(irisDisc)
#' trans5
#' inspect(head(trans5))
#'
#' ## Note, creating transactions without discretizing numeric variables will apply the
#' ## default discretization and also create a warning.
#'
#'
#' ## Example 6: create transactions manually (with the same item coding as in trans5)
#' trans6 <- transactions(
#'   list(
#'     c("Sepal.Length=[4.3,5.4)", "Species=setosa"),
#'     c("Sepal.Length=[4.3,5.4)", "Species=setosa")
#'   ), itemLabels = trans5)
#' trans6
#'
#' inspect(trans6)
#' @aliases initialize,transactions-method show,transactions-method 
setClass(
  "transactions",
  contains = "itemMatrix",
  
  validity = function(object) {
    ## check dimensions
    ## no transactionInfo (empty data.frame)
    if (length(object@itemsetInfo) &&
        length(object@itemsetInfo[[1]]) != length(object))
      return("transactionInfo does not match number of transactions")
    
    TRUE
  }
)

setMethod(initialize, "transactions", function(.Object, ...) {
  .Object <- callNextMethod()
  
  validObject(.Object)
  .Object
})

setMethod("show", signature(object = "transactions"),
  function(object) {
    cat(
      "transactions in sparse format with\n",
      nrow(object),
      "transactions (rows) and\n",
      ncol(object),
      "items (columns)\n"
    )
    invisible(NULL)
  })

#' @rdname transactions-class
transactions <-
  function(x,
    itemLabels = NULL,
    transactionInfo = NULL,
    format = "wide",
    cols = NULL) {
    format <- match.arg(format, c("wide", "long"))
    
    if (format == "wide") {
      trans <- as(x, "transactions")
    } else {
      if (is.null(cols))
        cols <- 1:2
      utils::write.table(x[, cols], file = tmp <-
          file(), row.names = FALSE)
      trans <- read.transactions(tmp,
        format = "single",
        header = TRUE,
        cols = 1:2)
      close(tmp)
      
    }
    
    if (!is.null(itemLabels))
      trans <- recode(trans, itemLabels = itemLabels)
    if (!is.null(transactionInfo))
      transactionInfo(trans) <- transactionInfo
    trans
  }


setClass("summary.transactions",
  contains = "summary.itemMatrix")

#' @describeIn transactions-class produce a summary
#' @aliases summary.transactions-class show,summary.transactions-method
setMethod("summary", signature(object = "transactions"),
  function(object)
    new(
      "summary.transactions",
      callNextMethod(),
      itemsetInfo = head(object@itemsetInfo, 3)
    ))

setMethod("show", signature(object = "summary.transactions"),
  function(object) {
    cat("transactions as ")
    show(as(object, "summary.itemMatrix"))
    
    if (length(object@itemsetInfo)) {
      cat("\nincludes extended transaction information - examples:\n")
      print(object@itemsetInfo)
    }
    invisible(NULL)
  })


#' @describeIn transactions-class convert the transactions to long format 
#' (a data.frame with two columns, tid and item). Column names can 
#' be specified as a character vector of length 2 called `cols`.
setMethod("toLongFormat", signature(from = "transactions"),
  function(from,
    cols = c("TID", "item"),
    decode = TRUE)
    callNextMethod(from, cols = cols, decode = decode))

# setGeneric("t") # is generic in Matrix

# no t for associations

#' @rdname transactions-class
#' @name t-transactions
#' @aliases t,transactions-method
NULL

setMethod("t", signature(x = "transactions"),
  function(x)
    stop(
      "Object not transposable! Use as(x, \"tidLists\") for coercion to tidLists."
    ))


#' @describeIn transactions-class get the transactions as an [itemMatrix]
setMethod("items", signature(x = "transactions"),
  function(x)
    as(x, "itemMatrix"))

#' @rdname transactions-class
setGeneric("transactionInfo",
  function(x)
    standardGeneric("transactionInfo"))

#' @describeIn transactions-class get the transaction info data.frame
setMethod("transactionInfo", signature(x = "transactions"),
  function(x)
    x@itemsetInfo)

#' @rdname transactions-class
setGeneric("transactionInfo<-",
  function(x, value)
    standardGeneric("transactionInfo<-"))

#' @describeIn transactions-class replace the transaction info data.frame
setReplaceMethod("transactionInfo", signature(x = "transactions"),
  function(x, value) {
    x@itemsetInfo <- value
    validObject(x)
    x
  })

#' @describeIn transactions-class get the dimnames
setMethod("dimnames", signature(x = "transactions"),
  function(x) {
    ## NOTE: as.character is to support old data which used I()
    rowLabels <- transactionInfo(x)[["transactionID"]]
    if (!is.null(rowLabels))
      rowLabels <- as.character(rowLabels)
    
    ## NOTE: as.character is to support old data which used I()
    colLabels <- as.character(itemInfo(x)[["labels"]])
    
    list(rowLabels, colLabels)
  })

#' @describeIn transactions-class set the dimnames
setReplaceMethod("dimnames", signature(x = "transactions",
  value = "list"),
  function(x, value) {
    if (any(dim(x) != sapply(value, length) & !sapply(value, is.null)))
      stop("length of dimnames does not equal the dimension of the object.")
    
    if (!is.null(value[[1]])) {
      if (ncol(transactionInfo(x)) == 0) {
        transactionInfo(x) <- data.frame(transactionID = value[[1]])
      } else{
        transactionInfo(x)[["transactionID"]] <- value[[1]]
      }
    }
    
    if (!is.null(value[[2]])) {
      itemInfo(x)[["labels"]] <- value[[2]]
    }
    
    x
  })

# is in itemMatrix
#setMethod("itemLabels", signature(object = "transactions"),
#  function(object) colnames(object))


#' @rdname transactions-class
#' @name coercion-transactions
#' @aliases 
#' coerce,transactions,matrix-method
#' coerce,matrix,transactions-method
#' coerce,list,transactions-method
#' coerce,transactions,list-method
#' coerce,data.frame,transactions-method
#' coerce,transactions,data.frame-method
#' coerce,ngCMatrix,transactions-method
#' 
#' @section Coercion:
#'
#' * as("transactions", "matrix")
#' * as("matrix", "transactions")
#' * as("list", "transactions")
#' * as("transactions", "list")
#' * as("data.frame", "transactions")
#' * as("transactions", "data.frame")
#' * as("ngCMatrix", "transactions")
NULL

setAs("matrix", "transactions",
  function(from)
    new(
      "transactions",
      as(from, "itemMatrix"),
      itemsetInfo = data.frame(
        transactionID = dimnames(from)[[1]],
        stringsAsFactors = FALSE
      )
    ))

setAs("transactions", "matrix",
  function(from) {
    to <- as(as(from, "itemMatrix"), "matrix")
    if (length(i <- from@itemsetInfo[["transactionID"]]))
      dimnames(to)[[1]] <- i
    to
  })

setAs("ngCMatrix", "transactions",
  function(from)
    as(as(from, "itemMatrix"), "transactions"))


setAs("list", "transactions",
  function(from)
    new(
      "transactions",
      as(from, "itemMatrix"),
      itemsetInfo = data.frame(transactionID = names(from),
        stringsAsFactors = FALSE)
    ))

setAs("transactions", "list",
  function(from)
    LIST(from, decode = TRUE))

setAs("data.frame", "transactions",
  function(from) {
    if (!length(from))
      return(new("transactions"))
    
    ## handle logical (only translate TRUE into items)
    if (any(logicals <- sapply(from, is.logical))) {
      for (i in which(logicals)) {
        f <- from[[i]]
        f[!f] <- NA
        from[[i]] <- as.factor(f)
      }
    }
    
    ## check that everything is factor
    if (!all((p <- sapply(from, is.factor)))) {
      warning(
        "Column(s) ",
        paste(which(!p), collapse = ", "),
        " not logical or factor. Applying default discretization (see '? discretizeDF').",
        call. = FALSE
      )
      from <- discretizeDF(from)
    }
    
    p <- seq(nrow(from))
    x <- lapply(from, function(x)
      tapply(p, x, eval, simplify = FALSE))
    
    ## variable names and levels
    l <- unlist(lapply(x, names), use.names = FALSE)
    v <- rep(names(x), sapply(x, length))
    
    ## create sparse encoding
    x <- unlist(x, recursive = FALSE, use.names = FALSE)
    p <- sapply(x, length)
    x <- unlist(x, use.names = FALSE)
    x <- new(
      "ngCMatrix",
      p   = c(0L, cumsum(p)),
      i   = x - 1L,
      Dim = c(dim(from)[1], length(p))
    )
    
    iInfo <- data.frame(
      labels        = paste(v, l, sep = "="),
      variables     = as.factor(v),
      levels        = as.factor(l),
      stringsAsFactors = FALSE
    )
    
    ## fix labels for logicals
    logicals <-
      which(iInfo[, "variables"] %in% colnames(from)[logicals])
    iInfo[logicals, "labels"] <-
      as.character(iInfo[logicals, "variables"])
    
    tInfo <- data.frame(transactionID = rownames(from),
      stringsAsFactors = FALSE)
    
    new(
      "transactions",
      data     = t(x),
      itemInfo = iInfo,
      itemsetInfo = tInfo
    )
  })

# This does not reverse coercion data.frame -> transactions
# it is just used for output formatting!

setAs("transactions", "data.frame",
  function(from) {
    if (!length(from))
      return (data.frame())
    
    if (!length(itemsetInfo(from)))
      return(data.frame(items = labels(from)))
    
    #cbind(itemsetInfo(from), data.frame(items = labels(from)))
    ## Deal with the case when transactionInfo contains items
    
    df <-
      cbind(data.frame(items = labels(from)), transactionInfo(from))
    fromnames <- colnames(transactionInfo(from))
    m <- match("items", fromnames)
    if (!is.na(m)) {
      warning("items in transactionInfo was relabeled to ransactionInfo.items!")
      colnames(df)[m + 1L] <- "transactionInfo.items"
    }
    df
  })

