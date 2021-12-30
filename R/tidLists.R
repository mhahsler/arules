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


#' Class tidLists --- Transaction ID Lists for Items/Itemsets
#'
#' Class to represent transaction ID lists and associated methods.
#' 
#' Transaction ID lists contains a set of lists.  Each list is associated with
#' an item/itemset and stores the IDs of the transactions which support the
#' item/itemset. 
#' 
#' `tidLists` uses the class
#' [Matrix::ngCMatrix-class] to efficiently store the
#' transaction ID lists as a sparse matrix.  Each column in the matrix
#' represents one transaction ID list.
#'
#' `tidLists` can be used for different purposes.  For some operations
#' (e.g., support counting) it is efficient to coerce a
#' [transactions] database into `tidLists` where each
#' list contains the transaction IDs for an item (and the support is given by
#' the length of the list).
#'
#' The implementation of the Eclat mining algorithm (which uses transaction ID
#' list intersection) can also produce transaction ID lists for the found
#' itemsets as part of the returned [itemsets] object.
#' These lists can then be used for further computation.
#'
#' @include associations.R transactions.R
#' @name tidLists-class
#' @aliases tidLists 
#' @family itemMatrix and transactions functions
#' 
#' @param x,object the object
#' @param maxsum maximum numbers of itemsets shown in the summary
#' @param ... further arguments
#' @param value replacement value
#' 
#' @section Objects from the Class: 
#' Objects are created
#'
#' * as part of the 
#' [itemsets] mined by [eclat()] with `tidLists = TRUE` in the
#' [ECparameter] object. 
#' * by [supportingTransactions()].
#' * by coercion from
#' an object of class [transactions]. 
#' * by calls of the form  `new("tidLists", ...)`.
#' 
#' @slot data an object of class [ngCMatrix-class] from package \pkg{Matrix}.
#' @slot itemInfo a data.frame
#' @slot transactionInfo a data.frame
#' 
#' @author Michael Hahsler
#' @keywords classes
#' @examples
#' ## Create transaction data set.
#' data <- list(
#'   c("a","b","c"),
#'   c("a","b"),
#'   c("a","b","d"),
#'   c("b","e"),
#'   c("b","c","e"),
#'   c("a","d","e"),
#'   c("a","c"),
#'   c("a","b","d"),
#'   c("c","e"),
#'   c("a","b","d","e")
#'   )
#' data <- as(data, "transactions")
#' data
#'
#' ## convert transactions to transaction ID lists
#' tl <- as(data, "tidLists")
#' tl
#'
#' inspect(tl)
#' dim(tl)
#' dimnames(tl)
#'
#' ## inspect visually
#' image(tl)
#'
#' ## mine itemsets with transaction ID lists
#' f <- eclat(data, parameter = list(support = 0, tidLists = TRUE))
#' tl2 <- tidLists(f)
#' inspect(tl2)
#' @aliases initialize,tidLists-method show,tidLists-method 
tidLists <- setClass(
  "tidLists",
  representation(
    data            = "ngCMatrix",
    itemInfo        = "data.frame",
    transactionInfo = "data.frame"
  ),
  
  prototype(itemInfo = data.frame(), transactionInfo = data.frame()),
  
  validity = function(object) {
    ## check number of labels in itemInfo
    ## no labels (empty data.frame)
    if (length(object@itemInfo) &&
        length(object@itemInfo[["labels"]]) != dim(object)[1])
      return("number of item labels does not match number of rows")
    
    if (length(object@transactionInfo) &&
        length(object@transactionInfo[[1]]) != dim(object)[2])
      return("transactionInfo does not match number of transactions")
    
    TRUE
  }
)

setMethod(initialize, "tidLists", function(.Object, ...) {
  .Object <- callNextMethod()
  
  validObject(.Object)
  .Object
})

setClassUnion("tidLists_or_NULL", c("tidLists", "NULL"))

setClass(
  "summary.tidLists",
  representation(
    Dim                = "integer",
    transactionSummary = "integer",
    lengths            = "table",
    lengthSummary      = "table",
    itemInfo           = "data.frame"
  )
)

setMethod("show", signature(object = "tidLists"),
  function(object) {
    cat(
      "tidLists in sparse format with\n",
      nrow(object),
      "items/itemsets (rows) and\n",
      ncol(object),
      "transactions (columns)\n"
    )
    invisible(NULL)
  })

#' @describeIn tidLists-class create a summary
#' @aliases summary.tidLists-class show,summary.tidLists-method
setMethod("summary", signature(object = "tidLists"),
  function(object, maxsum = 6, ...) {
    tfs   <-
      sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
    tsum  <- head(tfs, maxsum - as.integer(1))
    tsum  <- c(tsum, "(Other)" = sum(tfs) - sum(tsum))
    sizes <- size(object)
    
    new(
      "summary.tidLists",
      Dim                = dim(object),
      transactionSummary = tsum,
      lengths            = table(sizes),
      lengthSummary      = summary(sizes),
      itemInfo           = head(itemInfo(object), 3)
    )
  })

setMethod("show", signature(object = "summary.tidLists"),
  function(object) {
    cat(
      "tidLists in sparse format with\n",
      object@Dim[1],
      "items/itemsets (rows) and\n",
      object@Dim[2],
      "transactions (columns)\n"
    )
    
    cat("\nmost frequent transactions:\n")
    print(object@transactionSummary)
    cat("\nitem frequency distribution:\n")
    print(object@lengths)
    
    cat("\n")
    print(object@lengthSummary)
    
    if (length(names(object@itemInfo))) {
      cat("\nincludes extended item information - examples:\n")
      print(object@itemInfo)
    }
    invisible(NULL)
  })

#' @describeIn tidLists-class get dimensions. The rows represent the itemsets and the 
#' columns are the transactions.
setMethod("dim", signature(x = "tidLists"),
  function(x)
    rev(dim(x@data)))

#' @describeIn tidLists-class get dimnames
setMethod("dimnames", signature(x = "tidLists"),
  function(x) {
    colLabels <- x@transactionInfo[["transactionID"]]
    if (!is.null(colLabels))
      colLabels <- as.character(colLabels)
    
    rowLabels <- x@itemInfo[["labels"]]
    
    list(rowLabels, colLabels)
  })

#' @describeIn tidLists-class replace dimnames
setReplaceMethod("dimnames", signature(x = "tidLists",
  value = "list"),
  function(x, value) {
    if (any(dim(x) != sapply(value, length) & !sapply(value, is.null)))
      stop("length of dimnames does not equal the dimension of the object.")
    
    if (!is.null(value[[1]])) {
      x@itemInfo[["labels"]] <- value[[1]]
    }
    
    if (!is.null(value[[2]])) {
      if (ncol(x@transactionInfo) == 0) {
        x@transactionInfo <- data.frame(transactionID = value[[2]])
      } else{
        x@transactionInfo[["transactionID"]] <- value[[2]]
      }
    }
    
    x
  })

#' @describeIn tidLists-class get the number of itemsets.
setMethod("length", signature(x = "tidLists"),
  function(x)
    dim(x)[1])


#' @describeIn tidLists-class this object is not transposable. 
#' `t()` results in an error.
setMethod("t", signature(x = "tidLists"),
  function(x)
    stop(
      "Object not transposable! Use as(x, \"transactions\") for coercion."
    ))

#' @describeIn tidLists-class get the transaction info data.frame
setMethod("transactionInfo", signature(x = "tidLists"),
  function(x)
    x@transactionInfo)

#' @describeIn tidLists-class replace the the transaction info data.frame
setReplaceMethod("transactionInfo", signature(x = "tidLists"),
  function(x, value) {
    x@transactionInfo <- value
    validObject(x)
    x
  })


#' @describeIn tidLists-class get the item info data.frame
setMethod("itemInfo", signature(object = "tidLists"),
  function(object)
    object@itemInfo)

#' @describeIn tidLists-class replace the item info data.frame
setReplaceMethod("itemInfo", signature(object = "tidLists"),
  function(object, value) {
    object@itemInfo <- value
    validObject(object)
    object
  })

#' @describeIn tidLists-class get the item labels
setMethod("itemLabels", signature(object = "tidLists"),
  function(object)
    rownames(object))

#' @describeIn tidLists-class convert the tid lists into a text representation.
setMethod("labels", signature(object = "tidLists"),
  function(object) {
    sapply(as(object, "list"),
      function(x)
        paste("{", paste(x, collapse = ","), "}", sep = ""))
  })

#' @rdname tidLists-class
#' @name coercion-tidLists
#' @aliases 
#' coerce,tidLists,matrix-method
#' coerce,tidLists,list-method 
#' coerce,list,tidLists-method
#' coerce,tidLists,ngCMatrix-method 
#' coerce,tidLists,transactions-method
#' coerce,transactions,tidLists-method 
#' coerce,tidLists,itemMatrix-method
#' coerce,itemMatrix,tidLists-method 
#' 
#' @section Coercion:
#'
#' * as("tidLists", "list") 
#' * as("list", "tidLists")
#' * as("tidLists", "ngCMatrix") 
#' * as("tidLists", "transactions")
#' * as("transactions", "tidLists") 
#' * as("tidLists", "itemMatrix")
#' * as("itemMatrix", "tidLists")  
NULL

setAs("tidLists", "list",
  function(from)
    LIST(from, decode = TRUE))

setAs("list", "tidLists",
  function(from) {
    if (!all(sapply(from, is.atomic)))
      stop("item(s) not atomic")
    i <- lapply(from, sort)
    names(i) <- NULL
    p <- sapply(i, length)
    p <- cumsum(p)
    i <- unclass(factor(unlist(i)))
    l <- attr(i, "levels")
    attr(i, "levels") <- NULL
    i <- new(
      "ngCMatrix",
      p   = c(0L, p),
      i   = i - 1L,
      Dim = c(length(l), length(p))
    )
    new(
      "tidLists",
      data            = i,
      itemInfo        = data.frame(labels = names(from),
        stringsAsFactors = FALSE),
      transactionInfo = data.frame(labels = l, stringsAsFactors = FALSE)
    )
  })

setAs("tidLists", "matrix",
  function(from) {
    to <- as(t(from@data), "matrix")
    dimnames(to) <- dimnames(from)
    to
  })


setAs("tidLists", "ngCMatrix",
  function(from) {
    dimnames(from@data) <- rev(dimnames(from))
    from@data
  })


setAs("tidLists", "transactions",
  function(from)
    new(
      "transactions",
      data            = t(from@data),
      itemInfo        = from@itemInfo,
      itemsetInfo = from@transactionInfo
    ))


setAs("transactions", "tidLists",
  function(from)
    new(
      "tidLists",
      data            = t(from@data),
      itemInfo        = from@itemInfo,
      transactionInfo = transactionInfo(from)
    ))


setAs("tidLists", "itemMatrix",
  function(from) {
    k <- match("transactionID", names(from@transactionInfo))
    if (!is.na(k))
      names(from@transactionInfo)[k] <- "itemsetID"
    
    new(
      "itemMatrix",
      data        = t(from@data),
      itemInfo    = from@itemInfo,
      itemsetInfo = from@transactionInfo
    )
  })


setAs("itemMatrix", "tidLists",
  function(from) {
    k <- match("itemsetID", names(from@itemsetInfo))
    if (!is.na(k))
      names(from@itemsetInfo)[k] <- "transactionID"
    
    new(
      "tidLists",
      data            = t(from@data),
      itemInfo        = from@itemInfo,
      transactionInfo = from@itemsetInfo
    )
  })

