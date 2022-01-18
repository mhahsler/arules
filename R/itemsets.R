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


#' Class itemsets --- A Set of Itemsets
#'
#' The `itemsets` class represents a set of itemsets and the associated
#' quality measures.
#'
#' Itemsets are usually created by calling an association rule mining algorithm
#' like [apriori()].
#' To create itemsets manually, the itemMatrix for the items of the itemsets
#' can be created using [itemCoding].  An example is in the Example
#' section below.
#'
#' Mined itemsets sets contain several interest measures accessible
#' with the `quality()` method.  Additional measures can be
#' calculated via [interestMeasure()].
#'
#' @include associations.R
#' @name itemsets-class
#' @aliases itemsets
#' @family associations functions
#' 
#' @param object,x the object
#' @param ... further argments
#' @param value replacement value
#' @param items an [itemMatrix] or an object that can be converted using [encode()].
#' @param itemLabels item labels used for `encode()`.
#' @param quality a data.frame with quality information (one row per itemset).
#' 
#' @slot items an [itemMatrix] object representing the itemsets.        
#' @slot tidLists a [tidLists] or `NULL`.
#' @slot quality a data.frame with quality information
#' @slot info a list with mining information.
#' 
#' @section Objects from the Class: Objects are the result of calling the
#' functions [apriori()] (e.g., with `target = "frequent
#' itemsets"` in the parameter list) or [eclat()].  
#' 
#' Objects can also
#' be created by calls of the form `new("itemsets", ...)`
#' or by using the constructor function
#' `itemsets()`.
#' 
#' @author Michael Hahsler
#' @keywords classes
#' 
#' @seealso 
#' Superclass: [associations]
#' 
#' @examples
#' data("Adult")
#'
#' ## Mine frequent itemsets with Eclat.
#' fsets <- eclat(Adult, parameter = list(supp = 0.5))
#'
#' ## Display the 5 itemsets with the highest support.
#' fsets.top5 <- sort(fsets)[1:5]
#' inspect(fsets.top5)
#'
#' ## Get the itemsets as a list
#' as(items(fsets.top5), "list")
#'
#' ## Get the itemsets as a binary matrix
#' as(items(fsets.top5), "matrix")
#'
#' ## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
#' ## Warning: for efficiency reasons, the ngCMatrix you get is transposed
#' as(items(fsets.top5), "ngCMatrix")
#'
#' ## Manually create itemsets with the item coding in the Adult dataset
#' ## and calculate some interest measures
#' twoitemsets <- itemsets(
#'   items = list(
#'     c("age=Young", "relationship=Unmarried"),
#'     c("age=Old")
#'   ), itemLabels = Adult)
#'
#' quality(twoitemsets) <- data.frame(support = interestMeasure(twoitemsets,
#'   measure = c("support"), transactions = Adult))
#'
#' inspect(twoitemsets)
#' @aliases show,itemsets-method
setClass(
  "itemsets",
  representation(items = "itemMatrix",
    tidLists = "tidLists_or_NULL"),
  
  contains = "associations",
  
  prototype(
    tidLists = NULL,
    quality = data.frame(),
    info = list()
  ),
  
  validity = function(object) {
    ## if tidLists exists, check dimensions
    ## Note, we cannot check dim(object@tidLists)[2] here since we
    ## don't know the number of transactions in the used data set!
    if (length(tidLists(object)) &&
        length(tidLists(object)) != length(object))
      return("tidLists does not match number of itemsets")
    
    ## if quality exists, check dimensions
    if (length(quality(object)) &&
        nrow(quality(object)) != length(object))
      return("quality does not match number of itemsets")
    
    TRUE
  }
)

#' @rdname itemsets-class
itemsets <- function(items, itemLabels = NULL, quality = data.frame()) {
  if (!is(items, "itemMatrix"))
    items <- encode(items, itemLabels = itemLabels)
  
  new("itemsets",
    items = items,
    quality = quality)
}


setClass(
  "summary.itemsets",
  representation(tidLists = "logical", items = "summary.itemMatrix"),
  contains = "summary.associations"
)

#' @describeIn itemsets-class create a summary
#' @aliases summary.itemsets-class show,summary.itemsets-method
setMethod("summary", signature(object = "itemsets"),
  function(object, ...) {
    new(
      "summary.itemsets",
      
      length   = length(object),
      items    = summary(object@items,  ...),
      quality  = if (length(object@quality))
        summary(object@quality)
      else
        summary(NULL),
      info     = object@info,
      tidLists = !is.null(object@tidLists)
    )
  })

setMethod("show", signature(object = "summary.itemsets"),
  function(object) {
    cat("set of", object@length, "itemsets\n")
    
    if (object@length) {
      cat("\nmost frequent items:\n")
      print(object@items@itemSummary)
      cat("\nelement (itemset/transaction) length distribution:")
      print(object@items@lengths)
      
      cat("\n")
      print(object@items@lengthSummary)
      
      cat("\nsummary of quality measures:\n")
      print(object@quality)
      cat("\nincludes transaction ID lists:", object@tidLists, "\n")
      
      if (length(object@info)) {
        info <- object@info
        if (is(info$data, "language"))
          info$data <- deparse(info$data)
        
        cat("\nmining info:\n")
        print(data.frame(info, row.names = ""))
      }
      
    }
    invisible(NULL)
  })

#' @describeIn itemsets-class get the number of itemsets.
setMethod("length", signature(x = "itemsets"),
  function(x)
    length(x@items))

#' @describeIn itemsets-class get the number of items (columns) in the current encoding.
setMethod("nitems", signature(x = "itemsets"),
  function(x)
    ncol(items(x)))

#' @describeIn itemsets-class get the itemset labels.
setMethod("labels", signature(object = "itemsets"),
  function(object, ...)
    labels(object@items, ...))

#' @describeIn itemsets-class get the item labels.
setMethod("itemLabels", signature(object = "itemsets"),
  function(object)
    itemLabels(object@items))

#' @describeIn itemsets-class replace the item labels.
setReplaceMethod("itemLabels", signature(object = "itemsets"),
  function(object, value) {
    itemLabels(items(object)) <- value
    object
  })

#' @describeIn itemsets-class get item info data.frame.
setMethod("itemInfo", signature(object = "itemsets"),
  function(object)
    object@items@itemInfo)

#' @describeIn itemsets-class get items as an itemMatrix.
setMethod("items", signature(x = "itemsets"),
  function(x)
    x@items)

#' @describeIn itemsets-class with a different itemMatrix.
setReplaceMethod("items", signature(x = "itemsets"),
  function(x, value) {
    x@items <- value
    validObject(x)
    x
  })


setGeneric("tidLists", function(x) standardGeneric("tidLists"))

#' @describeIn itemsets-class get tidLists stored in the object (if any).
setMethod("tidLists", signature(x = "itemsets"),
  function(x)
    x@tidLists)

#' @rdname itemsets-class
#' @name coercion-itemsets
#' @aliases 
#' coerce,itemsets,data.frame-method
#' 
#' @section Coercions:
#' 
#' * `as("itemsets", "data.frame")`
NULL


setAs("itemsets", "data.frame",
  function(from) {
    if (!length(from))
      return (data.frame())
    if (!length(from@quality))
      return(data.frame(itemsets = labels(from)))
    data.frame(items = labels(from), from@quality)
  })
