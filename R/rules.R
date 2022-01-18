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


#' Class rules --- A Set of Rules
#'
#' Defines the `rules` class to represent a set of association rules and methods to work
#' with `rules`.
#'
#' Mined rule sets typically contain several interest measures accessible with
#' the [quality()] method. Additional measures can be calculated via
#' [interestMeasure()].
#' 
#' To create rules manually, the itemMatrix for the LHS and the RHS of the
#' rules need to be compatible. See [itemCoding] for details.
#' @include associations.R
#' @name rules-class
#' @aliases rules
#' @family associations functions
#' 
#' @param object,x the object
#' @param ... further arguments
#' @param ruleSep rule separation symbol
#' @param value replacement value
#' @param rhs,lhs [itemMatrix] objects or objects that can be converted using [encode()].
#' @param itemLabels a vector of all
#' possible item labels (character) or a transactions object to copy the item
#' coding used for `encode()` (see [itemCoding] for details).
#' @param quality a data.frame with quality information (one row per rule).    
#'
#' @slot lhs,rhs [itemMatrix] representing the left-hand-side and right-hand-side of
#'   the rules. 
#' @slot quality the quality data.frame
#' @slot info  a list with mining information.
#'
#' @section Objects from the Class: 
#' Objects are the result of calling the
#' function [apriori()]. Objects can also be created by calls of the
#' form `new("rules", ...)`
#' or by using the constructor function `rules()`.
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
#' ## Mine rules
#' rules <- apriori(Adult, parameter = list(support = 0.3))
#' rules
#'
#' ## Select a subset of rules using partial matching on the items
#' ## in the right-hand-side and a quality measure
#' rules.sub <- subset(rules, subset = rhs %pin% "sex" & lift > 1.3)
#'
#' ## Display the top 3 support rules
#' inspect(head(rules.sub, n = 3, by = "support"))
#'
#' ## Display the first 3 rules
#' inspect(rules.sub[1:3])
#'
#' ## Get labels for the first 3 rules
#' labels(rules.sub[1:3])
#' labels(rules.sub[1:3], itemSep = " + ", setStart = "", setEnd="",
#'   ruleSep = " ---> ")
#'
#' ## Manually create rules using the item coding in Adult and calculate some interest measures
#' twoRules <- rules(
#'   lhs = list(
#'     c("age=Young", "relationship=Unmarried"),
#'     c("age=Old")
#'   ),
#'   rhs = list(
#'     c("income=small"),
#'     c("income=large")
#'   ),
#'   itemLabels = Adult
#' )
#'
#' quality(twoRules) <- interestMeasure(twoRules,
#'   measure = c("support", "confidence", "lift"), transactions = Adult)
#'
#' inspect(twoRules)
#'
#' @aliases initialize,rules-method
setClass(
  "rules",
  representation(lhs = "itemMatrix",
    rhs = "itemMatrix"),
  contains = "associations",
  
  prototype(quality = data.frame(), info = list()),
  
  validity = function(object) {
    ## check dimensions
    if (!all(dim(object@lhs) == dim(object@rhs)))
      return("dimensions of lhs and rhs do not match")
    
    ## check quality
    if (length(object@quality) &&
        length(object@quality[[1]]) != length(object@lhs))
      return("quality does not match the number of rules")
    
    ## check lhs and rhs
    if (!validObject(object@lhs))
      return("lhs of rules is not a valid itemMatrix")
    if (!validObject(object@rhs))
      return("rhs of rules is not a valid itemMatrix")
    
    ## Removed check so rhs can be empty... (MFH)
    ## check size of rhs
    #if(any(size(object@rhs)<1)) return("rhs is empty for at least one rule")
    
    ## check item labels
    if (!identical(colnames(object@lhs), colnames(object@rhs)))
      return("item labels in lhs and rhs do not match")
    
    ## check union
    if (any(size(items(object)) != size(object@rhs) + size(object@lhs)))
      return("lhs and rhs of some rules is not distinct")
    
    TRUE
  }
)

setMethod("initialize", "rules",
  function(.Object, lhs, rhs, ...) {
    if (!identical(colnames(lhs), colnames(rhs))) {
      warning("item labels in lhs and rhs do not match. recoding rhs!")
      rhs <- recode(rhs, itemLabels = lhs)
    }
    
    .Object@lhs <- lhs
    .Object@rhs <- rhs
    
    .Object <- callNextMethod(.Object, ...)
    
    .Object
  })

#' @rdname rules-class
rules <- function(rhs, lhs, itemLabels = NULL, quality = data.frame()) {
  if (!is(lhs, "itemMatrix"))
    lhs <- encode(lhs, itemLabels = itemLabels)
  if (!is(rhs, "itemMatrix"))
    rhs <- encode(rhs, itemLabels = itemLabels)
  
  new("rules",
    lhs = lhs,
    rhs = rhs,
    quality = quality)
}


setClass(
  "summary.rules",
  representation(lengths = "table", lengthSummary = "table"),
  contains = "summary.associations"
)

#' @describeIn rules-class create a summary
#' @aliases summary.rules-class show,summary.rules-method
setMethod("summary", signature(object = "rules"),
  function(object, ...) {
    sizes <- size(object@lhs) + size(object@rhs)
    
    new(
      "summary.rules",
      length        = length(object),
      lengths       = table(sizes),
      lengthSummary = summary(sizes),
      quality       =
        if (length(object@quality))
          summary(object@quality)
      else
        summary(NULL),
      info          = object@info
    )
  })

setMethod("show", signature(object = "summary.rules"),
  function(object) {
    cat("set of", object@length, "rules\n\n")
    
    if (object@length) {
      cat("rule length distribution (lhs + rhs):")
      print(object@lengths)
      
      cat("\n")
      print(object@lengthSummary)
      
      cat("\nsummary of quality measures:\n")
      print(object@quality)
    
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

#' @describeIn rules-class returns the number of rules.
setMethod("length", signature(x = "rules"),
  function(x)
    length(x@lhs))

#' @describeIn rules-class returns the number of items used in the current encoding.
setMethod("nitems", signature(x = "rules"),
  function(x)
    ncol(lhs(x)))

#' @describeIn rules-class labels for the rules.
setMethod("labels", signature(object = "rules"),
  function(object, ruleSep = " => ", ...)
    paste(labels(object@lhs, ...), ruleSep,
      labels(object@rhs, ...), sep = ""))

#' @describeIn rules-class returns item labels for the current encoding.
setMethod("itemLabels", signature(object = "rules"),
  function(object)
    itemLabels(lhs(object)))

#' @describeIn rules-class change the item labels in the current encoding.
setReplaceMethod("itemLabels", signature(object = "rules"),
  function(object, value) {
    ### these low level operation avoids the validity check till all labels are changed
    itemLabels(object@lhs) <- value
    itemLabels(object@rhs) <- value
    validObject(object)
    object
  })


#' @describeIn rules-class returns the item info data.frame.
setMethod("itemInfo", signature(object = "rules"),
  function(object)
    itemInfo(object@lhs))

#' @rdname rules-class
setGeneric("lhs",
  function(x)
    standardGeneric("lhs"))

#' @describeIn rules-class returns the LHS of the rules as an [itemMatrix].
setMethod("lhs", signature(x = "rules"),
  function(x)
    x@lhs)

#' @rdname rules-class
setGeneric("lhs<-",
  function(x, value)
    staindardGeneric("lhs<-"))

#' @describeIn rules-class replaces the LHS of the rules with an [itemMatrix].
setReplaceMethod("lhs", signature(x = "rules"),
  function(x, value) {
    x@lhs <- value
    validObject(x)
    x
  })

#' @rdname rules-class
setGeneric("rhs",
  function(x)
    standardGeneric("rhs"))

#' @rdname rules-class
setGeneric("rhs<-",
  function(x, value)
    standardGeneric("rhs<-"))

#' @describeIn rules-class replaces the RHS of the rules with an [itemMatrix].
setReplaceMethod("rhs", signature(x = "rules"),
  function(x, value) {
    x@rhs <- value
    validObject(x)
    x
  })

#' @describeIn rules-class returns the RHS of the rules as an [itemMatrix].
setMethod("rhs", signature(x = "rules"),
  function(x)
    x@rhs)

#' @describeIn rules-class returns all items in a rule (LHS and RHS) an [itemMatrix].
setMethod("items", signature(x = "rules"),
  function(x)
    itemUnion(x@lhs, x@rhs))

#' @rdname rules-class
setGeneric("generatingItemsets",
  function(x)
    standardGeneric("generatingItemsets"))

#' @describeIn rules-class returns a collection of the itemsets which generated the rules, one itemset for each rule. Note that the collection can be a multiset and contain duplicated elements. Use [unique()] to remove duplicates and obtain a proper set. This method produces the same as the result as calling `items()`, but wrapped into an [itemsets] object with support information.
setMethod("generatingItemsets", signature(x = "rules"),
  function(x)
    new(
      "itemsets",
      items   = items(x),
      quality = data.frame(support = x@quality[["support"]]),
      info = x@info
    ))

#' @rdname rules-class
#' @name coercion-rules
#' @aliases 
#' coerce,rules,data.frame-method
#' 
#' @section Coercions:
#' 
#' * `as("rules", "data.frame")`
NULL

setAs("rules", "data.frame",
  function(from) {
    if (!length(from))
      return (data.frame())
    if (!length(from@quality))
      return(data.frame(rules = labels(from)))
    data.frame(rules = labels(from), from@quality)
  })



## this utility function joins the lhs and rhs so it can be
## used for duplicated, unique, etc. 0 is used as separator
## which avoids coercion to character.
.joinedList <- function(x) {
  if (class(x) != "rules")
    stop("not of class rules")
  
  mapply(
    function(l, r)
      c(l, 0, r),
    LIST(x@lhs, decode = FALSE),
    LIST(x@rhs, decode = FALSE),
    SIMPLIFY = FALSE
  )
}


