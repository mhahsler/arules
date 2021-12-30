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


#' Class associations --- A Set of Associations
#'
#' The `associations` class is a virtual class which is extended to
#' represent mining result (e.g., sets of [itemsets] or
#' [rules]). The class defines some common methods for its subclasses.
#'
#' The implementations of `associations` store itemsets (e.g., the LHS and
#' RHS of a rule) as objects of class [itemMatrix] (i.e., sparse
#' binary matrices). Quality measures (e.g., support) are stored in a
#' data.frame accessible via method `quality()`.
#'
#' See Sections Functions and See Also to see all available methods.
#'
#' **Note:** Associations can store multisets with duplicated elements. Duplicated
#' elements can result from combining several sets of associations.  Use
#' [unique()] to remove duplicate associations.
#'
#' @include itemMatrix.R
#' @name associations-class
#' @aliases associations
#' @family associations functions
#'
#' @section Objects from the Class: 
#' A virtual class: No objects may be created
#' from it.
#' 
#' @slot quality a data.frame
#' @slot info a list 
#' 
#' @param x,object the object.
#' @param n number of elements
#' @param by sort by this interest measure
#' @param decreasing sort in decreasing order? 
#' @param ... further arguments.
#' @param value the replacement value.
#' 
#' @author Michael Hahsler
#' @keywords classes
#' 
#' @seealso 
#' Subclasses: [rules], [itemsets]
#' 
#' @aliases initialize,associations-method show,associations-method
#' t,associations-method summary.associations-class
setClass(
  "associations",
  representation(
    quality = "data.frame", 
    info = "list", 
    "VIRTUAL")
  )

setMethod(initialize, "associations", function(.Object, ...) {
  .Object <- callNextMethod()
  
  ## fix empty data.frame in itemsetInfo
  if (all(dim(.Object@quality) == 0))
    .Object@quality <-
      data.frame(matrix(ncol = 0, nrow = length(.Object)))
  
  validObject(.Object)
  .Object
})

setMethod("show", signature(object = "associations"),
  function(object) {
    cat("set of", length(object), class(object), "\n")
    invisible(NULL)
  })

setMethod("t", signature(x = "associations"),
  function(x) {
    stop("Object not transposable!")
  })


setClass(
  "summary.associations",
  representation(
    length  = "integer",
    quality = "table",
    info = "list" ,
    "VIRTUAL"
  )
)

setGeneric("quality",
  function(x)
    standardGeneric("quality"))
setGeneric("quality<-",
  function(x, value)
    standardGeneric("quality<-"))

#' @describeIn associations-class returns the quality data.frame.
#' @aliases quality
setMethod("quality", signature(x = "associations"),
  function(x) {
    x@quality
  })

#' @describeIn associations-class replaces the quality data.frame. The lengths of the vectors in the data.frame have to equal the number of associations in the set.
#' @aliases quality<-
setReplaceMethod("quality", signature(x = "associations"),
  function(x, value) {
    x@quality <- value
    validObject(x)
    x
  })


setGeneric("info",
  function(x)
    standardGeneric("info"))
setGeneric("info<-",
  function(x, value)
    standardGeneric("info<-"))

#' @describeIn associations-class returns the info list.
#' @aliases info
setMethod("info", signature(x = "associations"),
  function(x) {
    x@info
  })

#' @describeIn associations-class replaces the info list.
#' @aliases info<-
setReplaceMethod("info", signature(x = "associations"),
  function(x, value) {
    x@info <- value
    ## no need to validObject(x)
    x
  })

#setGeneric("head") # generic in Matrix

#' @describeIn associations-class returns the first n associations.
#' @aliases head
setMethod("head", signature(x = "associations"),
  function (x,
    n = 6L,
    by = NULL,
    decreasing = TRUE,
    ...) {
    if (length(x) < 1)
      return(x)
    if (!is.null(by))
      o <- sort(x,
        by = by,
        decreasing = decreasing,
        order = TRUE)
    else
      o <- seq_len(length(x))
    
    x[head(o, n = n, ...)]
  })


#setGeneric("tail") # generic in Matrix

#' @describeIn associations-class returns the last n associations.
#' @aliases tail
setMethod("tail", signature(x = "associations"),
  function (x,
    n = 6L,
    by = NULL,
    decreasing = TRUE,
    ...) {
    if (length(x) < 1)
      return(x)
    if (!is.null(by))
      o <- sort(x,
        by = by,
        decreasing = decreasing,
        order = TRUE)
    else
      o <- seq_len(length(x))
    
    x[tail(o, n = n, ...)]
  })


setGeneric("items",
  function(x)
    standardGeneric("items"))
setGeneric("items<-",
  function(x, value)
    standardGeneric("items<-"))

#' @describeIn associations-class dummy method. This method has to be implemented by all subclasses of associations and return the items which make up each association as an object of class [itemMatrix].
#' @aliases items
#' @aliases items<-
setMethod("items", signature(x = "associations"),
  function(x) {
    stop(paste("Method items not implemented for class", class(x), "\n"))
  })

#' @describeIn associations-class dummy method. This method has to be implemented by all subclasses of associations and return the number of elements in the association.
setMethod("length", signature(x = "associations"),
  function(x) {
    stop(paste("Method length not implemented for class", class(x), "\n"))
  })

setGeneric("labels")

#' @describeIn associations-class dummy method. This method has to be implemented by all subclasses of associations and return a vector of length(object) of labels for the elements in the association.
setMethod("labels", signature(object = "associations"),
  function(object) {
    stop(paste(
      "Method duplicated not implemented for class",
      class(object),
      "\n"
    ))
  })
