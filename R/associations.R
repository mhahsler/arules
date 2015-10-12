#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
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



##*******************************************************
## Virtual class associations
##
## vitual class which defines the (and implements some) common 
## functionality for associations (e.g., rules, itemsets)


##************************************************
## common methods

## accessors for quality and info
setMethod("quality", signature(x = "associations"),
  function(x) {
    x@quality
  })

setReplaceMethod("quality", signature(x = "associations"),
  function(x, value) {
    x@quality <- value
    validObject(x)
    x
  })

setMethod("info", signature(x = "associations"),
  function(x) {
    x@info
  })

setReplaceMethod("info", signature(x = "associations"),
  function(x, value) {
    x@info <- value
    ## no need to validObject(x)
    x
  })


## sort + unique
setMethod("sort", signature(x = "associations"),
  function (x, decreasing = TRUE, na.last = NA, by = "support", order = FALSE, ...) {
    q <- quality(x)
    q <- q[, pmatch(by, colnames(q)), drop = FALSE]
    if(is.null(q)) stop("Unknown interest measure to sort by.")
    if(length(x) == 0) return(x)
    
    o <- do.call(base::order, c(q, list(na.last = na.last, 
      decreasing = decreasing)))
    
    if(order) o
    else x[o]
  })



## this needs a working implementation of duplicated for the 
## type of associations
setMethod("unique", signature(x = "associations"),
  function(x,  incomparables = FALSE, ...) {
    x[!duplicated(x, incomparables = incomparables, ...)]
  })

## show
setMethod("show", signature(object = "associations"),
  function(object) {
    cat("set of",length(object),class(object),"\n")
    invisible(NULL)
  })

## no t for associations
setMethod("t", signature(x = "associations"),
  function(x) {
    stop("Object not transposable!")  
  })


##************************************************
## implementations of associations must provide minimal interface

setMethod("items", signature(x = "associations"),
  function(x) {
    stop(paste("Method items not implemented for class", class(x),"\n"))
  })

setMethod("length", signature(x = "associations"),
  function(x) {
    stop(paste("Method length not implemented for class", class(x),"\n"))
  })

setMethod("labels", signature(object = "associations"),
  function(object) {
    stop(paste("Method duplicated not implemented for class", class(object),"\n"))
  })

