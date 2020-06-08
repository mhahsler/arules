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
  function (x, decreasing = TRUE, na.last = NA, 
    by = "support", order = FALSE, ...) {
    
    if(!is.na(na.last)) stop("na.last not supported. NAs are always put last.")
    
    q <- quality(x)
    m <- pmatch(by, colnames(q))
    if(any(is.na(m))) stop("Unknown interest measure to sort by.")
    
    q <- q[,m, drop = FALSE]
  
    if(length(x) == 0) return(x)
    
    o <- do.call(base::order, c(q, list(na.last = TRUE, 
      decreasing = decreasing)))
    
    if(order) o
    else x[o]
  })

setMethod("head", signature(x = "associations"),
  function (x, n = 6L, by = NULL, decreasing = TRUE, ...) {
    if(length(x) < 1) return(x)
    if(!is.null(by)) o <- sort(x, by = by, decreasing = decreasing, order = TRUE)
    else o <- seq_len(length(x))
    
    x[head(o, n = n, ...)]
  })

setMethod("tail", signature(x = "associations"),
  function (x, n = 6L, by = NULL, decreasing = TRUE, ...) {
    if(length(x) < 1) return(x)
    if(!is.null(by)) o <- sort(x, by = by, decreasing = decreasing, order = TRUE)
    else o <- seq_len(length(x))
    
    x[tail(o, n = n, ...)]
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

setMethod("%in%", signature(x = "associations", table = "associations"),
  function(x, table) match(x, table)
)

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

