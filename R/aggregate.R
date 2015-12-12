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



## aggregate items to item groups
setMethod("aggregate", signature(x = "itemMatrix"),
  function(x, by) {
    
    ## we can specify the name from itemInfo in by
    if(length(by) == 1 && !is.null(itemInfo(x)[[by]])) 
      by <- itemInfo(x)[[by]]
    
    by <- as.factor(by)
    
    if(length(by) != nitems(x)) stop("Name not available in itemInfo or supplied number of by does not match number of items in x!")
    
    ## create an indicator matrix (cols are items)
    aggrMat <- as(sapply(levels(by),
      FUN = function(l) as.numeric(by == l)), "dgCMatrix")
    
    ## count the items for each group and make binary
    x@data <- as(crossprod(aggrMat, as(as(x, "ngCMatrix"), "dgCMatrix")), 
      "ngCMatrix")
    
    ## fix itemInfo 
    ii <- x@itemInfo
    ii <- aggregate(ii, by = list(labels = by), FUN = unique)
    ii <- ii[,!sapply(ii, is.list), drop = FALSE]
    x@itemInfo <- ii
    
    validObject(x)
    x
    
    
  })

setMethod("aggregate", signature(x = "itemsets"),
  function(x, by) {
    
    new("itemsets", items=aggregate(items(x), by))
    
    ## first support value is used
    x <- unique(x)
    x
  })

setMethod("aggregate", signature(x = "rules"),
  function(x, by) {
    
    rhs <- aggregate(rhs(x), by)
    lhs <- aggregate(lhs(x), by)
    
    ## check if lhs items have to be removed
    lhs <- itemSetdiff(lhs, rhs)
    
    ### remove non-unique rules
    ## first support value is used
    x <- new("rules", lhs=lhs, rhs=rhs)
    x <- unique(x)
    x
  })


## mine multi-level rules 
addAggregate <- function(x, by, postfix="*") {
  ## get aggregated items
  x_aggr <- aggregate(x, by)
  itemLabels(x_aggr) <- paste(itemLabels(x_aggr), postfix, sep="")
  itemInfo(x_aggr)[["aggregatedBy"]] <- by 
  
  ## merge with transactions
  x_m <- merge(x, x_aggr)
  
  ## add technical itemInfo
  itemInfo(x_m)[["aggregateLevels"]] <- c(rep(1L, times = nitems(x)), 
    rep(2L, times = nitems(x_aggr)))
  
  itemInfo(x_m)[["aggregateID"]] <- c(nitems(x) + 
      as.integer(itemInfo(x)[[by]]), 
    rep(0, times = nitems(x_aggr)))
  
  x_m
}

## remove rules of type item -> its own category
filterAggregate <- function(x) {
  levels <- itemInfo(x)[["aggregateLevels"]]
  aggr <- itemInfo(x)[["aggregateID"]]
  if(is.null(levels) || is.null(aggr))
    stop("No aggregated hierarchy info available!")
  
  m <- as(items(x), "ngCMatrix")
  
  rem <- logical(length(x))
  for(i in 1:nrow(m)) {       ## number of items including level 1
    if(levels[i] > 1) break   ## done with items 
    rem <- rem | colSums(m[i,, drop=FALSE]) & 
      colSums(m[aggr[i],, drop=FALSE])
  }
  
  x[!rem]
}