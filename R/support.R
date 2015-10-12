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
## Function support
##
## return  support of itemsets in transactions using tid-list intersections

setMethod("support", signature(x = "itemMatrix"), 
  function(x, transactions, type= c("relative", "absolute"), weighted = FALSE,
    control = NULL) {
    
    if(!is(transactions, "transactions")) stop("transactions missing. Please specify the transactions used to mine the itemsets!")
    
    if(weighted && !("weight" %in% colnames(transactionInfo(transactions)))) 
      stop("transactions do not contain weights. Add a weight column to transactionInfo.")
    
    type <- match.arg(type)
    
    verbose <- if(is.null(control$v))   FALSE       else control$v
    method  <- if(is.null(control$m))   "ptree"     else control$m
    
    methods <- c("ptree", "tidlists")
    
    method <-  methods[pmatch(method , methods)]
    if(is.na(method)) stop("unknown method")
    
    if(verbose) cat("using method:", method, "\n")
    
    ## conform
    k <- match(itemLabels(transactions), itemLabels(x))
    n <- which(is.na(k))
    if (length(n)) {
      k[n] <- x@data@Dim[1] + seq(length(n))
      x@data@Dim[1] <- x@data@Dim[1] + length(n)
      ## may not be needed
      x@itemInfo <-
        transactions@itemInfo <-
        rbind(x@itemInfo, transactions@itemInfo[n,, drop = FALSE])
    }
    if (any(k != seq_len(length(k))))
      transactions@data <-
      .Call("R_recode_ngCMatrix", transactions@data, k, 
        PACKAGE="arules")
    if (transactions@data@Dim[1] <  x@data@Dim[1])
      transactions@data@Dim[1] <- x@data@Dim[1]
    
    if(weighted) { 
      tm <- system.time(support <- support.weighted(x, transactions, control))
      total <- sum(transactionInfo(transactions)[["weight"]])
    } else {
      total <- length(transactions)
      if(method == "ptree") 
        tm <- system.time(support <- support.ptree(x, transactions, control))
      else 
        tm <- system.time(support <- support.tidlists(x, transactions, control))
    }

    if(verbose) cat("timing:", sum(tm[1:2]), "sec.\n")
    
    switch(type,
      relative =  support/total,
      absolute =  support
    )
    
  })


## UNUSED: We have now a C implementation
support.tidlists.inR <- function(x, transactions, control = NULL) {
  
  if (nitems(x) != nitems(transactions))
    stop("number of items in x and transactions do not match.")
  
  ## prepare tid-list and list of itemsets
  tlists <- LIST(as(transactions, "tidLists"), decode = FALSE)
  xitems <- LIST(x, decode = FALSE)
  
  ## select tid-lists for items and do intersection
  support <- sapply(xitems, FUN = function(i) { 
    tidls <- unlist(tlists[i])
    if(!is.null(tidls)) 
      supp <- sum(tabulate(tidls) == length(i))
    else supp <- 0 
    supp
  })
  
  #names(support) <- labels(x)
  support
}

support.tidlists <- function(x, transactions, control = NULL) {
  
  if (nitems(x) != nitems(transactions))
    stop("number of items in x and transactions do not match.")
  
  reduce  <- if(is.null(control$r))    FALSE else control$r
  if(reduce == TRUE) warning("method tidlists does not use reduce")
  
  tid <- as(transactions, "tidLists")
  
  support <- .Call("R_tid_support" ,tid@data, x@data, PACKAGE="arules")
  
  #names(supports) <- labels(x)
  support
}

support.ptree <- function(x, transactions, control = NULL) {
  reduce  <- if(is.null(control$r))    FALSE else control$r
  verbose <- if(is.null(control$v))    FALSE else control$v
  
  .Call("R_pncount", x@data, transactions@data, TRUE, reduce, verbose, 
    PACKAGE="arules")
}
      
support.weighted <- function(x, transactions, control = NULL) {
  verbose <- if(is.null(control$v))    FALSE else control$v
  weights <- as.numeric(transactionInfo(transactions)[["weight"]])
  
  .Call(R_wcount_ngCMatrix, x@data, 
    #t(transactions@data), 
    selectMethod("t", class(transactions@data))(transactions@data), 
    weights, NULL, NULL, verbose, PACKAGE = "arules")
}

## wrapper method for associations
setMethod("support", signature(x = "associations"),
  function(x, transactions, type= c("relative", "absolute"), weighted = FALSE,
    control = NULL) 
    support(items(x), transactions = transactions, type = type, 
      weighted = weighted, control = control)
)

