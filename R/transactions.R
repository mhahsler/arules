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



##*****************************************************
## Class transactions
##
## transaction data

##*****************************************************
## coercions

setAs("matrix", "transactions",
  function(from)
    new("transactions", as(from, "itemMatrix"), 
      transactionInfo = data.frame(transactionID = dimnames(from)[[1]], 
        stringsAsFactors = FALSE)))

setAs("transactions", "matrix",
  function(from) {
    to <- as(as(from, "itemMatrix"), "matrix")
    if (length(i <- from@transactionInfo[["transactionID"]]))
      dimnames(to)[[1]] <- i
    to
  }
)

setAs("ngCMatrix", "transactions",
  function(from) as(as(from, "itemMatrix"), "transactions")
)

setAs("list", "transactions",
  function(from)
    new("transactions", as(from, "itemMatrix"), 
      transactionInfo = data.frame(transactionID = names(from), 
        stringsAsFactors = FALSE)))

setAs("transactions", "list",
  function(from) LIST(from, decode = TRUE))

setMethod("LIST", signature(from = "transactions"),
  function(from, decode = TRUE) {
    to <- LIST(as(from, "itemMatrix"), decode)
    if (decode == TRUE) 
      names(to) <- from@transactionInfo[["transactionID"]]
    to
  }
)

setAs("data.frame", "transactions",
  function(from) {
    if (!length(from))
      return(new("transactions"))
    
    ## handle logical (only translate TRUE into items)
    if(any(logicals <- sapply(from, is.logical))) {
      for(i in which(logicals)){
        f <- from[[i]]
        f[!f] <- NA
        from[[i]] <- as.factor(f)
      }
    }
    
    ## check that everything is factor
    if (!all((p <- sapply(from, is.factor))))
      stop("column(s) ", paste(which(!p), collapse=", "), " not logical or a factor. Use as.factor, as.logical or categorize first.")
    
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
    x <- new("ngCMatrix", p   = c(0L, cumsum(p)),
      i   = x - 1L,
      Dim = c(dim(from)[1], length(p)))
    
    iInfo <- data.frame(
      labels        = paste(v, l, sep = "="),
      variables     = as.factor(v),
      levels        = as.factor(l),
      stringsAsFactors = FALSE
    )
    
    ## fix labels for logicals
    logicals <- which(iInfo[,"variables"] %in% colnames(from)[logicals])
    iInfo[logicals, "labels"] <- as.character(iInfo[logicals, "variables"])
    
    tInfo <- data.frame(transactionID = rownames(from), 
      stringsAsFactors = FALSE)
    
    new("transactions",
      data     = t(x),
      itemInfo = iInfo,
      transactionInfo = tInfo
    )
  }
)

## This does not reverse coercion data.frame -> transactions
## it is just used for output formating!
setAs("transactions", "data.frame",
  function(from) {
    if (!length(from)) return (data.frame())
    
    if (!length(transactionInfo(from))) 
      return(data.frame(items = labels(from)))
    
    #cbind(transactionInfo(from), data.frame(items = labels(from)))
    ## Deal with the case when transactionInfo contains items
    
    df <- cbind(data.frame(items = labels(from)), transactionInfo(from))
    fromnames <- colnames(transactionInfo(from)) 
    m <- match("items", fromnames)
    if (!is.na(m)) {
      warning("items in transactionInfo was relabeled to ransactionInfo.items!")
      colnames(df)[m+1L] <- "transactionInfo.items"
    }
    df   
  }
)

## no t for associations
setMethod("t", signature(x = "transactions"),
  function(x) stop("Object not transposable! Use as(x, \"tidLists\") for coercion to tidLists."))

##*****************************************************
## subset + combine

## drop is unused
setMethod("[", signature(x = "transactions", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop) {
    ## i and j are reversed
    if (!missing(i)) {
      if(any(is.na(i))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if(is.logical(i)) i[is.na(i)] <- FALSE
        else i <- i[!is.na(i)]
      }  
      
      x <- new("transactions", as(x, "itemMatrix")[i,, ..., drop],
        transactionInfo = x@transactionInfo[i,, drop = FALSE])
    }
    
    if (!missing(j))
      x <- new("transactions", as(x, "itemMatrix")[,j, ..., drop],
        transactionInfo = x@transactionInfo)
    
    x
  }
)

setMethod("c", signature(x = "transactions"),
  function(x, ..., recursive = FALSE){
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "transactions"))
        stop("can only combine transactions")
      x <- new("transactions", c(as(x, "itemMatrix"), 
        as(y, "itemMatrix")),
        transactionInfo = .combineMeta(x, y, "transactionInfo"))
    }
    x
  }
)

setMethod("merge", signature(x="transactions"),
  function(x, y, ...) {
    m <- merge(as(x, "itemMatrix"), as(y, "itemMatrix"))
    as(m, "transactions")
  })

##*****************************************************
## show / summary

setMethod("show", signature(object = "transactions"),
  function(object) {
    cat("transactions in sparse format with\n",
      nrow(object), "transactions (rows) and\n",
      ncol(object), "items (columns)\n")
    invisible(NULL)
  }
)

setMethod("summary", signature(object = "transactions"),
  function(object)
    new("summary.transactions",
      summary(as(object, "itemMatrix")),
      transactionInfo = head(object@transactionInfo, 3))
)

setMethod("show", signature(object = "summary.transactions"),
  function(object) {
    cat("transactions as ")
    show(as(object,"summary.itemMatrix"))
    
    if (length(object@transactionInfo)) {
      cat("\nincludes extended transaction information - examples:\n")
      print(object@transactionInfo)
    }
    invisible(NULL)
  }
)

##*****************************************************
## accessors

setMethod("transactionInfo", signature(x = "transactions"),
  function(x) x@transactionInfo)

setReplaceMethod("transactionInfo", signature(x = "transactions"),
  function(x, value) {
    x@transactionInfo <- value
    validObject(x)
    x
  }
)

setMethod("dimnames", signature(x = "transactions"),
  function(x) {
    ## NOTE: as.character is to support old data which used I()
    rowLabels <- transactionInfo(x)[["transactionID"]]
    if(!is.null(rowLabels)) rowLabels <- as.character(rowLabels)
    
    ## NOTE: as.character is to support old data which used I()
    colLabels <- as.character(itemInfo(x)[["labels"]])
    
    list(rowLabels, colLabels)
  })

setReplaceMethod("dimnames", signature(x = "transactions",
  value = "list"), 
  function(x, value) {
    if(any(dim(x) != sapply(value, length) & !sapply(value, is.null))) 
      stop("length of dimnames does not equal the dimension of the object.")
    
    if(!is.null(value[[1]])){ 
      if(ncol(transactionInfo(x))==0) {
        transactionInfo(x) <- data.frame(transactionID = value[[1]])
      }else{
        transactionInfo(x)[["transactionID"]] <- value[[1]]
      }
    } 
    
    if(!is.null(value[[2]])){ 
      itemInfo(x)[["labels"]] <- value[[2]]
    }
     
    x
  })



# is in itemMatrix
#setMethod("itemLabels", signature(object = "transactions"),
#  function(object) colnames(object))

###
