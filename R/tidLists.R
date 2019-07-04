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
## Class tidLists
##
## transaction ID lists


##*********************************************************
## dimensions of the binary matrix
setMethod("dim", signature(x = "tidLists"),
  function(x) rev(dim(x@data)))

setMethod("dimnames", signature(x = "tidLists"),
  function(x){
    colLabels <- x@transactionInfo[["transactionID"]]
    if(!is.null(colLabels)) colLabels <- as.character(colLabels)
    
    rowLabels <- x@itemInfo[["labels"]]
    
    list(rowLabels, colLabels)
  })

setReplaceMethod("dimnames", signature(x = "tidLists",
  value = "list"), 
  function(x, value) {
    if(any(dim(x) != sapply(value, length) & !sapply(value, is.null))) 
      stop("length of dimnames does not equal the dimension of the object.")
    
    if(!is.null(value[[1]])){ 
      x@itemInfo[["labels"]] <- value[[1]]
    } 
    
    if(!is.null(value[[2]])){ 
      if(ncol(x@transactionInfo)==0) {
        x@transactionInfo <- data.frame(transactionID = value[[2]])
      }else{
        x@transactionInfo[["transactionID"]] <- value[[2]]
      }
    }
    
    x
  })


## number of elements (rows)
setMethod("length", signature(x = "tidLists"),
  function(x) dim(x)[1])

## produces a vector of element sizes
setMethod("size", signature(x = "tidLists"),
  function(x) .Call("R_colSums_ngCMatrix", x@data, PACKAGE = "arules"))

##*******************************************************
## show/summary

setMethod("show", signature(object = "tidLists"),
  function(object) {
    cat("tidLists in sparse format with\n",
      nrow(object), "items/itemsets (rows) and\n",
      ncol(object), "transactions (columns)\n")
    invisible(NULL)
  }
)

setMethod("summary", signature(object = "tidLists"),
  function(object, maxsum = 6, ...) {
    tfs   <- sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
    tsum  <- head(tfs, maxsum - as.integer(1))
    tsum  <- c(tsum, "(Other)" = sum(tfs) - sum(tsum))
    sizes <- size(object)
    
    new("summary.tidLists", 
      Dim                = dim(object),
      transactionSummary = tsum,
      lengths            = table(sizes),
      lengthSummary      = summary(sizes),
      itemInfo           = head(itemInfo(object), 3)
    )
  }
)

setMethod("show", signature(object = "summary.tidLists"),
  function(object) {
    cat("tidLists in sparse format with\n",
      object@Dim[1], "items/itemsets (rows) and\n",
      object@Dim[2], "transactions (columns)\n")
    
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
  }
)


## no t for associations
setMethod("t", signature(x = "tidLists"),
  function(x) stop("Object not transposable! Use as(x, \"transactions\") for coercion."))

##*****************************************************
## combine
## FIXME: Does not check for duplicated itemsets...

setMethod("c", signature(x = "tidLists"),
  function(x, ..., recursive = FALSE) {
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    
    dat <- x@data
    itemI <- itemInfo(x)
    for (y in args) {
      if (!is(y, "tidLists")) stop("can only combine tidLists.")
      
      if(ncol(x) != ncol(y)) stop("transactions not conforming.")
      
      dat <- .Call("R_cbind_ngCMatrix", dat, y@data, PACKAGE = "arules")
      itemI <- rbind(itemI, itemInfo(y))
    }
    
    x@data <- dat 
    x@itemInfo <- itemI
    x
  })

##*****************************************************
## subset

setMethod("[", signature(x = "tidLists", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop) {
    
    ## i and j are reversed internally!
    if (!missing(i)) {
      if(any(is.na(i))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if(is.logical(i)) i[is.na(i)] <- FALSE
        else i <- i[!is.na(i)]
      } 
      
      i <- .translate_index(i, rownames(x), nrow(x)) 
      x@data <- .Call("R_colSubset_ngCMatrix", x@data, i, PACKAGE = "arules")
      
      x@itemInfo <- x@itemInfo[i,, drop = FALSE]
    }
    
    if (!missing(j)) {
      if(any(is.na(j))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if(is.logical(j)) j[is.na(j)] <- FALSE
        else j <- j[!is.na(j)]
      } 
      
      j <- .translate_index(j, colnames(x), ncol(x))
      x@data <- .Call("R_rowSubset_ngCMatrix", x@data, j, PACKAGE = "arules")
      
      x@transactionInfo <- x@transactionInfo[j,, drop = FALSE]
    }
    
    validObject(x, complete = TRUE)
    x
  }
)

##*****************************************************
## coercions 

setAs("tidLists", "list",
  function(from) LIST(from, decode = TRUE))

setMethod("LIST", signature(from = "tidLists"),
  function(from, decode = TRUE) {
    if (decode) {
      i <- from@transactionInfo[["transactionID"]]
      if (!is.null(i))
        i <- as.character(i)
      to <- .Call("R_asList_ngCMatrix", from@data, i, PACKAGE = "arules")
      names(to) <- from@itemInfo[["labels"]]
      to
    } else
      .Call("R_asList_ngCMatrix", from@data, NULL, PACKAGE = "arules")
  }
)

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
    i <- new("ngCMatrix", p   = c(0L, p),
      i   = i - 1L, 
      Dim = c(length(l), length(p)))
    new("tidLists", 
      data            = i,
      itemInfo        = data.frame(labels = names(from), 
        stringsAsFactors = FALSE),
      transactionInfo = data.frame(labels = l, stringsAsFactors = FALSE)
    )
  }
)

setAs("tidLists", "matrix",
  function(from) {
    to <- as(t(from@data), "matrix")
    dimnames(to) <- dimnames(from) 
    to
  }
)

setAs("tidLists", "ngCMatrix",
  function(from) {
    dimnames(from@data) <- rev(dimnames(from))
    from@data
  }
)

setAs("tidLists", "transactions",
  function(from) 
    new("transactions", 
      data            = t(from@data), 
      itemInfo        = from@itemInfo, 
      itemsetInfo = from@transactionInfo
    ))

setAs("transactions", "tidLists",
  function(from) 
    new("tidLists", 
      data            = t(from@data), 
      itemInfo        = from@itemInfo, 
      transactionInfo = transactionInfo(from)))

##
setAs("tidLists", "itemMatrix",
  function(from) {
    k <- match("transactionID", names(from@transactionInfo))
    if (!is.na(k))
      names(from@transactionInfo)[k] <- "itemsetID"
    
    new("itemMatrix", data        = t(from@data), 
      itemInfo    = from@itemInfo,
      itemsetInfo = from@transactionInfo)
  }
)

setAs("itemMatrix", "tidLists",
  function(from) {
    k <- match("itemsetID", names(from@itemsetInfo))
    if (!is.na(k))
      names(from@itemsetInfo)[k] <- "transactionID"
    
    new("tidLists", data            = t(from@data),
      itemInfo        = from@itemInfo,
      transactionInfo = from@itemsetInfo)
  }
)

##*****************************************************
## accessors

## TODO: Add replacement functions
setMethod("transactionInfo", signature(x = "tidLists"),
  function(x) x@transactionInfo)

setReplaceMethod("transactionInfo", signature(x = "tidLists"),
  function(x, value) {
    x@transactionInfo <- value
    validObject(x)
    x
  }
)


setMethod("itemInfo", signature(object = "tidLists"),
  function(object) object@itemInfo)

setReplaceMethod("itemInfo", signature(object = "tidLists"),
  function(object, value) {
    object@itemInfo <- value
    validObject(object)
    object
  })

setMethod("itemLabels", signature(object = "tidLists"),
  function(object) rownames(object))

setMethod("labels", signature(object = "tidLists"),
  function(object) {
    sapply(as(object, "list"),
      function(x) paste("{", paste(x, collapse = ","), "}", sep=""))
  }
)

###
