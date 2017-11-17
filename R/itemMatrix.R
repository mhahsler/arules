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
## Class itemMatrix
##
## Basic class for sparse representation of sets or collections
## of itemsets


## dimensions of the binary matrix
setMethod("dim", signature(x = "itemMatrix"),
  function(x) rev(dim(x@data)))

## with dim nrow and ncol should work too

setMethod("nitems", signature(x = "itemMatrix"),
  function(x) ncol(x))

setMethod("length", signature(x = "itemMatrix"),
  function(x) nrow(x))

## produces a vector of element sizes
setMethod("size", signature(x = "itemMatrix"),
  function(x) {
    ## if Matrix had colSums implemented efficiently,
    ## we could use colSums(x@data). we use our own C code.
    ## diff(x@data@p) is nearly as fast as colSums(x@data).
    
    ## FIXME: Add transactionID or itemsetID as names
    cnt <- .Call(R_colSums_ngCMatrix, x@data)
    cnt
  }
)

##*******************************************************
## Coercions

setAs("matrix", "itemMatrix",
  function(from) {
    ## Matrix requires logical. however, it should be
    ## the responsibility of mode to avoid uneccessary
    ## coercions.
    if (mode(from) != "logical") {
      ## check if it is a 0-1 matrix
      if(mode(from) != "numeric") stop("matrix is not logical or a numeric 0-1 matrix!")
      if(any(from != 0 & from != 1)) warning("matrix contains values other than 0 and 1! Setting all entries != 0 to 1.")
      mode(from) <- "logical"
    }
      
    ## we have to transpose since there is currently no
    ## support for "ngRMatrix" in Matrix. note that we
    ## can fail later as the row or column names need 
    ## not neccessarily be unique.
    itemInfo <- data.frame(labels = labels(from)[[2]], 
      stringsAsFactors = FALSE) ## colnames
    itemsetInfo <- data.frame(itemsetID = labels(from)[[1]], 
      stringsAsFactors = FALSE) ## rownames
    
    dimnames(from) <- NULL
    
    new("itemMatrix", 
      data        = t(as(from, "ngCMatrix")),
      itemInfo    = itemInfo,
      itemsetInfo = itemsetInfo)
  }
)

setAs("itemMatrix", "matrix",
  function(from) {
    to <- as(t(from@data), "matrix")
    
    dimnames(to) <- dimnames(from)
    to
  }
)

setMethod("dimnames", signature(x = "itemMatrix"),
  function(x) {
    
    ## NOTE: as.character is to support old data which used I()
    rowLabels <- itemsetInfo(x)[["itemsetID"]]
    if(!is.null(rowLabels)) rowLabels <- as.character(rowLabels)
    
    ## NOTE: as.character is to support old data which used I()
    colLabels <- as.character(itemInfo(x)[["labels"]])
    
    list(rowLabels, colLabels)
  })

setReplaceMethod("dimnames", signature(x = "itemMatrix",
  value = "list"), 
  function(x, value) {
    if(any(dim(x) != sapply(value, length) & !sapply(value, is.null))) 
      stop("length of dimnames does not equal the dimension of the object.")
    
    if(!is.null(value[[1]])){ 
      if(ncol(itemsetInfo(x))==0) {
        itemsetInfo(x) <- data.frame(itemsetID = value[[1]])
      }else{
        itemsetInfo(x)[["itemsetID"]] <- value[[1]]
      }
    }
    
    if(!is.null(value[[2]])) {
      itemInfo(x)[["labels"]] <- value[[2]]
    }
    
    x
  })

setAs("itemMatrix", "list",
  function(from) LIST(from, decode = TRUE)
)

setMethod("LIST", signature(from = "itemMatrix"),
  function(from, decode = TRUE) {
    if (decode) {
      to <- .Call(R_asList_ngCMatrix, from@data, itemLabels(from))
      names(to) <- itemsetInfo(from)[["itemsetID"]]
      to
    } else
      .Call(R_asList_ngCMatrix, from@data, NULL)
  }
)


setAs("list", "itemMatrix", 
  function(from) {
    if (!length(from))
      return(new("itemMatrix"))
    
    ## some checks
    if (!all(sapply(from, is.atomic)))
      stop("can coerce list with atomic components only")
    if (any(dup <- sapply(from, anyDuplicated))) {
      warning("removing duplicated items in transactions")
      for(i in which(dup>0)) from[[i]] <- unique(from[[i]])
    }
    
    ## fix for Matrix (ceeboo 2009)
    from <- lapply(from, sort)
    p <- sapply(from, length)
    names(p) <- NULL
    p <- cumsum(p)
    i <- unlist(from, use.names = FALSE)
    i <- factor(i)
    
    p <- new("ngCMatrix", p   = c(0L, p),
      i   = c(i) - 1L,
      Dim = c(length(levels(i)), length(p)))
    
    
    new("itemMatrix", 
      data        = p,
      itemInfo    = data.frame(labels    = levels(i), 
        stringsAsFactors = FALSE),
      itemsetInfo = data.frame(itemsetID = labels(from), 
        stringsAsFactors = FALSE))
  }
)

setAs("itemMatrix", "ngCMatrix",
  function(from) {
    dimnames(from@data) <- rev(dimnames(from)) 
    from@data
  }
)

setAs("ngCMatrix", "itemMatrix",
  function(from) {
    itemInfo <- data.frame(labels = labels(from)[[1]],
      stringsAsFactors = FALSE) ## rownames
    itemsetInfo <- data.frame(itemsetID = labels(from)[[2]],
      stringsAsFactors = FALSE) ## colnames
    
    dimnames(from) <- list(NULL, NULL)
    
    new("itemMatrix", data=from, 
      itemInfo=itemInfo,
      itemsetInfo=itemsetInfo)
  }
)



##*******************************************************
## find elements which contain some items (as labels or 
## in itemInfo) note this is not what we would expect for
## %in% in R! but match below works the R-way

setMethod("%in%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    pos <- match(table, itemLabels(x))
    if (any(is.na(pos)))
      stop("table contains an unknown item label" )
    size(x[, pos]) > 0
  }
)

setMethod("%in%", signature(x = "itemMatrix", table = "itemMatrix"),
  function(x, table) x %in% unlist(as(table, "list"))
)

## all items have to be in
setMethod("%ain%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    pos <- match(table, itemLabels(x))
    if (any(is.na(pos))) 
      stop("table contains an unknown item label" )
    size(x[, pos]) == length(pos)
  }
)

## only items can to be in
setMethod("%oin%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    pos <- match(table, itemLabels(x))
    if (any(is.na(pos))) 
      stop("table contains an unknown item label" )
    size(x[, -pos]) == 0
  }
)

## partial in  
setMethod("%pin%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    if (length(table) > 1) {
      warning("table contains more than one item label pattern and only the first element will be used")
      table <- table[1]
    }
      
    if (table[1] == "")
      stop("table contains an illegal pattern (empty string)")
    
    pos <- grep(table, itemLabels(x))
    
    if (is.na(pos[1])) 
      return(rep(FALSE, length(x)))
    
    size(x[, pos]) > 0
  }
)

##*******************************************************
## subset, combine, duplicated, unique 

## remember that the sparse matrix is stored in transposed
## form (i and j are reversed). dimnames are handled by the
## C code but these should be list(NULL, NULL). extracting
## from data.frame() results in changing the dimension of 
## the data frame and must therfore be prevented. taking a
## row subset of a column sparse matrix is more costly than
## taking a column subset. for strict subsets it is more
## efficient to do the latter first.


setMethod("[", signature(x = "itemMatrix", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop) {
    
    ## i and j are reversed internally!
    if (!missing(i)) {
      
      ## recycling?
      if(is.logical(i) && length(i) != ncol(x)) i <- rep(i, length.out = nrow(x))
      
      ## deal with NAs we do not take NAs
      if(any(is.na(i))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if(is.logical(i)) i[is.na(i)] <- FALSE
        else i <- i[!is.na(i)]
      }
      
      i <- .translate_index(i, rownames(x), nrow(x))
      ## faster than: x@data <- x@data[,i, drop=FALSE]
      x@data <- .Call(R_colSubset_ngCMatrix, x@data, i)
      
      ### only subset if we have rows
      if(nrow(x@itemsetInfo)) x@itemsetInfo <- x@itemsetInfo[i,, drop = FALSE]
    }
    
    if (!missing(j)) {
      
      ## recycling?
      if(is.logical(j) && length(j) != ncol(x)) j <- rep(j, length.out = ncol(x))
      
      ## deal with NAs we do not take NAs
      if(any(is.na(j))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if(is.logical(j)) j[is.na(j)] <- FALSE
        else j <- j[!is.na(j)]
      }
      
      j <- .translate_index(j, colnames(x), ncol(x))
      ## faster than: x@data <- x@data[j,, drop=FALSE]
      x@data <- .Call(R_rowSubset_ngCMatrix, x@data, j)
        
      x@itemInfo <- x@itemInfo[j,, drop = FALSE]
    }
    
    ## makes sure that items are still unique
    validObject(x, complete = TRUE)
    x
  })

### this is rbind
### FIXME: labels are not sorted
setMethod("c", signature(x = "itemMatrix"),
  function(x, ..., recursive = FALSE) {
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "itemMatrix"))
        stop("can only combine itemMatrix")
      x@itemsetInfo <- .combineMeta(x, y, "itemsetInfo")
      k <- match(itemLabels(y), itemLabels(x))
      n <- which(is.na(k))
      if (length(n)) {
        k[n] <- x@data@Dim[1] + seq(length(n))
        x@data@Dim[1] <- x@data@Dim[1] + length(n)
        x@itemInfo <- rbind(x@itemInfo, 
          y@itemInfo[n,, drop = FALSE])
      }
      if (any(k != seq_len(length(k))))
        y@data <- .Call(R_recode_ngCMatrix, y@data, k)
      if (y@data@Dim[1] <  x@data@Dim[1])
        y@data@Dim[1] <- x@data@Dim[1]
      
      ## this is faste than x@data <- cbind(x@data, y@data)
      x@data <- .Call(R_cbind_ngCMatrix, x@data, y@data)
    }
    validObject(x, complete = TRUE)
    x
  }
)

### merge items (this is cbind)
setMethod("merge", signature(x="itemMatrix"),
  function(x, y, ...) {
    y <- as(y, "itemMatrix")
    if(nrow(x)!=nrow(y)) stop("The number of rows in x and y do not conform!")
    
    ## this is faster than dc <- rbind(x@data, y@data) 
    dc <- t(.Call(R_cbind_ngCMatrix, t(x@data), t(y@data)))
    
    ## fix itemInfo
    iix <- itemInfo(x)
    iiy <- itemInfo(y)
    names <- unique(union(colnames(iix), colnames(iiy)))
    for(n in names) {
      if(is.null(iix[[n]])) iix[[n]] <- NA_character_
      if(is.null(iiy[[n]])) iiy[[n]] <- NA_character_
    }
    
    ii <- rbind(iix, iiy)
    
    new("itemMatrix",
      data        = dc,
      itemInfo    = ii,
      itemsetInfo = itemsetInfo(x)
    )
  })

setMethod("duplicated", signature(x = "itemMatrix"),
  function(x, incomparables = FALSE) {
    ## use a prefix tree
    i <- .Call(R_pnindex, x@data, NULL, FALSE)
    duplicated(i)
  }
)

setMethod("unique", signature(x = "itemMatrix"),
  function(x,  incomparables = FALSE)
    x[!duplicated(x, incomparables = incomparables)])

## checks if the item labels conform
## and uses more efficient prefix tree C code
setMethod("match", signature(x = "itemMatrix", table = "itemMatrix"),
  function(x, table, nomatch = NA_integer_, incomparables = NULL) {
    k <- match(itemLabels(x), itemLabels(table))
    n <- which(is.na(k))
    if (length(n)) {
      k[n] <- table@data@Dim[1] + seq(length(n))
      table@data@Dim[1] <- table@data@Dim[1] + length(n)
    }
    if (any(k != seq_len(length(k))))
      x@data <- .Call(R_recode_ngCMatrix, x@data, k)
    if (x@data@Dim[1] <  table@data@Dim[1])
      x@data@Dim[1] <- table@data@Dim[1]
    i <- .Call(R_pnindex, table@data, x@data, FALSE)
    match(i, seq_len(length(table)), nomatch = nomatch, 
      incomparables = incomparables)
  }
)

##*******************************************************
## accessors

setMethod("labels", signature(object = "itemMatrix"),
  function(object, itemSep = ",", setStart = "{", setEnd = "}")
    paste(setStart, sapply(as(object, "list"), paste,
      collapse = itemSep), setEnd, sep = ""))

setMethod("itemLabels", signature(object = "itemMatrix"),
  function(object) colnames(object))

setReplaceMethod("itemLabels", signature(object = "itemMatrix"),
  function(object, value) {
    colnames(object) <- value
    object
  }
)

setMethod("itemInfo", signature(object = "itemMatrix"),
  function(object) object@itemInfo)

setReplaceMethod("itemInfo", signature(object = "itemMatrix"),
  function(object, value) {
    object@itemInfo <- value
    validObject(object)
    object
  }
)

setMethod("itemsetInfo", signature(object = "itemMatrix"),
  function(object) object@itemsetInfo)

setReplaceMethod("itemsetInfo", signature(object = "itemMatrix"),
  function(object, value) {
    object@itemsetInfo <- value
    validObject(object)
    object
  }
)

##*******************************************************
## show/summary + plots

setMethod("show", signature(object = "itemMatrix"),
  function(object) {
    cat("itemMatrix in sparse format with\n", 
      length(object), "rows (elements/transactions) and\n", 
      nitems(object), "columns (items)\n")
    invisible(NULL)
  }
)

setMethod("summary", signature(object = "itemMatrix"),
  function(object, maxsum = 6, ...) {
    ifs   <- sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
    isum  <- head(ifs, maxsum - 1L)
    isum  <- c(isum, "(Other)" = sum(ifs) - sum(isum))
    sizes <- size(object)
    
    new("summary.itemMatrix", 
      Dim           = dim(object),
      density       = .density_Matrix(as(object, "ngCMatrix")),
      itemSummary   = isum,
      lengths       = table(sizes),
      lengthSummary = summary(sizes),
      itemInfo      = head(itemInfo(object), 3)
    )
  }
)

setMethod("show", signature(object = "summary.itemMatrix"),
  function(object) {
    cat("itemMatrix in sparse format with\n",
      object@Dim[1], "rows (elements/itemsets/transactions) and\n",
      object@Dim[2], "columns (items) and a density of",
      object@density, "\n")
    
    cat("\nmost frequent items:\n")
    print(object@itemSummary)
    cat("\nelement (itemset/transaction) length distribution:\n")
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

###
