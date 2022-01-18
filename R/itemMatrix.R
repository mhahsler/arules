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

#' Class itemMatrix --- Sparse Binary Incidence Matrix to Represent Sets of
#' Items
#'
#' The `itemMatrix` class is the basic building block for [transactions],
#' and [associations].  The class contains a sparse
#' Matrix representation of a set of itemsets and the
#' corresponding item labels.
#'
#' **Representation**
#'
#' Sets of itemsets are represented as a compressed sparse
#' binary matrix. Conceptually, columns represent items and rows are the sets/transactions. 
#' In the compressed form, each itemset is a vector of column indices (called item
#' IDs) representing the items. 
#' 
#' **Warning:** Ideally, we would store the matrix as a row-oriented sparse 
#'   matrix (`ngRMatrix`), but the \pkg{Matrix} package provides better support for
#'   column-oriented sparse classes (`ngCMatrix`). The matrix is therefore internally stored
#'   in transposed form.
#' 
#' **Working with several `itemMatrix` objects**
#' 
#' If you work with several `itemMatrix` objects at the same time (e.g.,
#' several transaction sets, lhs and rhs of a rule, etc.), then the encoding
#' (itemLabes and order of the items in the binary matrix) in the different
#' itemMatrices is important and needs to conform. See [itemCoding]
#' to learn how to encode and recode `itemMatrix` objects.
#'
#' @aliases itemMatrix
#' @family itemMatrix and transactions functions
#' 
#' @slot data a sparse matrix of class [ngCMatrix-class] representing the itemsets. 
#'       **Warning:** the matrix is stored in transposed form for efficiency reasons!.
#' @slot itemInfo a data.frame
#' @slot itemsetInfo a data.frame
#' 
#' @param object,x,from the object.
#' @param maxsum integer, how many items should be shown for the summary?
#' @param ... further parameters
#' @param cols columns for the long format.
#' @param decode decode item IDs to item labels.
#' @param value replacement value
#' @param itemSep item separator symbol.
#' @param setStart set start symbol.
#' @param setEnd set end symbol.
#' 
#' @section Objects from the Class: 
#' Objects can be created by calls of the form
#' `new("itemMatrix", ...)`.  However, most of the time objects will be
#' created by coercion from a matrix, list or data.frame.
#' @author Michael Hahsler
#' @keywords classes
#' @examples
#' set.seed(1234)
#'
#' ## Generate a logical matrix with 5000 random itemsets for 20 items
#' m <- matrix(runif(5000 * 20) > 0.8, ncol = 20,
#'             dimnames = list(NULL, paste("item", c(1:20), sep = "")))
#' head(m)
#'
#' ## Coerce the logical matrix into an itemMatrix object
#' imatrix <- as(m, "itemMatrix")
#' imatrix
#'
#' ## An itemMatrix contains a set of itemsets (each row is an itemset).
#' ## The length of the set is the number of rows.
#' length(imatrix)
#'
#' ## The sparese matrix also has regular matrix  dimensions.
#' dim(imatrix)
#' nrow(imatrix)
#' ncol(imatrix)
#'
#' ## Subsetting: Get first 5 elements (rows) of the itemMatrix. This can be done in
#' ## several ways.
#' imatrix[1:5]            ### get elements 1:5
#' imatrix[1:5, ]          ### Matrix subsetting for rows 1:5
#' head(imatrix, n = 5)    ### head()
#'
#' ## Get first 5 elements (rows) of the itemMatrix as list.
#' as(imatrix[1:5], "list")
#'
#' ## Get first 5 elements (rows) of the itemMatrix as matrix.
#' as(imatrix[1:5], "matrix")
#'
#' ## Get first 5 elements (rows) of the itemMatrix as sparse ngCMatrix.
#' ## **Warning:** For efficiency reasons, the ngCMatrix is transposed! You
#' ## can transpose it again to get the expected format.
#' as(imatrix[1:5], "ngCMatrix")
#' t(as(imatrix[1:5], "ngCMatrix"))
#'
#' ## Get labels for the first 5 itemsets (first default and then with
#' ## custom formating)
#' labels(imatrix[1:5])
#' labels(imatrix[1:5], itemSep = " + ", setStart = "", setEnd = "")
#'
#' ## Create itemsets manually from an itemMatrix. Itemsets contain items in the form of
#' ## an itemMatrix and additional quality measures (not supplied in the example).
#' is <- new("itemsets", items = imatrix)
#' is
#' inspect(head(is, n = 3))
#'
#'
#' ## Create rules manually. I use imatrix[4:6] for the lhs of the rules and
#' ## imatrix[1:3] for the rhs. Rhs and lhs cannot share items so I use
#' ## itemSetdiff here. I also assign missing values for the quality measures support
#' ## and confidence.
#' rules <- new("rules",
#'              lhs = itemSetdiff(imatrix[4:6], imatrix[1:3]),
#'              rhs = imatrix[1:3],
#'              quality = data.frame(support = c(NA, NA, NA),
#'                                   confidence =  c(NA, NA, NA)
#'           ))
#' rules
#' inspect(rules)
#'
#' ## Manually create a itemMatrix with an item encoding that matches imatrix (20 items in order
#' ## item1, item2, ..., item20)
#' itemset_list <- list(c("item1","item2"),
#'                      c("item3"))
#'
#' imatrix_new <- encode(itemset_list, itemLabels = imatrix)
#' imatrix_new
#' compatible(imatrix_new, imatrix)
#'
#' @aliases initialize,itemMatrix-method show,itemMatrix-method
setClass(
  "itemMatrix",
  representation(
    data        = "ngCMatrix",
    itemInfo    = "data.frame",
    itemsetInfo = "data.frame"
  ),
  
  prototype(itemInfo = data.frame(), itemsetInfo = data.frame()),
  
  validity = function(object) {
    ## itemInfo needs a labels column of appropriate length
    ## also the labels must be unique for matching objects.
    if (length(itemInfo(object)[["labels"]]) != nitems(object))
      return("item labels do not match number of columns")
    if (length(unique(itemInfo(object)[["labels"]])) != nitems(object))
      return("item labels not unique")
    
    if (ncol(itemsetInfo(object)) &&
        nrow(itemsetInfo(object)) != length(object))
      return("itemsetInfo does not match number of rows")
    
    TRUE
  }
)

setMethod(initialize, "itemMatrix", function(.Object, ...) {
  .Object <- callNextMethod()
  
  validObject(.Object)
  .Object
})

setMethod("show", signature(object = "itemMatrix"),
  function(object) {
    cat(
      "itemMatrix in sparse format with\n",
      length(object),
      "rows (elements/transactions) and\n",
      nitems(object),
      "columns (items)\n"
    )
    invisible(NULL)
  })


setClass(
  "summary.itemMatrix",
  representation(
    Dim           = "integer",
    density       = "numeric",
    itemSummary   = "integer",
    lengths       = "table",
    lengthSummary = "table",
    itemInfo      = "data.frame",
    itemsetInfo      = "data.frame"
  )
)

# setGeneric("summary") # is generic in Matrix

#' @describeIn itemMatrix-class show a summary.
#' @aliases summary.itemMatrix-class show,summary.itemMatrix-method
setMethod("summary", signature(object = "itemMatrix"),
  function(object, maxsum = 6, ...) {
    ifs   <-
      sort(itemFrequency(object, type = "abs"), decreasing = TRUE)
    isum  <- head(ifs, maxsum - 1L)
    isum  <- c(isum, "(Other)" = sum(ifs) - sum(isum))
    sizes <- size(object)
    
    new(
      "summary.itemMatrix",
      Dim           = dim(object),
      density       = .density_Matrix(as(object, "ngCMatrix")),
      itemSummary   = isum,
      lengths       = table(sizes),
      lengthSummary = summary(sizes),
      itemInfo      = head(itemInfo(object), 3)
    )
  })

setMethod("show", signature(object = "summary.itemMatrix"),
  function(object) {
    cat(
      "itemMatrix in sparse format with\n",
      object@Dim[1],
      "rows (elements/itemsets/transactions) and\n",
      object@Dim[2],
      "columns (items) and a density of",
      object@density,
      "\n"
    )
    
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
  })


#' @describeIn itemMatrix-class returns the number of rows 
#' (itemsets) and columns (items in the encoding).
setMethod("dim", signature(x = "itemMatrix"),
  function(x)
    rev(dim(x@data)))


#' @rdname itemMatrix-class
setGeneric("nitems",
  function(x, ...)
    standardGeneric("nitems"))

#' @describeIn itemMatrix-class returns the number of items in the encoding.
#' @aliases nitems
setMethod("nitems", signature(x = "itemMatrix"),
  function(x)
    ncol(x))

#' @describeIn itemMatrix-class returns the number of itemsets (rows) in the matrix.
setMethod("length", signature(x = "itemMatrix"),
  function(x)
    nrow(x))


#' @rdname itemMatrix-class
setGeneric("toLongFormat",
  function(from, ...) standardGeneric("toLongFormat"))

#' @describeIn itemMatrix-class convert the sets to long format 
#' (a data.frame with two columns, ID and item). Column names 
#' can be specified as a character vector of length 2 called `cols`.
#' @aliases toLongFormat
setMethod("toLongFormat", signature(from = "itemMatrix"),
  function(from,
    cols = c("ID", "item"),
    decode = TRUE) {
    from_list <- LIST(from, decode = FALSE)
    tids <-
      unlist(lapply(
        seq(length(from_list)),
        FUN = function(i)
          rep(i, times = length(from_list[[i]]))
      ))
    items <- unlist(from_list)
    
    if (decode)
      items <- factor(items, labels = itemLabels(from))
    
    df <- data.frame(tids, items)
    
    colnames(df) <- cols
    df
  })


#' @describeIn itemMatrix-class returns labels for the itemsets. 
#' The following arguments can be used to customize the representation 
#' of the labels: `itemSep`, `setStart` and `setEnd`.
setMethod("labels", signature(object = "itemMatrix"),
  function(object,
    itemSep = ",",
    setStart = "{",
    setEnd = "}")
    paste(setStart, sapply(as(object, "list"), paste,
      collapse = itemSep), setEnd, sep = ""))

#' @rdname itemMatrix-class
setGeneric("itemLabels",
  function(object, ...)
    standardGeneric("itemLabels"))

#' @rdname itemMatrix-class
setGeneric("itemLabels<-",
  function(object, value)
    standardGeneric("itemLabels<-"))

#' @describeIn itemMatrix-class returns the item labels used for encoding as a character vector.
#' @aliases itemLabels
setMethod("itemLabels", signature(object = "itemMatrix"),
  function(object)
    colnames(object))

#' @describeIn itemMatrix-class replaces the item labels used for encoding.
setReplaceMethod("itemLabels", signature(object = "itemMatrix"),
  function(object, value) {
    colnames(object) <- value
    object
  })


#' @rdname itemMatrix-class
setGeneric("itemInfo",
  function(object) standardGeneric("itemInfo"))

#' @rdname itemMatrix-class
setGeneric("itemInfo<-",
  function(object, value) standardGeneric("itemInfo<-"))

#' @describeIn itemMatrix-class returns the whole item/column information data.frame including labels.
#' @aliases itemInfo
setMethod("itemInfo", signature(object = "itemMatrix"),
  function(object)
    object@itemInfo)

#' @describeIn itemMatrix-class replaces the item/column info by a data.frame.
#' @aliases itemInfo<-
setReplaceMethod("itemInfo", signature(object = "itemMatrix"),
  function(object, value) {
    object@itemInfo <- value
    validObject(object)
    object
  })


#' @rdname itemMatrix-class
setGeneric("itemsetInfo",
  function(object) standardGeneric("itemsetInfo"))

#' @rdname itemMatrix-class
setGeneric("itemsetInfo<-",
  function(object, value) standardGeneric("itemsetInfo<-"))

#' @describeIn itemMatrix-class returns the item set/row information data.frame.
#' @aliases itemsetInfo
setMethod("itemsetInfo", signature(object = "itemMatrix"),
  function(object)
    object@itemsetInfo)

#' @describeIn itemMatrix-class replaces the item set/row info by a data.frame.
#' @aliases itemsetInfo<-
setReplaceMethod("itemsetInfo", signature(object = "itemMatrix"),
  function(object, value) {
    object@itemsetInfo <- value
    validObject(object)
    object
  })


#' @rdname itemMatrix-class
#' @name coercion-itemMatrix
#' @aliases 
#' coerce,matrix,itemMatrix-method
#' coerce,itemMatrix,matrix-method
#' coerce,list,itemMatrix-method
#' coerce,itemMatrix,list-method
#' coerce,itemMatrix,ngCMatrix-method
#' coerce,ngCMatrix,itemMatrix-method
#' 
#' @section Coercions:
#' 
#' * `as("matrix", "itemMatrix")`
#' * `as("itemMatrix", "matrix")`
#' * `as("list", "itemMatrix")`
#' * `as("itemMatrix", "list")`
#' * `as("itemMatrix", "ngCMatrix")`
#' * `as("ngCMatrix", "itemMatrix")`
#' 
#' **Warning:** the `ngCMatrix` representation is transposed! 
NULL

setAs("matrix", "itemMatrix",
  function(from) {
    ## Matrix requires logical. however, it should be
    ## the responsibility of mode to avoid unnecessary
    ## coercions.
    if (mode(from) != "logical") {
      ## check if it is a 0-1 matrix
      if (mode(from) != "numeric")
        stop("matrix is not logical or a numeric 0-1 matrix!")
      if (any(from != 0 &
          from != 1))
        warning("matrix contains values other than 0 and 1! Setting all entries != 0 to 1.")
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
    
    new(
      "itemMatrix",
      data        = t(as(from, "ngCMatrix")),
      itemInfo    = itemInfo,
      itemsetInfo = itemsetInfo
    )
  })

setAs("itemMatrix", "matrix",
  function(from) {
    to <- as(t(from@data), "matrix")
    
    dimnames(to) <- dimnames(from)
    to
  })

#' @describeIn itemMatrix-class returns a list with the dimname vectors.
setMethod("dimnames", signature(x = "itemMatrix"),
  function(x) {
    ## NOTE: as.character is to support old data which used I()
    rowLabels <- itemsetInfo(x)[["itemsetID"]]
    if (!is.null(rowLabels))
      rowLabels <- as.character(rowLabels)
    
    ## NOTE: as.character is to support old data which used I()
    colLabels <- as.character(itemInfo(x)[["labels"]])
    
    list(rowLabels, colLabels)
  })

#' @describeIn itemMatrix-class replace the dimnames.
setReplaceMethod("dimnames", signature(x = "itemMatrix",
  value = "list"),
  function(x, value) {
    if (any(dim(x) != sapply(value, length) & !sapply(value, is.null)))
      stop("length of dimnames does not equal the dimension of the object.")
    
    if (!is.null(value[[1]])) {
      if (ncol(itemsetInfo(x)) == 0) {
        itemsetInfo(x) <- data.frame(itemsetID = value[[1]])
      } else{
        itemsetInfo(x)[["itemsetID"]] <- value[[1]]
      }
    }
    
    if (!is.null(value[[2]])) {
      itemInfo(x)[["labels"]] <- value[[2]]
    }
    
    x
  })


setAs("itemMatrix", "list",
  function(from)
    LIST(from, decode = TRUE))

setAs("list", "itemMatrix",
  function(from) {
    if (!length(from))
      return(new("itemMatrix"))
    
    ## some checks
    if (!all(sapply(from, is.atomic)))
      stop("can coerce list with atomic components only")
    if (any(dup <- sapply(from, anyDuplicated))) {
      warning("removing duplicated items in transactions")
      for (i in which(dup > 0))
        from[[i]] <- unique(from[[i]])
    }
    
    ## fix for Matrix (ceeboo 2009)
    from <- lapply(from, sort)
    p <- sapply(from, length)
    names(p) <- NULL
    p <- cumsum(p)
    i <- unlist(from, use.names = FALSE)
    i <- factor(i)
    
    p <- new(
      "ngCMatrix",
      p   = c(0L, p),
      i   = as.integer(i) - 1L,
      Dim = c(length(levels(i)), length(p))
    )
    
    
    new(
      "itemMatrix",
      data        = p,
      itemInfo    = data.frame(labels    = levels(i),
        stringsAsFactors = FALSE),
      itemsetInfo = data.frame(itemsetID = labels(from),
        stringsAsFactors = FALSE)
    )
  })

setAs("itemMatrix", "ngCMatrix",
  function(from) {
    dimnames(from@data) <- rev(dimnames(from))
    from@data
  })

setAs("ngCMatrix", "itemMatrix",
  function(from) {
    itemInfo <- data.frame(labels = labels(from)[[1]],
      stringsAsFactors = FALSE) ## rownames
    itemsetInfo <- data.frame(itemsetID = labels(from)[[2]],
      stringsAsFactors = FALSE) ## colnames
    
    dimnames(from) <- list(NULL, NULL)
    
    new(
      "itemMatrix",
      data = from,
      itemInfo = itemInfo,
      itemsetInfo = itemsetInfo
    )
  })

