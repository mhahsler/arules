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


#' Item Coding --- Conversion between Item Labels and Column IDs
#'
#' The order in which items are stored in an [itemMatrix] is called the
#' _item coding_. The following generic functions and methods are used
#' to translate between the representation in the itemMatrix format
#' (used in transactions, rules and itemsets), item labels and numeric item IDs
#' (i.e., the column numbers in the itemMatrix representation).
#'
#' **Item coding compatibility:** When working with several datasets or different
#' subsets of the same dataset, combining or compare the found
#' itemsets or rules requires a compatible item coding.  
#' That is, the sparse matrices representing the
#' items (the itemMatrix objects) have columns for the same items in exactly the 
#' same order. The coercion to transactions with `transactions()` or 
#' `as(x, "transactions")` will create the
#' item coding by adding items in the order they are encountered in the dataset. This
#' can lead to different item codings (different order, missing items) for even
#' only slightly different datasets or versions of a dataset. 
#' Method `compatible()` can be used to check if two sets have the same item coding.
#'
#' **Defining a common item coding:** 
#' When working with many sets, then first a common item
#' coding should be defined by creating a vector with all possible item labels and then 
#' specify them as `itemLabels` to create transactions with `transactions()`. 
#' Compatible [itemMatrix] objects can be created using `encode()`.
#' 
#' **Recoding and Decoding:**
#' Two incompatible objects can be made compatible using `recode()`. Recode
#' one object by specifying the other object in `itemLabels`.
#'
#' `decode()` converts from the column IDs used in the itemMatrix
#' representation to item labels. `decode()` is used by [LIST()].
#'
#' @name itemCoding
#' @aliases itemCoding itemcoding
#' @family itemMatrix functions
#' @family preprocessing
#' 
#' @param x a vector or a list of vectors of character strings (for
#' `encode()` or of numeric (for `decode()`), or an object of class
#' [itemMatrix] (for `recode()`).
#' @param itemLabels a vector of character strings used for coding where the
#' position of an item label in the vector gives the item's column ID.
#' Alternatively, a [itemMatrix], [transactions] or
#' [associations] object can be specified and the item labels or these
#' objects are used.
#' @param itemMatrix return an object of class [itemMatrix] otherwise an
#' object of the same class as `x` is returned.
#' @param y an object of class [itemMatrix], [transactions] or
#' [associations] to compare item coding to `x`.
#' @param match deprecated: used `itemLabels` instead.
#' @param ... further arguments.
#' 
#' @return `recode()` always returns an object of the same class as `x`.
#'
#' For `encode()` with `itemMatrix = TRUE` an object of class
#' [itemMatrix] is returned.  Otherwise the result is of the same type as
#' `x`, e.g., a list or a vector.
#' @author Michael Hahsler
#' @seealso [LIST()], [associations], [itemMatrix]
#' @keywords manip
#' @examples
#' data("Adult")
#'
#' ## Example 1: Manual decoding
#' ## Extract the item coding as a vector of item labels.
#' iLabels <- itemLabels(Adult)
#' head(iLabels)
#'
#' ## get undecoded list (itemIDs)
#' list <- LIST(Adult[1:5], decode = FALSE)
#' list
#'
#' ## decode itemIDs by replacing them with the appropriate item label
#' decode(list, itemLabels = iLabels)
#'
#'
#' ## Example 2: Manually create an itemMatrix using iLabels as the common item coding
#' data <- list(
#'     c("income=small", "age=Young"),
#'     c("income=large", "age=Middle-aged")
#'     )
#'
#' # Option a: encode to match the item coding in Adult
#' iM <- encode(data, itemLabels = Adult)
#' iM
#' inspect(iM)
#' compatible(iM, Adult)
#'
#' # Option b: coercion plus recode to make it compatible to Adult
#' #           (note: the coding has 115 item columns after recode)
#' iM <- as(data, "itemMatrix")
#' iM
#' compatible(iM, Adult)
#'
#' iM <- recode(iM, itemLabels = Adult)
#' iM
#' compatible(iM, Adult)
#'
#'
#' ## Example 3: use recode to make itemMatrices compatible
#' ## select first 100 transactions and all education-related items
#' sub <- Adult[1:100, itemInfo(Adult)$variables ==  "education"]
#' itemLabels(sub)
#' image(sub)
#'
#' ## After choosing only a subset of items (columns), the item coding is now
#' ## no longer compatible with the Adult dataset
#' compatible(sub, Adult)
#'
#' ## recode to match Adult again
#' sub.recoded <- recode(sub, itemLabels = Adult)
#' image(sub.recoded)
#'
#'
#' ## Example 4: manually create 2 new transaction for the Adult data set
#' ##            Note: check itemLabels(Adult) to see the available labels for items
#' twoTransactions <- as(
#'     encode(list(
#'         c("age=Young", "relationship=Unmarried"),
#'         c("age=Senior")
#'       ), itemLabels = Adult),
#'     "transactions")
#'
#' twoTransactions
#' inspect(twoTransactions)
#'
#' ## the same using the transactions constructor function instead
#' twoTransactions <- transactions(
#'     list(
#'         c("age=Young", "relationship=Unmarried"),
#'         c("age=Senior")
#'     ), itemLabels = Adult)
#'
#' twoTransactions
#' inspect(twoTransactions)
#'
#' ## Example 5: Use a common item coding
#'
#' # Creation of transactions separately will produce different item codings
#' trans1 <- transactions(
#'     list(
#'         c("age=Young", "relationship=Unmarried"),
#'         c("age=Senior")
#'     ))
#' trans1
#'
#' trans2 <- transactions(
#'     list(
#'         c("age=Middle-aged", "relationship=Married"),
#'         c("relationship=Unmarried", "age=Young")
#'     ))
#' trans2
#'
#' compatible(trans1, trans2)
#'
#' # produce common item coding (all item labels in the two sets)
#' commonItemLabels <- union(itemLabels(trans1), itemLabels(trans2))
#' commonItemLabels
#'
#' trans1 <- recode(trans1, itemLabels = commonItemLabels)
#' trans1
#' trans2 <- recode(trans2, itemLabels = commonItemLabels)
#' trans2
#'
#' compatible(trans1, trans2)
#'
#'
#' ## Example 6: manually create a rule using the item coding in Adult
#' ## and calculate interest measures
#' aRule <- new("rules",
#'   lhs = encode(list(c("age=Young", "relationship=Unmarried")),
#'     itemLabels = Adult),
#'   rhs = encode(list(c("income=small")),
#'     itemLabels = Adult)
#' )
#'
#' ## shorter version using the rules constructor
#' aRule <- rules(
#'   lhs = list(c("age=Young", "relationship=Unmarried")),
#'   rhs = list(c("income=small")),
#'   itemLabels = Adult
#' )
#'
#' quality(aRule) <- interestMeasure(aRule,
#'   measure = c("support", "confidence", "lift"), transactions = Adult)
#'
#' inspect(aRule)
setGeneric("decode",
  function(x, ...)
    standardGeneric("decode"))

#' @rdname itemCoding
#' @aliases decode
setMethod("decode", signature(x = "numeric"),
  function(x, itemLabels)
    itemLabels[x])

#' @rdname itemCoding
setMethod("decode", signature(x = "list"),
  function(x, itemLabels)
    lapply(x, function(x)
      itemLabels[x]))


#' @rdname itemCoding
#' @aliases encode
setGeneric("encode",
  function(x, ...)
    standardGeneric("encode"))

#' @rdname itemCoding
setMethod("encode", signature(x = "character"),
  function(x, itemLabels, itemMatrix = TRUE) {
    ## itemMatrix always is created from list
    if (itemMatrix == TRUE)
      return(encode(list(x), itemLabels, itemMatrix == TRUE))
    
    ## regular encoding
    r <- which(itemLabels %in% x)
    if (length(r) < length(x))
      warning(
        "The following item labels are not available in itemLabels: ",
        paste(setdiff(x, itemLabels), collapse = ", "),
        "\nItems with missing labels are dropped!",
        call. = FALSE
      )
    r
  })

#' @rdname itemCoding
setMethod("encode", signature(x = "numeric"),
  function(x, itemLabels, itemMatrix = TRUE) {
    ## itemMatrix always is created from list
    if (itemMatrix == TRUE)
      return(encode(list(x), itemLabels, itemMatrix == TRUE))
    
    ## handle empty sets
    if (length(x) == 0)
      return(integer(0))
    
    ## regular encoding
    r <- range(x)
    if (r[1] < 1 || r[2] > length(itemLabels))
      stop("Invalid item ID in ", deparse(x), call. = FALSE)
    
    ## deal with numeric
    if (!is.integer(x)) {
      if (any(x %% 1 != 0))
        stop("Invalid item ID (needs to be integer) in ", deparse(x), call. = FALSE)
      x <- as.integer(x)
    }
    x
  })

## NOTES this is less error prone than creating ngCMatrix
##       directly in internal code.

#' @rdname itemCoding
setMethod("encode", signature(x = "list"),
  function(x, itemLabels, itemMatrix = TRUE) {
    if (is(itemLabels, "itemMatrix") ||
        is(itemLabels, "association"))
      itemLabels <- itemLabels(itemLabels)
    
    # this calls encode for character
    i <- lapply(x, encode, itemLabels, itemMatrix = FALSE)
    if (itemMatrix == FALSE)
      return(i)
    
    ## yuck
    if (!length(i))
      return(recode(new("itemMatrix"), itemLabels))
    
    ## fix Matrix mess  (ceeboo 2009)
    i <- lapply(i, sort)
    
    p <- sapply(i, length)
    names(p) <- NULL
    p <- cumsum(p)
    i <- unlist(i, use.names = FALSE)
    
    i <- new(
      "ngCMatrix",
      p   = c(0L, p),
      i   = i - 1L,
      Dim = c(length(itemLabels), length(p))
    )
    
    ## item labels must be character
    new(
      "itemMatrix",
      data     = i,
      itemInfo = data.frame(
        labels = as.character(itemLabels),
        stringsAsFactors = FALSE
      )
    )
  })


#' @rdname itemCoding
#' @aliases recode
setGeneric("recode",
  function(x, ...)
    standardGeneric("recode"))

#' @rdname itemCoding
setMethod("recode", signature(x = "itemMatrix"),
  function(x,
    itemLabels = NULL,
    match = NULL) {
    ### FIXME: Deprecated
    if (!is.null(match))
      message("recode: parameter 'match' is deprecated. Use 'itemLabels' instead.")
    
    if (!is.null(itemLabels) && !is.null(match))
      stop("'match' and 'itemLabels' cannot both be specified")
    if (is.null(itemLabels))
      if (is.null(match))
        stop("Either 'match' or 'itemLabels' has to be specified")
    else
      itemLabels <- itemLabels(match)
    ### END
    
    if (is(itemLabels, "itemMatrix") ||
        is(itemLabels, "association"))
      itemLabels <- itemLabels(itemLabels)
    
    ## nothing to do
    if (identical(itemLabels(x), itemLabels))
      return(x)
    
    k <- match(itemLabels(x), itemLabels)
    if (any(is.na(k)))
      stop ("All item labels in x must be contained in 'itemLabels'.", call. = FALSE)
    
    ## recode items
    if (any(k != seq(length(k))))
      x@data <- .Call(R_recode_ngCMatrix, x@data, k)
    
    ## enlarge matrix for additional items
    if (x@data@Dim[1] <  length(itemLabels))
      x@data@Dim[1] <- length(itemLabels)
    
    if (!is.null(match))
      itemInfo(x) <- itemInfo(match)
    else
      itemInfo(x) <-
      data.frame(labels = as.character(itemLabels),
        stringsAsFactors = FALSE)
    
    validObject(x)
    x
  })

#' @rdname itemCoding
setMethod("recode", signature(x = "itemsets"),
  function(x,
    itemLabels = NULL,
    match = NULL) {
    x@items <- recode(x@items, itemLabels, match)
    x
  })

#' @rdname itemCoding
setMethod("recode", signature(x = "rules"),
  function(x,
    itemLabels = NULL,
    match = NULL) {
    x@lhs <- recode(x@lhs, itemLabels, match)
    x@rhs <- recode(x@rhs, itemLabels, match)
    x
  })


#' @rdname itemCoding
setGeneric("compatible",
  function(x, y)
    standardGeneric("compatible"))

#' @rdname itemCoding
setMethod("compatible", signature(x = "itemMatrix"),
  function(x, y)
    identical(itemLabels(x), itemLabels(y)))

#' @rdname itemCoding
setMethod("compatible", signature(x = "associations"),
  function(x, y)
    identical(itemLabels(x), itemLabels(y)))