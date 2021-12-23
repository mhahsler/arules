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

#' Combining Objects
#'
#' Provides the S4 methods to combine several objects based on
#' [itemMatrix] into a single object.
#'
#' Note, use [union()] rather than `c()` to combine several mined
#' [itemsets] (or [rules]) into a single
#' set.
#'
#' @name c
#' @aliases c
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param x first object.
#' @param y second object.
#' @param ... further objects of the same class as `x` to be combined.
#' @param recursive a logical. If `recursive = TRUE`, the function
#' recursively descends through lists combining all their elements into a
#' vector.
#' @return An object of the same class as `x`.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#'
#' ## combine transactions
#' a1 <- Adult[1:10]
#' a2 <- Adult[101:110]
#'
#' aComb <- c(a1, a2)
#' summary(aComb)
#'
#' ## combine rules (can contain the same rule multiple times)
#' r1 <- apriori(Adult[1:1000])
#' r2 <- apriori(Adult[1001:2000])
#' rComb <- c(r1, r2)
#' rComb
#'
#' ## union of rules (a set with only unique rules: same as unique(rComb))
#' rUnion <- union(r1,r2)
#' rUnion
#' @docType methods
NULL

#' @rdname c
setMethod("c", signature(x = "itemMatrix"),
  function(x, ..., recursive = FALSE) {
    ### this is rbind
    ### FIXME: labels are not sorted
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "itemMatrix"))
        stop("can only combine itemMatrix")
      
      x@itemsetInfo <- .combineMeta(x, y, "itemsetInfo")
      
      if (!compatible(x, y)) {
        warning("Item coding not compatible, recoding item matrices.")
        
        # expand x if y has additional items
        k <- match(itemLabels(y), itemLabels(x))
        n <- which(is.na(k))
        if (length(n)) {
          k[n] <- x@data@Dim[1] + seq(length(n))
          x@data@Dim[1] <- x@data@Dim[1] + length(n)
          x@itemInfo <- rbind(x@itemInfo,
            y@itemInfo[n, , drop = FALSE])
        }
        
        # recode y to match x
        if (any(k != seq_len(length(k))))
          y@data <- .Call(R_recode_ngCMatrix, y@data, k)
        if (y@data@Dim[1] <  x@data@Dim[1])
          y@data@Dim[1] <- x@data@Dim[1]
      }
      
      ## this is faster than x@data <- cbind(x@data, y@data)
      x@data <- .Call(R_cbind_ngCMatrix, x@data, y@data)
    }
    validObject(x, complete = TRUE)
    x
  })

#' @rdname c
setMethod("c", signature(x = "transactions"),
  function(x, ..., recursive = FALSE) {
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "transactions"))
        stop("can only combine transactions")
      x <- new("transactions",
        c(as(x, "itemMatrix"),
          as(y, "itemMatrix")),
        itemsetInfo = .combineMeta(x, y, "itemsetInfo"))
    }
    x
  })

#' @rdname c
setMethod("merge", signature(x = "transactions"),
  function(x, y, ...) {
    m <- merge(as(x, "itemMatrix"), as(y, "itemMatrix"))
    as(m, "transactions")
  })

#' @rdname c
setMethod("c", signature(x = "tidLists"),
  function(x, ..., recursive = FALSE) {
    args <- list(...)
    if (recursive)
      args <- unlist(args)
    
    dat <- x@data
    itemI <- itemInfo(x)
    for (y in args) {
      if (!is(y, "tidLists"))
        stop("can only combine tidLists.")
      
      if (ncol(x) != ncol(y))
        stop("transactions not conforming.")
      
      dat <- .Call(R_cbind_ngCMatrix, dat, y@data)
      itemI <- rbind(itemI, itemInfo(y))
    }
    
    x@data <- dat
    x@itemInfo <- itemI
    x
  })

#' @rdname c
setMethod("c", signature(x = "rules"),
  function(x, ..., recursive = FALSE) {
    args <- list(...)
    
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "rules"))
        stop("can combine rules only")
      
      ## retain identical info attributes
      info <- y@info
      if (length(info)) {
        k <- match(names(info), names(x@info))
        k <- mapply(identical, info, x@info[k])
        info <- info[k]
      }
      
      x <- new(
        "rules",
        lhs     = c(x@lhs, y@lhs),
        rhs     = c(x@rhs, y@rhs),
        quality = .combineMeta(x, y, "quality"),
        info    = info
      )
    }
    x
  })


#' @rdname c
setMethod("c", signature(x = "itemsets"),
  function(x, ..., recursive = FALSE) {
    args <- list(...)
    
    if (recursive)
      args <- unlist(args)
    for (y in args) {
      if (!is(y, "itemsets"))
        stop("can combine itemsets only")
      
      ## retain identical info attributes
      info <- y@info
      if (length(info)) {
        k <- match(names(info), names(x@info))
        k <- mapply(identical, info, x@info[k])
        info <- info[k]
      }
      
      x <- new(
        "itemsets",
        items   = c(x@items, y@items),
        quality = .combineMeta(x, y, "quality"),
        info    = info
      )
    }
    x
  })
