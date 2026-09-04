#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
# 			Bettina Gruen and Kurt Hornik
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

#' Support Counting for Itemsets
#'
#' Counts support for itemsets represented by an [itemMatrix] or an
#' [associations] object in a [transactions] data set.
#'
#' Normally, the support of frequent itemsets is counted efficiently during the
#' mining process using a minimum support threshold.
#' However, if only the support for specific itemsets (maybe itemsets with very low support)
#' is needed, or the support of a set of itemsets needs to be recalculated on
#' different [transactions] than they were mined on, then `support()` can be used.
#'
#' Several methods for support counting are available:
#'
#' * `"ptree"` (default method): The counters for the itemsets
#'   are organized in a prefix tree. The transactions are sequentially processed
#'   and the corresponding counters in the prefix tree are incremented (see
#'   Hahsler et al, 2008). This method is used by default since it is typically
#'   significantly faster than transaction ID list intersection.
#'
#' * `"tidlists"`: Support is counted using
#'   transaction ID list intersection which is used by several fast mining
#'   algorithms (e.g., by Eclat). However, support is determined for each itemset
#'   individually which is slow for a large number of long itemsets in dense
#'   data.
#'
#' The item coding of `x` and `transactions` is reconciled using item labels.
#' Items that occur only in `transactions` do not affect the count. With
#' `reduce = TRUE`, unused items are removed before prefix-tree counting.
#'
#' Weighted support uses the numeric `weight` column in
#' `transactionInfo(transactions)`. Absolute weighted support is the sum of the
#' weights of supporting transactions; relative weighted support divides this
#' value by the sum of all transaction weights.
#'
#' @aliases support
#' @family interest measures
#'
#' @param x an [itemMatrix] or [associations] object containing the itemsets for
#' which support is counted.
#' @param transactions the [transactions] data set in which support is counted.
#' @param type return `"relative"` support or `"absolute"` counts (or summed
#' weights when `weighted = TRUE`).
#' @param weighted logical; use transaction weights stored in the `weight`
#' column of [transactionInfo()]?
#' @param method support-counting method: `"ptree"` or `"tidlists"`.
#' @param reduce logical; remove unused items before prefix-tree counting?
#' @param verbose logical; report progress and timing information?
#' @param ... further arguments passed from the generic to a method.
#'
#' @return An unnamed numeric vector of length `length(x)`. Values are relative
#' supports when `type = "relative"` and counts or weight sums when
#' `type = "absolute"`.
#' @author Michael Hahsler and Christian Buchta
#' @references Michael Hahsler, Christian Buchta, and Kurt Hornik. Selective
#' association rule generation. _Computational Statistics_, 23(2):303-315,
#' April 2008.
#' @keywords models
#' @examples
#' data("Income")
#'
#' ## find and some frequent itemsets
#' itemsets <- eclat(Income)[1:5]
#'
#' ## inspect the support returned by eclat
#' inspect(itemsets)
#'
#' ## count support in the database
#' support(items(itemsets), Income)
#' support(itemsets, Income, type = "absolute")
#' @export
setGeneric(
  "support",
  function(x, transactions, ...) {
    standardGeneric("support")
  }
)

#' @rdname support
setMethod(
  "support", signature(x = "itemMatrix"),
  function(
      x,
      transactions,
      type = c("relative", "absolute"),
      method = c("ptree", "tidlists"),
      reduce = FALSE,
      weighted = FALSE,
      verbose = FALSE,
      ...) {
    if (!is(transactions, "transactions")) {
      stop("transactions missing. Please specify the transactions used to mine the itemsets!")
    }

    if (weighted &&
      !("weight" %in% colnames(transactionInfo(transactions)))) {
      stop("transactions do not contain weights. Add a weight column to transactionInfo.")
    }

    type <- match.arg(type)
    method <- match.arg(method)

    if (verbose) {
      cat("using method:", method, "\n")
    }

    ## conform
    k <- match(itemLabels(transactions), itemLabels(x))
    n <- which(is.na(k))
    if (length(n)) {
      k[n] <- x@data@Dim[1] + seq(length(n))
      x@data@Dim[1] <- x@data@Dim[1] + length(n)
      ## may not be needed
      x@itemInfo <-
        transactions@itemInfo <-
        rbind(x@itemInfo, transactions@itemInfo[n, , drop = FALSE])
    }
    if (any(k != seq_len(length(k)))) {
      transactions@data <-
        .Call(R_recode_ngCMatrix, transactions@data, k)
    }
    if (transactions@data@Dim[1] < x@data@Dim[1]) {
      transactions@data@Dim[1] <- x@data@Dim[1]
    }

    if (weighted) {
      tm <-
        system.time(support <-
          support.weighted(x, transactions, reduce = reduce, verbose = verbose))
      total <- sum(transactionInfo(transactions)[["weight"]])
    } else {
      total <- length(transactions)
      if (method == "ptree") {
        tm <-
          system.time(support <-
            support.ptree(x, transactions, reduce = reduce, verbose = verbose))
      } else {
        tm <-
          system.time(support <-
            support.tidlists(x, transactions, reduce = reduce, verbose = verbose))
      }
    }

    if (verbose) {
      cat("timing:", sum(tm[1:2]), "sec.\n")
    }

    switch(type,
      relative = support / total,
      absolute = support
    )
  }
)


## UNUSED: We have now a C implementation
support.tidlists.inR <- function(x, transactions, control = NULL) {
  if (nitems(x) != nitems(transactions)) {
    stop("number of items in x and transactions do not match.")
  }

  ## prepare tid-list and list of itemsets
  tlists <- LIST(as(transactions, "tidLists"), decode = FALSE)
  xitems <- LIST(x, decode = FALSE)

  ## select tid-lists for items and do intersection
  support <- sapply(
    xitems,
    FUN = function(i) {
      tidls <- unlist(tlists[i])
      if (!is.null(tidls)) {
        supp <- sum(tabulate(tidls) == length(i))
      } else {
        supp <- 0
      }
      supp
    }
  )

  # names(support) <- labels(x)
  support
}

support.tidlists <-
  function(
      x,
      transactions,
      reduce = FALSE,
      verbose = FALSE) {
    if (nitems(x) != nitems(transactions)) {
      stop("number of items in x and transactions do not match.")
    }

    if (reduce) {
      warning("method tidlists does not use reduce")
    }

    tid <- as(transactions, "tidLists")

    support <- .Call(R_tid_support, tid@data, x@data)

    # names(supports) <- labels(x)
    support
  }

support.ptree <-
  function(
      x,
      transactions,
      reduce = FALSE,
      verbose = FALSE) {
    .Call(
      R_pncount,
      x@data,
      transactions@data,
      TRUE,
      as.logical(reduce),
      as.logical(verbose)
    )
  }

support.weighted <- function(
    x,
    transactions,
    reduce = FALSE,
    verbose = FALSE) {
  weights <- as.numeric(transactionInfo(transactions)[["weight"]])

  .Call(
    R_wcount_ngCMatrix,
    x@data,
    # t(transactions@data),
    selectMethod("t", class(transactions@data))(transactions@data),
    weights,
    NULL,
    NULL,
    as.logical(verbose)
  )
}

#' @rdname support
setMethod(
  "support", signature(x = "associations"),
  function(
      x,
      transactions,
      type = c("relative", "absolute"),
      method = c("ptree", "tidlists"),
      reduce = FALSE,
      weighted = FALSE,
      verbose = FALSE,
      ...) {
    support(
      items(x),
      transactions = transactions,
      type = type,
      method = method,
      reduce = reduce,
      weighted = weighted,
      verbose = verbose,
      ...
    )
  }
)
