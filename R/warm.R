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


#' Computing Transaction Weights With HITS
#'
#' Compute the hub transaction weights for a collection
#' of [transactions] using the HITS (hubs and authorities) algorithm.
#'
#' Model a collection of [transactions] as a bipartite graph of hubs
#' (transactions) and authorities (items) with unit arcs and free node weights.
#' That is, a transaction weight is the sum of the (normalized) weights of the
#' items and vice versa. The weights are estimated by iterating the model to a
#' steady-state using a builtin convergence tolerance of `FLT_EPSILON` for
#' (the change in) the norm of the vector of authorities.
#'
#' @family weighted association mining functions
#'
#' @param data an object of or coercible to class [transactions].
#' @param iter an integer value specifying the maximum number of iterations to
#' use.
#' @param tol convergence tolerance (default `FLT_EPSILON`).
#' @param type a string value specifying the norming of the hub weights.  For
#' `"normed"` scale the weights to unit length (L2 norm), and for
#' `"relative"` to unit sum.
#' @param verbose a logical specifying if progress and runtime information
#' should be displayed.
#' @return A `numeric` vector with transaction weights for `data`.
#' @author Christian Buchta
#' @references K. Sun and F. Bai (2008). Mining Weighted Association Rules
#' without Preassigned Weights. _IEEE Transactions on Knowledge and Data
#' Engineering_, 4 (30), 489--495.
#' @keywords models
#' @examples
#' data(SunBai)
#'
#' ## calculate transaction weigths
#' w <- hits(SunBai)
#' w
#'
#' ## add transaction weight to the dataset
#' transactionInfo(SunBai)[["weight"]] <- w
#' transactionInfo(SunBai)
#'
#' ## calulate regular item frequencies
#' itemFrequency(SunBai, weighted = FALSE)
#'
#' ## calulate weighted item frequencies
#' itemFrequency(SunBai, weighted = TRUE)
#' @export hits
hits <- function(
    data,
    iter = 16L,
    tol = NULL,
    type = c("normed", "relative", "absolute"),
    verbose = FALSE) {
  data <- as(data, "transactions")
  type <- match.arg(type)

  r <- .Call(R_hits_ngCMatrix, data@data, iter, tol, verbose)
  names(r) <- transactionInfo(data)[["transactionID"]]

  switch(type,
    normed   = r / sqrt(sum(r^2)),
    relative = r / sum(r),
    absolute = r
  )
}

#' Mining Associations from Weighted Transaction Data with Eclat (WARM)
#'
#' Find frequent [itemsets] with the Eclat algorithm. This implementation uses
#' optimized transaction ID list joins and transaction weights to implement weighted
#' association rule mining (WARM).
#'
#' Transaction weights are stored in the [transactions] as a column called
#' `weight` in [transactionInfo].
#'
#' The weighted support of an itemset is the sum of the weights of the
#' transactions that contain the itemset. An itemset is frequent if its
#' weighted support is equal or greater than the threshold specified by
#' `support` (assuming that the weights sum to one).
#'
#' Note that Eclat only mines (weighted) frequent itemsets. Weighted
#' association rules can be created using [ruleInduction()].
#'
#' @aliases WARM warm WECLAT
#' @family mining algorithms
#' @family weighted association mining functions
#'
#' @param data an object that can be coerced into an object of class
#' [transactions].
#' @param parameter an object of class [ASparameter] (default
#' values: `support = 0.1`, `minlen = 1L`, and `maxlen = 5L`) or
#' a named list with corresponding components.
#' @param control an object of class [AScontrol] (default values:
#' `verbose = TRUE`) or a named list with corresponding components.
#' @return Returns an object of class [itemsets]. Note that
#' weighted support is returned in [quality] as column `support`.
#' @note The C code can be interrupted by `CTRL-C`. This is convenient but comes
#' at the price that the code cannot clean up its internal memory.
#' @author Christian Buchta
#' @references G.D. Ramkumar, S. Ranka, and S. Tsur (1998).  Weighted
#' Association Rules: Model and Algorithm, _Proceedings of ACM SIGKDD._
#' @keywords models
#' @examples
#' ## Example 1: SunBai data
#' data(SunBai)
#' SunBai
#'
#' ## weights are stored in transactionInfo
#' transactionInfo(SunBai)
#'
#' ## mine weighted support itemsets using transaction support in SunBai
#' s <- weclat(SunBai,
#'   parameter = list(support = 0.3),
#'   control = list(verbose = TRUE)
#' )
#' inspect(sort(s))
#'
#' ## create rules using weighted support (satisfying a minimum
#' ## weighted confidence of 90%).
#' r <- ruleInduction(s, confidence = .9)
#' inspect(r)
#'
#' ## Example 2: Find association rules in weighted data
#' trans <- list(
#'   c("A", "B", "C", "D", "E"),
#'   c("C", "F", "G"),
#'   c("A", "B"),
#'   c("A"),
#'   c("C", "F", "G", "H"),
#'   c("A", "G", "H")
#' )
#'
#' weight <- c(5, 10, 6, 7, 5, 1)
#'
#' ## convert list to transactions
#' trans <- transactions(trans)
#'
#' ## add weight information
#' transactionInfo(trans) <- data.frame(weight = weight)
#' inspect(trans)
#'
#' ## mine weighed support itemsets
#' s <- weclat(trans,
#'   parameter = list(support = 0.3),
#'   control = list(verbose = TRUE)
#' )
#' inspect(sort(s))
#'
#' ## create association rules
#' r <- ruleInduction(s, confidence = .5)
#' inspect(r)
#' @export weclat
weclat <- function(
    data,
    parameter = NULL,
    control = NULL) {
  data <- as(data, "transactions")

  weight <- transactionInfo(data)[["weight"]]
  if (is.null(weight)) {
    weight <- rep(1, length(data))
    if (!is.null(control) && control$v) {
      cat("Transactions do not contain weights in transactionInfo. Using a weight of 1 for each.")
    }
  }

  weight <- as.numeric(weight)

  if (!is(parameter, "ASparameter")) {
    parameter <- do.call("new", c(list("ASparameter"), parameter))
  }
  if (!is(control, "AScontrol")) {
    control <- do.call("new", c(list("AScontrol"), control))
  }

  ## these are not available
  parameter@target <- NA_character_
  parameter@ext <- NA
  control@sort <- NA_integer_

  if (control@verbose) {
    cat("Weighted Eclat (WEclat)\n")
    cat("\nparameter specification:\n")
    show(parameter)
    cat("\nalgorithmic control:\n")
    show(control)
    cat("\n")
  }
  ## r <- .Call(R_transpose_ngCMatrix, data@data)
  ## r <- selectMethod("t", class(data@data))(data@data)
  r <- t(data@data)

  r <- .Call(
    R_weclat_ngCMatrix,
    r,
    weight,
    parameter@support,
    parameter@minlen,
    parameter@maxlen,
    control@verbose
  )
  names(r) <- c("data", "support")
  validObject(r$data)

  quality <- data.frame(support = r$support)

  r <- new("itemMatrix",
    data = r$data,
    itemInfo = data@itemInfo
  )
  info <- c(
    data = match.call()$data,
    ntransactions = length(data),
    support = parameter@support
  )

  r <- new("itemsets",
    items    = r,
    quality  = quality,
    info     = info
  )
  r
}
