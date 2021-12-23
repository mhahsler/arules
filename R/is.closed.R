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


#' Find Closed Itemsets
#'
#' Provides the generic function and the S4 method `is.closed()` for finding
#' closed itemsets. Closed itemsets are used as a concise representation of
#' frequent itemsets. The closure of an itemset is its largest proper superset
#' which has the same support (is contained in exactly the same transactions).
#' An itemset is closed, if it is its own closure (Pasquier et al. 1999).
#'
#' Closed frequent itemsets can also be mined directly using
#' [apriori()] or [eclat()] with target `"closed frequent
#' itemsets"`.
#'
#' @aliases is.closed
#' @family postprocessing
#' @family associations functions
#' 
#' @param x a set of itemsets.
#' @return a logical vector with the same length as \code{x} indicating for
#' each element in \code{x} if it is a closed itemset.
#' @author Michael Hahsler
#' @references Nicolas Pasquier, Yves Bastide, Rafik Taouil, and Lotfi Lakhal
#' (1999). Discovering frequent closed itemsets for association rules. In
#' \emph{Proceeding of the 7th International Conference on Database Theory},
#' Lecture Notes In Computer Science (LNCS 1540), pages 398--416. Springer,
#' 1999.
#' @keywords models
setGeneric("is.closed",
  function(x)
    standardGeneric("is.closed"))

#' @rdname is.closed
setMethod("is.closed", signature(x = "itemsets"),
  function(x) {
    ## An itemset X is closed if no proper super set of X is contained
    ## in every transaction in which X is contained. Which means there
    ## exists no super set of X with the same support count as X
    
    ## Check if the set of itemsets is unique in order to
    ## avoid the problem that the same itemset could have
    ## different support counts.
    if (any(is.na(match(x, unique(x), nomatch = NA))))
      stop("itemsets not unique")
    
    ## R_pnclosed only supports abs. support counts
    cnt <- quality(x)$count
    if (is.null(cnt)) {
      ntrans <-  info(x)$ntransactions
      if (is.null(ntrans))
        stop(
          sQuote("x"),
          " does not contain sufficient support information (ntransations is missing in info)"
        )
      supp <- quality(x)$support
      if (is.null(supp))
        stop(
          sQuote("x"),
          " does not contain sufficient support information (support is missing in quality)"
        )
      
      cnt <- as.integer(round(supp * ntrans))
    }
    
    structure(.Call(R_pnclosed, x@items@data, as.integer(cnt), FALSE),
      names = labels(x))
  })