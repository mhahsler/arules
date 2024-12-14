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


#' Remove Duplicated Elements from a Collection
#'
#' Provides the generic function `unique()` and the methods for
#' [itemMatrix] [transactions], and [associations].
#'
#' `unique()` uses [duplicated()] to return an
#' object with the duplicate elements removed.
#'
#' @family associations functions
#' @family itemMatrix and transactions functions
#'
#' @param x an object of class [itemMatrix] or [associations].
#' @param \dots further arguments (currently unused).
#' @param incomparables currently unused.
#' @return An object of the same class as `x` with duplicated elements
#' removed.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#'
#' r1 <- apriori(Adult[1:1000], parameter = list(support = 0.5))
#' r2 <- apriori(Adult[1001:2000], parameter = list(support = 0.5))
#'
#' ## Note that this produces a collection of rules from two sets
#' r_comb <- c(r1, r2)
#' r_comb <- unique(r_comb)
#' r_comb
setGeneric("unique")

#' @rdname unique
setMethod(
  "unique", signature(x = "itemMatrix"),
  function(x, incomparables = FALSE) {
    x[!duplicated(x, incomparables = incomparables)]
  }
)

## this needs a working implementation of duplicated for the
## type of associations

#' @rdname unique
setMethod(
  "unique", signature(x = "associations"),
  function(x, incomparables = FALSE, ...) {
    x[!duplicated(x, incomparables = incomparables, ...)]
  }
)
