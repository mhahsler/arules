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


#' Find Duplicated Elements
#' 
#' Provides the generic function `duplicated()` and the S4 methods to find 
#' duplicated elements in
#' [itemMatrix], [associations] and their subclasses.
#' 
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param x an object of class [itemMatrix] or [associations].
#' @param \dots further arguments (currently unused).
#' @param incomparables argument currently unused.
#' @return A logical vector indicating duplicated elements.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#' 
#' r1 <- apriori(Adult[1:1000], parameter = list(support = 0.5))
#' r2 <- apriori(Adult[1001:2000], parameter = list(support = 0.5))
#' 
#' ## Note this creates a collection of rules from two sets of rules
#' r_comb <- c(r1, r2)
#' duplicated(r_comb)
setGeneric("duplicated")

#' @rdname duplicated
setMethod("duplicated", signature(x = "itemMatrix"),
  function(x, incomparables = FALSE) {
    ## use a prefix tree
    i <- .Call(R_pnindex, x@data, NULL, FALSE)
    duplicated(i)
  })

#' @rdname duplicated
setMethod("duplicated", signature(x = "rules"),
  function(x, incomparables = FALSE)
    duplicated(.joinedList(x), incomparables = incomparables))

#' @rdname duplicated
setMethod("duplicated", signature(x = "itemsets"),
  function(x, incomparables = FALSE)
    duplicated(x@items, incomparables = incomparables))


