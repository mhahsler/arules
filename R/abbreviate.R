#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011-2015 Michael Hahsler and Sudheer Chelluboina
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


#' Abbreviate item labels in transactions, itemMatrix and associations
#'
#' Provides the generic function and the methods to abbreviate long item labels
#' in transactions, associations (rules and itemsets) and transaction ID lists.
#' Note that `abbreviate()` is not a generic and this \pkg{arules} defines a
#' generic with the R's abbreviate as the default.
#'
#' @include itemMatrix.R transactions.R associations.R rules.R itemsets.R tidLists.R
#' @name abbreviate
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param names.arg an object of class [transactions], [itemMatrix],
#' [itemsets], [rules] or [tidLists].
#' @param minlength number of characters allowed in abbreviation
#' @param method apply to level and value (both.sides)
#' @param ... further arguments passed on to the default abbreviation
#' function.
#' @author Sudheer Chelluboina and Michael Hahsler based on code by Martin
#' Vodenicharov.
#' @seealso [base::abbreviate()]
#' @keywords manip
#' @examples
#'
#' data(Adult)
#' inspect(head(Adult, 1))
#'
#' Adult_abbr <- abbreviate(Adult, 15)
#' inspect(head(Adult_abbr, 1))
#'
setGeneric("abbreviate", function(names.arg, ...)
  base::abbreviate(names.arg, ...))

#' @rdname abbreviate
setMethod("abbreviate", signature(names.arg = "itemMatrix"),
  function(names.arg,
    minlength = 4,
    ...,
    method = "both.sides") {
    ## both sides helps with labels of form variable=level
    itemInfo(names.arg)$labels_orig <- itemInfo(names.arg)$labels
    
    itemInfo(names.arg)$labels <- as.factor(abbreviate(itemInfo(names.arg)$labels, minlength, ..., method = method))
    
    names.arg
  })

#' @rdname abbreviate
setMethod("abbreviate", signature(names.arg = "transactions"),
  function(names.arg,
    minlength = 4,
    ...,
    method = "both.sides") {
    abbreviate(as(names.arg, "itemMatrix"),
      minlength = minlength,
      ...,
      method = method)
  })


#' @rdname abbreviate
setMethod("abbreviate", signature(names.arg = "rules"),
  function(names.arg,
    minlength = 4,
    ...,
    method = "both.sides") {
    names.arg@lhs <- abbreviate(lhs(names.arg),
      minlength = minlength,
      ...,
      method = method)
    names.arg@rhs <-
      abbreviate(rhs(names.arg),
        minlength = minlength,
        ...,
        method = method)
    names.arg
  })

#' @rdname abbreviate
setMethod("abbreviate", signature(names.arg = "itemsets"),
  function(names.arg,
    minlength = 4,
    ...,
    method = "both.sides") {
    names.arg@items <-
      abbreviate(items(names.arg),
        minlength = minlength,
        ...,
        method = method)
    names.arg
  })


#' @rdname abbreviate
setMethod("abbreviate", signature(names.arg = "tidLists"),
  function(names.arg,
    minlength = 4,
    ...,
    method = "both.sides") {
    abbreviate(as(names.arg, "itemMatrix"),
      minlength = minlength,
      ...,
      method = method)
  })
