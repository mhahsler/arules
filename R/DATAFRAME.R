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

#' @include itemMatrix.R transactions.R associations.R rules.R itemsets.R tidLists.R


#' @rdname DATAFRAME
setGeneric("DATAFRAME",
  function(from, ...) standardGeneric("DATAFRAME"))

#' Data.frame Representation for arules Objects
#' 
#' Provides the generic function `DATAFRAME()` and the methods to create
#' a data.frame representation from some arules objects. 
#' These methods are used for the coercion to a
#' data.frame, but offer more control over the coercion process (item
#' separators, etc.).
#' 
#' Using `DATAFRAME()` is equivalent to the standard coercion 
#' `as(x, "data.frame")`.  However, for rules, the argument `separate = TRUE`
#' will produce separate columns for the LHS and the RHS of the rule.
#' 
#' Furthermore, the arguments `itemSep`, `setStart`, `setEnd`
#' (and `ruleSep` for `separate = FALSE`) will be passed on to the
#' `labels()` method for the object specified in `from`.
#'
#' @name DATAFRAME
#' @family import/export
#'  
#' @param from the object to be converted into a data.frame.
#' @param separate logical; separate LHS and RHS in separate columns? (only for rules)
#' @param ... further arguments are passed on to the `labels()` method defined for the object in `from`.
#' @return a data.frame.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data(Adult)
#'   
#' DATAFRAME(head(Adult))
#' DATAFRAME(head(Adult), setStart = '', itemSep = ' + ', setEnd = '')
#' 
#' rules <- apriori(Adult, 
#'   parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
#' rules <- head(rules, by = "conf")
#' 
#' 
#' ### default coercions (same as as(rules, "data.frame"))
#' DATAFRAME(rules)
#' 
#' DATAFRAME(rules, separate = TRUE)
#' DATAFRAME(rules, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '')
#' @aliases DATAFRAME,rules-method
setMethod("DATAFRAME", signature(from = "rules"),
  function(from, separate = TRUE, ...) {
    if (separate) {
      antes <- labels(lhs(from), ...)
      conseqs <- labels(rhs(from), ...)
      
      data.frame(
        LHS = factor(antes, levels = unique(antes)),
        RHS = factor(conseqs, levels = unique(conseqs)),
        quality(from)
      )
    } else{
      rule <- labels(from, ...)
      
      data.frame(rule = factor(rule, levels = unique(rule)),
        quality(from))
    }
  })


#' @rdname DATAFRAME
setMethod("DATAFRAME", signature(from = "itemsets"),
  function(from, ...) {
    is <- labels(from, ...)
    
    data.frame(items = factor(is, levels = unique(is)),
      quality(from))
  })

#' @rdname DATAFRAME
setMethod("DATAFRAME", signature(from = "itemMatrix"),
  function(from, ...) {
    is <- labels(from, ...)
    
    df <- data.frame(items = factor(is, levels = unique(is)))
    if (nrow(itemsetInfo(from)) == nrow(df))
      df <- cbind(df, itemsetInfo(from))
    
    df
  })
