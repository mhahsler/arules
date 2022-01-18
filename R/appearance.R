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


#' Class APappearance --- Specifying the appearance Argument of Apriori to
#' Implement Rule Templates
#'
#' Specifies the restrictions on the associations mined by
#' [apriori()]. These restrictions can implement certain aspects of
#' rule templates described by Klemettinen (1994).
#'
#' Note that appearance is only supported by the implementation of
#' [apriori()].
#'
#' @name APappearance-class
#' @aliases APappearance
#' @family mining algorithms
#' 
#' @section Objects from the Class: 
#' If appearance restrictions are used, an
#' appearance object will be created automatically within the
#' [apriori()] function using the information in the named list of
#' the function's `appearance` argument.  In this case, the item labels
#' used in the list will be automatically matched against the items in the used
#' [transactions].
#' 
#' Objects can also be created by calls of the form `new("APappearance",
#' ...)`.  In this case, item IDs (column numbers of the transactions incidence
#' matrix) have to be used instead of labels.
#' 
#' @slot labels character vectors giving the labels of the
#' items which can appear in the specified place (rhs, lhs or both for rules
#' and items for itemsets).  none specifies, that the items mentioned there
#' cannot appear anywhere in the rule/itemset. Note that items cannot be
#' specified in more than one place (i.e., you cannot specify an item in lhs
#' and rhs, but have to specify it as both).
#' @slot default one of
#' `"both"`, `"lhs"`, `"rhs"`, `"none"`.  Specified the
#' default appearance for all items not explicitly mentioned in the other
#' elements of the list.  Leave unspecified and the code will guess the correct
#' setting. 
#' @slot set used internally.
#' @slot items used internally.
#'
#' @author Michael Hahsler and Bettina Gruen
#' @references Christian Borgelt (2004) _Apriori --- Finding Association
#' Rules/Hyperedges with the Apriori Algorithm._
#' \url{https://borgelt.net/apriori.html}
#'
#' M. Klemettinen, H. Mannila, P. Ronkainen, H. Toivonen and A. I. Verkamo
#' (1994).  Finding Interesting Rules from Large Sets of Discovered Association
#' Rules.  In _Proceedings of the Third International Conference on
#' Information and Knowledge Management,_ 401--407.
#' @keywords classes
#' @examples
#' data("Adult")
#'
#' ## find only frequent itemsets which do not contain small or large income
#' is <- apriori(Adult, parameter = list(support= 0.1, target="frequent"),
#'   appearance = list(none = c("income=small", "income=large")))
#' itemFrequency(items(is))["income=small"]
#' itemFrequency(items(is))["income=large"]
#'
#' ## find itemsets that only contain small or large income, or young age
#' is <- apriori(Adult, parameter = list(support= 0.1, target="frequent"),
#'   appearance = list(items = c("income=small", "income=large", "age=Young")))
#' inspect(head(is))
#'
#' ## find only rules with income-related variables in the right-hand-side.
#' incomeItems <- grep("^income=", itemLabels(Adult), value = TRUE)
#' incomeItems
#' rules <- apriori(Adult, parameter = list(support=0.2, confidence = 0.5),
#'   appearance = list(rhs = incomeItems))
#' inspect(head(rules))
#'
#' ## Note: For more complicated restrictions you have to mine all rules/itemsets and
#' ## then filter the results afterwards.
setClass(
  "APappearance",
  representation(
    set     = "integer",
    items   = "integer",
    labels  = "character",
    default = "character"
  ),
  
  prototype(
    set     = rep(0L, 5),
    items   = integer(),
    labels  = "",
    default = "both"
  ),
  
  validity = function(object) {
    if (!object@default %in% c("lhs", "rhs", "none", "both"))
      return("Default value not specified correctly")
    else if (!sum(object@set) == length(object@items))
      return("Slots 'set' and 'items' do not match")
    return(TRUE)
  }
)

#' @rdname APappearance-class
#' @name coercion-APappearance
#' @section Coercions:
#' 
#' * `as("NULL", "APappearance")`
#' * `as("list", "APappearance")`
#'
#' @aliases 
#' coerce,NULL,APappearance-method 
#' coerce,list,APappearance-method
NULL

setAs("NULL", "APappearance",
  function(from, to) {
    new(to)
  })

setAs("list", "APappearance",
  function(from, to) {
    if (!length(from))
      return(new("APappearance"))
    
    if (is.null(from$labels))
      stop("labels are missing")
    
    args <- c("lhs", "rhs", "both", "none", "items")
    other <- c("default", "labels")
    if (!all(names(from) %in% c(args, other)))
      stop(paste(
        names(from)[!names(from) %in% c(args, other)],
        "is an unknown appearance indicator, use:",
        paste(args, collapse = " "),
        collapse = ", "
      ))
    
    ## cannot set items and lhs, rhs or both
    if (!is.null(from$items) &&
        (!is.null(from$lhs) ||
            !is.null(from$rhs) || !is.null(from$both))) {
      stop(
        "Cannot set appearance for mining association rules (lhs, rhs, both) and frequent itemset mining (items) at the same time!"
      )
    }
    
    ## guess default
    if (is.null(from$default)) {
      if (is.null(from$lhs)
        && is.null(from$rhs))
        from$default <- "both"
      if (!is.null(from$lhs)
        && is.null(from$rhs))
        from$default <- "rhs"
      if (is.null(from$lhs)
        && !is.null(from$rhs))
        from$default <- "lhs"
      
      if (!is.null(from$rhs)
        && !is.null(from$lhs))
        from$default <- "none"
      
      if (!is.null(from$both))
        from$default <- "none"
      
      ## for itemsets
      if (!is.null(from$items))
        from$default <- "none"
    }
    
    set <- c()
    items <- c()
    for (i in seq_len(length(args))) {
      indicator <- from[[args[i]]]
      if (is.null(indicator))
        add_items <- NULL
      else if (!all(indicator %in% from$labels))
        stop(paste(indicator[!indicator %in% from$labels],
          "is an unknown item label", collapse = ", "))
      else
        add_items <- unlist(sapply(indicator,
          function(x) {
            which(from$labels == x) - 1
          }))
      items <- c(items, add_items)
      set <- c(set, length(add_items))
    }
    
    ## check for items in multiple positions (crashes C code!)
    if (any(dup <- duplicated(items)))
      stop(
        "The following items cannot be specified in multiple appearance locations: ",
        paste(from$labels[items[dup] + 1L], collapse = ", ")
      )
    
    ## check NA's
    return(
      new(
        "APappearance",
        default = from$default,
        items = as.integer(items),
        set = set,
        labels = from$labels
      )
    )
  })
