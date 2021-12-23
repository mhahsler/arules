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


#' Support for Item Hierarchies
#'
#' Functions to use item hierarchies to aggregate items at different
#' group levels, to produce multi-level [transactions] and to filter spurious
#' associations mined from multi-level transactions.
#' 
#' Often an item hierarchy is available for [transactions] 
#' used for association rule
#' mining. For example in a supermarket dataset items like "bread" and "beagle"
#' might belong to the item group (category) "baked goods."
#'
#' Transactions can store item hierarchies as additional columns in the
#' itemInfo data.frame (`"labels"` is reserved for the item labels).
#'
#' \bold{Aggregation:} To perform analysis at a group level of the item
#' hierarchy, `aggregate()` produces a new object with items aggregated to
#' a given group level. A group-level item is present if one or more of the
#' items in the group are present in the original object.  If rules are
#' aggregated, and the aggregation would lead to the same aggregated group item
#' in the lhs and in the rhs, then that group item is removed from the lhs.
#' Rules or itemsets, which are not unique after the aggregation, are also
#' removed. Note also that the quality measures are not applicable to the new
#' rules and thus are removed.  If these measures are required, then aggregate
#' the transactions before mining rules.
#'
#' \bold{Multi-level analysis:} To analyze relationships between individual
#' items and item groups at the same time, `addAggregate()` can be used to
#' create a new transactions object which contains both, the original items and
#' group-level items (marked with a given postfix). In association rule mining,
#' all items are handled the same, which means that we will produce a large
#' number of rules of the type
#'
#' \deqn{item A => group of item A}
#'
#' with a confidence of 1. This will also happen if you mine itemsets.
#' `filterAggregate()` can be used to filter these spurious rules or
#' itemsets.
#'
#' @name hierarchy
#' @aliases aggregate
#' @family preprocessing
#' @family itemMatrix and transactions functions
#' 
#' @param x an transactions, itemsets or rules object.
#' @param by name of a field (hierarchy level) available in
#' [itemInfo] of `x` or a grouping vector of the same length
#' as items in `x` by which should be aggregated. Items with the same
#' group label in `by` will be aggregated into a single with that name.
#' Note that the grouping vector will be coerced to factor before use.
#' @param postfix characters added to mark group-level items.
#' @param ... further arguments.
#' 
#' @return `aggregate()` returns an object of the same class as `x`
#' encoded with a number of items equal to the number of unique values in
#' `by`. Note that for associations (itemsets and rules) the number of
#' associations in the returned set will most likely be reduced since several
#' associations might map to the same aggregated association and aggregate
#' returns a unique set. If several associations map to a single aggregated
#' association then the quality measures of one of the original associations is
#' randomly chosen.
#'
#' `addAggregate()` returns a new transactions object with the original
#' items and the group-items added. `filterAggregateRules()` returns a new
#' rules object with the spurious rules remove.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Groceries")
#' Groceries
#'
#' ## Groceries contains a hierarchy stored in itemInfo
#' head(itemInfo(Groceries))
#'
#' ## Example 1: Aggregate items using an existing hierarchy stored in itemInfo.
#' ## We aggregate to level2 stored in Groceries. All items with the same level2 label
#' ## will become a single item with that name.
#' ## Note that the number of items is therefore reduced to 55
#' Groceries_level2 <- aggregate(Groceries, by = "level2")
#' Groceries_level2
#' head(itemInfo(Groceries_level2)) ## labels are alphabetically sorted!
#'
#'
#' ## compare original and aggregated transactions
#' inspect(head(Groceries, 2))
#' inspect(head(Groceries_level2, 2))
#'
#' ## Example 2: Aggregate using a character vector.
#' ## We create here labels manually to organize items by their first letter.
#' mylevels <- toupper(substr(itemLabels(Groceries), 1, 1))
#' head(mylevels)
#'
#' Groceries_alpha <- aggregate(Groceries, by = mylevels)
#' Groceries_alpha
#' inspect(head(Groceries_alpha, 2))
#'
#' ## Example 3: Aggregate rules
#' ## Note: You could also directly mine rules from aggregated transactions to
#' ## get support, lift and support
#' rules <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.5))
#' rules
#' inspect(rules[1])
#'
#' rules_level2 <- aggregate(rules, by = "level2")
#' inspect(rules_level2[1])
#'
#' ## Example 4: Mine multi-level rules.
#' ## (1) Add aggregate items. These items will have labels ending with a *
#' Groceries_multilevel <- addAggregate(Groceries, "level2")
#' summary(Groceries_multilevel)
#' inspect(head(Groceries_multilevel))
#'
#' rules <- apriori(Groceries_multilevel,
#'   parameter = list(support = 0.01, conf = .9))
#' inspect(head(rules, by = "lift"))
#' ## Note that this contains many spurious rules of type 'item X => aggregate of item X'
#' ## with a confidence of 1 and high lift. We can filter spurious rules resulting from
#' ## the aggregation
#' rules <- filterAggregate(rules)
#' inspect(head(rules, by = "lift"))
addAggregate <- function(x, by, postfix = "*") {
  ## get aggregated items
  x_aggr <- aggregate(x, by)
  itemLabels(x_aggr) <- paste(itemLabels(x_aggr), postfix, sep = "")
  itemInfo(x_aggr)[["aggregatedBy"]] <- by
  
  ## merge with transactions
  x_m <- merge(x, x_aggr)
  
  ## add technical itemInfo
  itemInfo(x_m)[["aggregateLevels"]] <-
    c(rep(1L, times = nitems(x)),
      rep(2L, times = nitems(x_aggr)))
  
  itemInfo(x_m)[["aggregateID"]] <- c(nitems(x) +
      as.integer(as.factor(itemInfo(x)[[by]])),
    rep(0, times = nitems(x_aggr)))
  
  x_m
}

#' @rdname hierarchy 
filterAggregate <- function(x) {
  levels <- itemInfo(x)[["aggregateLevels"]]
  aggr <- itemInfo(x)[["aggregateID"]]
  if (is.null(levels) || is.null(aggr))
    stop("No aggregated hierarchy info available!")
  
  m <- as(items(x), "ngCMatrix")
  
  rem <- logical(length(x))
  for (i in 1:nrow(m)) {
    ## number of items including level 1
    if (levels[i] > 1)
      break   ## done with items
    rem <- rem | colSums(m[i, , drop = FALSE]) &
      colSums(m[aggr[i], , drop = FALSE])
  }
  
  x[!rem]
}


#' @rdname hierarchy
setGeneric("aggregate")

#' @rdname hierarchy
setMethod("aggregate", signature(x = "itemMatrix"),
  function(x, by) {
    ## we can specify the name from itemInfo in by
    if (length(by) == 1 && !is.null(itemInfo(x)[[by]]))
      by <- itemInfo(x)[[by]]
    
    by <- as.factor(by)
    
    if (length(by) != nitems(x))
      stop(
        "Name not available in itemInfo or supplied number of by does not match number of items in x!"
      )
    
    ## create an indicator matrix (cols are items)
    aggrMat <- as(sapply(
      levels(by),
      FUN = function(l)
        as.numeric(by == l)
    ), "dgCMatrix")
    
    ## count the items for each group and make binary
    x@data <-
      as(crossprod(aggrMat, as(as(x, "ngCMatrix"), "dgCMatrix")),
        "ngCMatrix")
    
    ## fix itemInfo
    ii <- x@itemInfo
    ii <- aggregate(ii, by = list(labels = by), FUN = unique)
    ii <- ii[,!sapply(ii, is.list), drop = FALSE]
    x@itemInfo <- ii
    
    validObject(x)
    x
  })

#' @rdname hierarchy
setMethod("aggregate", signature(x = "itemsets"),
  function(x, by) {
    new("itemsets", items = aggregate(items(x), by))
    
    ## first support value is used
    x <- unique(x)
    x
  })

#' @rdname hierarchy
setMethod("aggregate", signature(x = "rules"),
  function(x, by) {
    rhs <- aggregate(rhs(x), by)
    lhs <- aggregate(lhs(x), by)
    
    ## check if lhs items have to be removed
    lhs <- itemSetdiff(lhs, rhs)
    
    ### remove non-unique rules
    ## first support value is used
    x <- new("rules", lhs = lhs, rhs = rhs)
    x <- unique(x)
    x
  })

