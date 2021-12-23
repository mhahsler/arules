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


#' Supporting Transactions
#'
#' Find transactions which support each of a set of associations and return
#' this information as a transaction ID list.
#'
#' The supporting transactions are all transactions of which the itemset
#' representing the association is a subset of.
#'
#' @family itemMatrix and transactions functions
#' 
#' @param x a set of [associations] ([itemsets], [rules], etc.)
#' @param transactions an object of class [transactions] used to mine the
#' associations in \code{x}.
#' @param ... currently unused.
#' 
#' @return An object of class [tidLists] containing one transaction ID
#' list per association in \code{x}.
#' @author Michael Hahsler
#' @keywords models
#' @examples
#' data <- list(
#' 	c("a","b","c"),
#' 	c("a","b"),
#' 	c("a","b","d"),
#' 	c("b","e"),
#' 	c("b","c","e"),
#' 	c("a","d","e"),
#' 	c("a","c"),
#' 	c("a","b","d"),
#' 	c("c","e"),
#' 	c("a","b","d","e")
#' 	)
#' data <- as(data, "transactions")
#'
#' ## mine itemsets
#' f <- eclat(data, parameter = list(support = .2, minlen = 3))
#' inspect(f)
#'
#' ## find supporting Transactions
#' st <- supportingTransactions(f, data)
#' st
#'
#' as(st, "list")
setGeneric("supportingTransactions",
  function(x, transactions, ...)
    standardGeneric("supportingTransactions"))

#' @rdname supportingTransactions
setMethod("supportingTransactions",  signature(x = "associations"),
  function(x, transactions) {
    ss <- is.subset(x, transactions, sparse = TRUE)
    new(
      "tidLists",
      data = t(ss),
      itemInfo = data.frame(labels = labels(x)),
      transactionInfo = transactionInfo(transactions)
    )
  })
