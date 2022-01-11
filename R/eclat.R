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


#' Mining Associations with Eclat
#'
#' Mine frequent itemsets with the Eclat algorithm.  This algorithm uses simple
#' intersection operations for equivalence class clustering along with
#' bottom-up lattice traversal.
#'
#' Calls the C implementation of the Eclat algorithm by Christian Borgelt for
#' mining frequent itemsets.
#'
#' Eclat can also return the transaction IDs for each found itemset using
#' `tidLists = TRUE` as a parameter and the result can be retrieved as a
#' [tidLists] object with method `tidLists()` for class
#' [itemsets].  Note that storing transaction ID lists is
#' very memory intensive, creating transaction ID lists only works for minimum
#' support values which create a relatively small number of itemsets.  See also
#' [supportingTransactions()].
#'
#' [ruleInduction()] can be used to generate rules from the found
#' itemsets.
#'
#' A weighted version of ECLAT is available as function [weclat()].
#' This version can be used to perform weighted association rule mining (WARM).
#'
#' @family mining algorithms
#' @aliases ECLAT Eclat
#'
#' @param data object of class [transactions] or any data
#' structure which can be coerced into transactions (e.g.,
#' binary matrix, data.frame).
#' @param parameter object of class [ECparameter] or named
#' list (default values are: support 0.1 and maxlen 5)
#' @param control object of class [ECcontrol] or named list
#' for algorithmic controls.
#' @param ... Additional arguments are added for convenience to the parameter list.
#' @return Returns an object of class [itemsets].
#' @author Michael Hahsler and Bettina Gruen
#' @references Mohammed J. Zaki, Srinivasan Parthasarathy, Mitsunori Ogihara,
#' and Wei Li. (1997) _New algorithms for fast discovery of association
#' rules_.  KDD'97: Proceedings of the Third International Conference on
#' Knowledge Discovery and Data Mining, August 1997, Pages 283-286.
#'
#' Christian Borgelt (2003) Efficient Implementations of Apriori and Eclat.
#' _Workshop of Frequent Item Set Mining Implementations_ (FIMI 2003,
#' Melbourne, FL, USA).
#'
#' ECLAT Implementation: \url{https://borgelt.net/eclat.html}
#' @keywords models
#' @examples
#' data("Adult")
#' ## Mine itemsets with minimum support of 0.1 and 5 or less items
#' itemsets <- eclat(Adult,
#' 		parameter = list(supp = 0.1, maxlen = 5))
#' itemsets
#'
#' ## Create rules from the frequent itemsets
#' rules <- ruleInduction(itemsets, confidence = .9)
#' rules
#' @export eclat
eclat <- function(data,
  parameter = NULL,
  control = NULL,
  ...)
{
  ## prepare data
  data <- as(data, "transactions")
  items <- data@data
  if (is.null(parameter) ||
      !is(parameter, "ACparameter"))
    parameter <- as(c(parameter, list(...)), "ECparameter")
  control <- as(control, "ECcontrol")
  
  if (control@verbose) {
    cat("Eclat\n")
    cat("\nparameter specification:\n")
    show(parameter)
    cat("\nalgorithmic control:\n")
    show(control)
  }
  
  ## sanity check for support (abs. support >1)
  abs_supp <- as.integer(parameter@support * length(data))
  if (control@verbose) {
    cat("\nAbsolute minimum support count:", abs_supp, "\n\n")
  }
  
  ## the C code of eclat dies when no item is frequent so we do this
  if (max(itemFrequency(data)) < parameter@support) {
    if (control@verbose)
      message("eclat - zero frequent items\n")
    return(new("itemsets"))
  }
  
  ## call eclat
  result <- .Call(R_reclat,
    ## transactions
    items@p,
    items@i,
    items@Dim,
    ## parameter
    parameter,
    control,
    data@itemInfo)
  
  ## validate sparse Matrix (this takes care of sorting vector i)
  validObject(result@items@data)
  
  ## copy itemInfo
  result@items@itemInfo <- data@itemInfo
  
  ## empty itemsetInfo
  result@items@itemsetInfo <- data.frame()
  
  ## make sure quality is a data.frame
  result@quality <- as.data.frame(result@quality)
  
  ## add count to quality
  quality(result)$count <-
    as.integer(round(quality(result)$support * length(data)))
  
  ## add some reflectance
  call <- match.call()
  result@info <- list(
    data = call$data,
    ntransactions = length(data),
    support = parameter@support,
    call = deparse1(call)[1]
  )
  
  ## make sure tid list itemInfo is OK
  if (!is.null(result@tidLists)) {
    result@tidLists@itemInfo <- data.frame(labels = labels(result))
    result@tidLists@transactionInfo <- transactionInfo(data)
  }
  
  result
}
