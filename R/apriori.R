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


#' Mining Associations with the Apriori Algorithm
#'
#' Mine frequent itemsets, association rules or association hyperedges using
#' the Apriori algorithm.
#'
#' The Apriori algorithm (Agrawal et al, 1993) employs level-wise search for
#' frequent itemsets. The used C implementation of Apriori by Christian
#' Borgelt (2003) includes some improvements (e.g., a prefix tree and item sorting).
#'
#' **Warning about automatic conversion of matrices or data.frames to transactions.**
#' It is preferred to create transactions manually before
#' calling `apriori()` to have control over item coding. This is especially
#' important when you are working with multiple datasets or several subsets of
#' the same dataset. To read about item coding, see [itemCoding].
#'
#' If a data.frame is specified as `x`, then the data is automatically
#' converted into transactions by discretizing numeric data using
#' [discretizeDF()] and then coercion to transactions. The discretization
#' may fail if the data is not well behaved.
#'
#' **Apriori only creates rules with one item in the RHS (Consequent).**
#' The default value in [APparameter] for `minlen` is 1.
#' This meains that rules with only one item (i.e., an empty antecedent/LHS)
#' like
#'
#' \deqn{\{\} => \{beer\}}{{} => {beer}}
#'
#' will be created.  These rules mean that no matter what other items are
#' involved, the item in the RHS will appear with the probability given by the
#' rule's confidence (which equals the support).  If you want to avoid these
#' rules then use the argument `parameter = list(minlen = 2)`.
#'
#' **Notes on run time and memory usage:**
#' If the minimum `support` is
#' chosen too low for the dataset, then the algorithm will try to create an
#' extremely large set of itemsets/rules. This will result in very long run
#' time and eventually the process will run out of memory.  To prevent this,
#' the default maximal length of itemsets/rules is restricted to 10 items (via
#' the parameter element `maxlen = 10`) and the time for checking subsets is
#' limited to 5 seconds (via `maxtime = 5`). The output will show if you hit
#' these limits in the "checking subsets" line of the output. The time limit is
#' only checked when the subset size increases, so it may run significantly
#' longer than what you specify in maxtime.  Setting `maxtime = 0` disables
#' the time limit.
#'
#' Interrupting execution with `Control-C/Esc` is not recommended.  Memory
#' cleanup will be prevented resulting in a memory leak. Also, interrupts are
#' only checked when the subset size increases, so it may take some time till
#' the execution actually stops.
#'
#' @family mining algorithms
#' @aliases APRIORI Apriori
#'
#' @param data object of class [transactions]. Any data
#' structure which can be coerced into transactions (e.g.,
#' a logical matrix, a data.frame or a tibble) can also be specified and will be
#' internally coerced to transactions. However, it is recommended to first create
#' a transactions object using [`transactions()`] and then to check that items are 
#' correctly created.
#' @param parameter object of class [APparameter] or named
#' list.  The default behavior is to mine rules with minimum support of 0.1,
#' minimum confidence of 0.8, maximum of 10 items (maxlen), and a maximal time
#' for subset checking of 5 seconds (`maxtime`).
#' @param appearance object of class [APappearance] or named
#' list.  With this argument item appearance can be restricted (implements rule
#' templates).  By default all items can appear unrestricted.
#' @param control object of class [APcontrol] or named list.
#' Controls the algorithmic performance of the mining algorithm (item sorting,
#' report progress (verbose), etc.)
#' @param ... Additional arguments are for convenience added to the parameter list.
#' @return Returns an object of class [rules] or
#' [itemsets].
#' @author Michael Hahsler and Bettina Gruen
#' @references
#' R. Agrawal, T. Imielinski, and A. Swami (1993) Mining
#' association rules between sets of items in large databases. In
#' _Proceedings of the ACM SIGMOD International Conference on Management
#' of Data_, pages 207--216, Washington D.C.
#' \doi{10.1145/170035.170072}
#'
#' Christian Borgelt (2012) Frequent Item Set Mining.  _Wiley
#' Interdisciplinary Reviews: Data Mining and Knowledge Discovery_
#' 2(6):437-456. J. Wiley & Sons, Chichester, United Kingdom 2012.
#' \doi{10.1002/widm.1074}
#'
#' Christian Borgelt and Rudolf Kruse (2002) Induction of Association Rules:
#' Apriori Implementation. _15th Conference on Computational Statistics_
#' (COMPSTAT 2002, Berlin, Germany) Physica Verlag, Heidelberg, Germany.
#'
#' Christian Borgelt (2003) Efficient Implementations of Apriori and Eclat.
#' _Workshop of Frequent Item Set Mining Implementations_ (FIMI 2003,
#' Melbourne, FL, USA).
#'
#' APRIORI Implementation: \url{https://borgelt.net/apriori.html}
#' @keywords models
#' @examples
#'
#' ## Example 1: Create transaction data and mine association rules
#' a_list <- list(
#'   c("a", "b", "c"),
#'   c("a", "b"),
#'   c("a", "b", "d"),
#'   c("c", "e"),
#'   c("a", "b", "d", "e")
#' )
#'
#' ## Set transaction names
#' names(a_list) <- paste("Tr", c(1:5), sep = "")
#' a_list
#'
#' ## Use the constructor to create transactions
#' trans1 <- transactions(a_list)
#' trans1
#'
#' rules <- apriori(trans1)
#' inspect(rules)
#'
#' ## Example 2: Mine association rules from an existing transactions dataset
#' ##   using different minimum support and minimum confidence thresholds
#' data("Adult")
#'
#' rules <- apriori(Adult,
#'   parameter = list(supp = 0.5, conf = 0.9, target = "rules")
#' )
#' summary(rules)
#'
#' # since ... gets automatically added to parameter, we can also write the
#' #  same call shorter:
#' apriori(Adult, supp = 0.5, conf = 0.9, target = "rules")
#' @export
apriori <-
  function(
      data,
      parameter = NULL,
      appearance = NULL,
      control = NULL,
      ...) {
    ## prepare data
    data <- as(data, "transactions")
    items <- data@data

    if (is(appearance, "list")) {
      appearance <-
        as(c(appearance, list(labels = itemLabels(data))), "APappearance")
    }
    appearance <- as(appearance, "APappearance")

    control <- as(control, "APcontrol")
    if (is.null(parameter) ||
      !is(parameter, "APparameter")) {
      parameter <- as(c(parameter, list(...)), "APparameter")
    }

    if (control@verbose) {
      cat("Apriori\n")
      cat("\nParameter specification:\n")
      show(parameter)
      cat("\nAlgorithmic control:\n")
      show(control)
    }

    ## sanity check for support (abs. support >1)
    abs_supp <- as.integer(parameter@support * length(data))
    if (control@verbose) {
      cat("\nAbsolute minimum support count:", abs_supp, "\n\n")
    }

    ## call apriori
    result <- .Call(
      R_rapriori,
      ## transactions
      items@p,
      items@i,
      items@Dim,
      ## parameter
      parameter,
      control,
      appearance,
      data@itemInfo
    )

    ## add some reflectance
    call <- match.call()
    result@info <- list(
      data = call$data,
      ntransactions = length(data),
      support = parameter@support,
      confidence = parameter@confidence,
      call = deparse1(call)[1]
    )

    ## add count to quality
    quality(result)$count <-
      as.integer(round(quality(result)$support * length(data)))

    if (is(result, "rules")) {
      ## validate sparse Matrix (this takes care of sorting vector i)
      validObject(result@lhs@data)
      validObject(result@rhs@data)
    } else {
      ## validate sparse Matrix (this takes care of sorting vector i)
      validObject(result@items@data)
    }
    result
  }
