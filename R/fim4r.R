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

#' Interface to Mining Algorithms from fim4r
#'
#' Interfaces the algorithms implemented in fim4r. The algorithms include: 
#' apriori, arules, eclat, fpgrowth, carpenter, estpsp, fim, genpsp, ista,
#' relim, sam. 
#' 
#' Note that the package \pkg{fim4r} is not available via CRAN. If needed, 
#' the `fim4r()` function installs the package automatically.
#' 
#' Note on additional parameters for `fim4r.x` functions:
#' 
#' * `report` is used internally and cannot be specified.
#' * method `"carpenter"` can only mine closed itemsets.
#' * for `appear`, the item ID needs to be used and not the item label. 
#'
#' @param transactions a [transactions] object
#' @param method the algorithm to be used. One of: "apriori", "eclat", "fpgrowth",
#'   "carpenter" (closed itemsets only), "ista", "relim", "sam"
#' @param target the target type. One of: "frequent", 
#'   "closed", "maximal", "generators" or "rules".
#' @param ... further arguments are passed on to the `fim4r.x()` in 
#'   package \pkg{fim4r} (`x` is the chosen method). Minimum support and
#'   minimum confidence can be set as parameters `supp` and `conf` 
#'   (note the range is \eqn{[0, 100]} percent).
#' @returns An object of class [itemsets] or [rules].
#' @references 
#' Christian Borgelt, fimi4r: Frequent Item Set Mining and Association Rule Induction for R.
#' \url{https://borgelt.net/fim4r.html}
#' @examples 
#' \dontrun{
#' data(Adult)
#' 
#' # note that fim4r specifies support and confidence out of 100%
#' r <- fim4r(Adult, method = "fpgrowth", target = "rules", supp = 50, conf = 80)
#' r
#' 
#' inspect(head(r, by = "lift"))
#' }
fim4r <-
  function(transactions,
    method = NULL,
    target = "frequent",
    ...) {
    #fim4r_url <- "https://borgelt.net/src/fim4r_1.7.tar.gz"
    fim4r_url <-
      "https://mhahsler.github.io/arules/docs/fim4r/fim4r_1.7.tar.gz"
    
    methods_rules <-
      c("apriori",
        "eclat",
        "fpgrowth")
    
    methods_itemsets <-
      c("carpenter",
        "ista",
        "relim",
        "sam")
    methods <- c(methods_rules, methods_itemsets)
    
    if (is.null(method) || is.na(pmatch(method, methods))) {
      cat("Available methods in fim4r are:",
        paste(methods, collapse = ", "))
      return()
    }
    method <- methods[pmatch(method, methods)]
    
    #targets: "frequent", "closed", "maximal", "generators", "rules"
    if (!is.na(pmatch("r", target)) &&
        is.na(pmatch(method, methods_rules)))
      stop(method, " cannot be used to mine target rules. \nOnly methods ",
        paste(methods_rules, collapse = ", "), " can mine rules directly.")
    
    if (!check_installed("fim4r", action = "check")) {
      question <-
        "Package fim4r is required.\nDownload and install the package?"
      cat(question, sep = '')
      if (utils::menu(c("Yes", "No")) != 1) {
        invokeRestart("abort")
      }
      utils::install.packages(fim4r_url, repos = NULL)
    }
    
    # prepare data
    transactions <- transactions(transactions)
    tracts <- LIST(transactions, decode = FALSE)
    
    res <- do.call(utils::getFromNamespace(paste0("fim4r.", method), "fim4r"),
      args = c(list(
        tracts = tracts,
        target = target,
        report = "scl"
      ),
        list(...)))
    res
    
    # handle no rules/itemsets
    if (length(res) == 0) {
      nosets <- encode(list(), itemLabels = itemLabels(transactions))
      if (!is.na(pmatch("r", target))) {
        return(rules(lhs = nosets, rhs = nosets))
      } else{
        return(itemsets(items = nosets))
      }
    }
    
    # encode rules/itemsets
    if (!is.na(pmatch("r", target))) {
      # for rules
      rhs <- lapply(res, "[[", 1)
      rhs <- encode(rhs, itemLabels = itemLabels(transactions))
      lhs <- lapply(res, "[[", 2)
      lhs <- encode(lhs, itemLabels = itemLabels(transactions))
      qual <- do.call(rbind.data.frame, lapply(res, "[[", 3))
      qual <-
        cbind(qual, as.integer(qual[[1]] * length(transactions)))
      colnames(qual) <-
        c("support", "confidence", "lift", "count")
      return(rules(
        lhs = lhs,
        rhs = rhs,
        quality = qual
      ))
    } else{
      # for itemsets
      items <- lapply(res, "[[", 1)
      items <-
        encode(items, itemLabels = itemLabels(transactions))
      qual <-
        do.call(rbind.data.frame, lapply(res, "[[", 2))[, 1, drop = FALSE]
      qual <-
        cbind(qual, as.integer(qual[[1]] * length(transactions)))
      colnames(qual) <- c("support", "count")
      return(itemsets(items = items, quality = qual))
    }
  }