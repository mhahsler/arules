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

#' Interface to Mining Algorithms from fim4r
#'
#' Interfaces the algorithms implemented in fim4r. The algorithms include:
#' Apriori, Eclat, FPgrowth, Carpenter, IsTa, RElim and SaM.
#'
#' **Installation:**
#' The package \pkg{fim4r} is not available via CRAN. If needed,
#' the `fim4r()` function downloads and installs the current version of the
#' package automatically. Your system needs to have build tools installed.
#'
#' Build tools: You need to be able to install source packages. For Windows
#' users this means that you need to install the
#' [RTools](https://cran.r-project.org/bin/windows/Rtools/) with a version
#' matching your R version.
#'
#' **Additional Notes:**
#'
#' * Support and confidence are specified here in the range \eqn{[0,1]}.
#'   This is different from the use in  `fim4r` package where `support` and `confidence`
#'   have the range \eqn{[0, 100]}.
#'   `arules::fim4r()` automatically converts support and confidence internally.
#' * `fim4r` methods also return the empty itemset while `arules` methods do not.
#' * See `? fim4r::fim4r` for help on additional available arguments. This is only available
#'   after package `fim4r` is installed.
#' * Algorithm descriptions and references can be found on the
#'   [fim4r web page](https://borgelt.net/fim4r.html) in the References Section.
#'
#' @family mining algorithms
#' @aliases fpgrowth FPgrowth carpenter ista IsTa relim RElim sam SaM
#'
#' @param transactions a [transactions] object
#' @param method the algorithm to be used. One of:
#'
#'   * `"apriori"`, `"eclat"`, `"fpgrowth"` can mine itemsets and rules.
#'   * `"relim"`, `"sam"` can mine itemsets.
#'   * `"carpenter"`, `"ista"`  can only mine closed itemset.
#'
#' @param support a numeric value for the minimal support in the range \eqn{[0,1]}.
#' @param confidence a numeric value for the minimal confidence of rules in the range \eqn{[0,1]}.
#' @param target the target type. One of: `"frequent"`,
#'   `"closed"`, `"maximal"`, `"generators"` or `"rules"`.
#' @param originalSupport logical; Use the support threshold on the support of the whole rule (LHS and RHS).
#'  If `FALSE`, then LHS support (i.e., coverage) is used by the support threshold.
#' @param report cannot be used via the interface.
#' @param appear Specify item appearance in rules (only for apriori, eclat, fpgrowth
#' and the target `"rules"`) Specify a list with two vectors (item labels and
#' appearance modifiers) of the same length. Appearance modifiers are:
#'
#'   * `"-"` (item may not appear in a rule),
#'   * `"a"` (item may only appear a rule antecedent/LHS),
#'   * `"c"` (item may only appear a rule consequent/RHS),
#'   * `"x"` (item may appear anywhere).
#' @param verbose logical; print used parameters?
#' @param ... further arguments are passed on to the function
#'    the `fim4r::fim4r` function for the given method. Examples
#'    are: `zmin`, `zmax`, `wgts`.
#' @returns An object of class [itemsets] or [rules].
#' @references
#' Christian Borgelt, fimi4r: Frequent Item Set Mining and Association Rule Induction for R.
#' \url{https://borgelt.net/fim4r.html}
#' @examples
#' \dontrun{
#' data(Adult)
#'
#' # list available algorithms
#' fim4r()
#'
#' # mine association rules with FPgrowth
#' r <- fim4r(Adult,
#'   method = "fpgrowth",
#'   target = "rules", support = .7, confidence = .8
#' )
#' r
#' inspect(head(r, by = "lift"))
#'
#' # mine closed itemsets with Carpenter or IsTa
#' fim4r(Adult,
#'   method = "carpenter",
#'   target = "closed", support = .7
#' )
#' fim4r(Adult,
#'   method = "ista",
#'   target = "closed", support = .7
#' )
#'
#' # mine frequent itemset of length 2 (zmin and zmax = 2)
#' freq_2 <- fim4r(Adult,
#'   method = "relim", target = "frequent", support = .7,
#'   zmin = 2, zmax = 2
#' )
#' inspect(freq_2)
#'
#' # mine maximal frequent itemsets
#' mfis <- fim4r(Adult, method = "sam", target = "maximal", support = .7)
#' inspect(mfis)
#'
#' # Examples for how to use item appearance with apriori, eclat,
#' #   fpgrowth in fim4r. We first mine all rules.
#' inspect(fim4r(Adult,
#'   method = "fpgrowth",
#'   target = "rules", support = .8
#' ))
#'
#' # ignore item "capital-gain=None"
#' inspect(fim4r(Adult,
#'   method = "fpgrowth",
#'   target = "rules", support = .8,
#'   appear = list(c("capital-gain=None"), c("-"))
#' ))
#'
#' # "capital-gain=None" cannot appear in consequent (antecedent only)
#' inspect(fim4r(Adult,
#'   method = "fpgrowth",
#'   target = "rules", support = .8,
#'   appear = list(c("capital-gain=None"), c("a"))
#' ))
#'
#' # "capital-gain=None" cannot appear in the antecedent
#' inspect(fim4r(Adult,
#'   method = "fpgrowth",
#'   target = "rules", support = .8,
#'   appear = list(c("capital-gain=None"), c("c"))
#' ))
#'
#' # restrict the consequent to the item "capital-gain=None".
#' # That is, "" = all items can only appear in the antecedent with the
#' # exception that "capital-gain=None" can only appear in the consequent.
#' inspect(fim4r(Adult,
#'   method = "fpgrowth",
#'   target = "rules", support = .8,
#'   appear = list(c("", "capital-gain=None"), c("a", "c"))
#' ))
#' }
fim4r <-
  function(
      transactions,
      method = NULL,
      target = "frequent",
      support = .1,
      confidence = .8,
      originalSupport = TRUE,
      appear = NULL,
      report = NULL,
      verbose = TRUE,
      ...) {
    # fim4r_url <- "https://borgelt.net/src/fim4r_1.7.tar.gz"
    fim4r_url <-
      "https://mhahsler.github.io/arules/docs/fim4r/fim4r_latest.tar.gz"

    if (!check_installed("fim4r", action = "check")) {
      question <-
        "Package fim4r is required.\nDownload and install the package?"
      cat(question, sep = "")
      if (utils::menu(c("Yes", "No")) != 1) {
        invokeRestart("abort")
      }
      utils::install.packages(fim4r_url, repos = NULL)
    }

    methods_itemsets <-
      c(
        "carpenter",
        "ista",
        "relim",
        "sam"
      )

    # these use conf
    methods_rules <-
      c(
        "apriori",
        "eclat",
        "fpgrowth"
      )

    methods <- c(methods_rules, methods_itemsets)

    if (is.null(method)) {
      cat(
        "Available methods in fim4r are:",
        paste(sQuote(methods), collapse = ", ")
      )
      return(invisible(NULL))
    }

    method_id <- pmatch(tolower(method), methods)

    if (is.na(method_id)) {
      stop(
        "Unknown method: ",
        sQuote(method),
        "\n Available methods in fim4r are: ",
        paste(sQuote(methods), collapse = ", ")
      )
    }

    method <- methods[method_id]

    if (!is.null(report)) {
      stop("Argument 'report' cannot be used with this interface!")
    }

    # targets: "frequent", "closed", "maximal", "generators", "rules"
    if (!is.na(pmatch("r", target)) &&
      is.na(pmatch(method, methods_rules))) {
      stop(
        method,
        " cannot be used to mine target rules. \nOnly methods ",
        paste(methods_rules, collapse = ", "),
        " can mine rules directly."
      )
    }

    # prepare data
    transactions <- transactions(transactions)
    tracts <- LIST(transactions, decode = FALSE)

    # fix supp to support count and conf to scale [0, 100]
    # Note: the man page for fim4r:: is wrong! support is not the support
    # Note: fim4r has a bug and returns elements with support < supp so we have to filter it manually!
    supp <- support * 100
    conf <- confidence * 100

    # translate appear to IDs
    if (!is.null(appear)) {
      if (!is.list(appear) ||
        length(appear) != 2 ||
        length(appear[[1]]) != length(appear[[2]])) {
        stop(
          "'appear' needs to contain a list with two elements specifying a vector of items and a vector of appearance specifiers. See ? fim4r::fim4r"
        )
      }

      if (is.numeric(appear[[1]])) {
        appear[[1]] <- as.integer(appear[[1]])
      } else {
        # "" means all items in fim4r. Translate to -1L
        appearID <- pmatch(appear[[1]], itemLabels(transactions), duplicates.ok = TRUE)
        appearID[appear[[1]] == ""] <- -1L
        if (any(is.na(appearID))) {
          stop(
            "The following item labels cannot be matched: ",
            paste(sQuote(appear[[1]][is.na(appearID)]), collapse = ", ")
          )
        }
        appear[[1]] <- appearID
      }
    }

    args <- c(
      list(target = target, report = "scl"),
      list(...)
    )

    # appear messed up verbose output so we add it later
    if (!is.null(appear)) {
      args$appear <- "specified"
    }

    if (method %in% methods_rules) {
      args <- c(list(supp = supp, conf = conf), args)
    } else {
      args <- c(list(supp = supp), args)
    }

    fun <- paste0("fim4r.", method)

    if (verbose) {
      cat(fun, "\n\nParameter specification:\n")
      print(data.frame(args, row.names = ""))
      cat("\n")
      cat("Data size:", paste(dim(transactions), c("transactions", "items"), collapse = " and "), "\n")
    }

    # appear messed up verbose output so we add it now
    if (!is.null(appear)) {
      args$appear <- appear
    }

    res <-
      do.call(utils::getFromNamespace(fun, "fim4r"),
        args = c(list(tracts = tracts), args)
      )

    # handle no rules/itemsets
    if (length(res) == 0) {
      nosets <- encode(list(), itemLabels = itemLabels(transactions))
      if (!is.na(pmatch("r", target))) {
        if (verbose) {
          cat("Result: 0 rules\n")
        }
        return(rules(lhs = nosets, rhs = nosets))
      } else {
        if (verbose) {
          cat("Result: 0 itemsets\n")
        }
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

      r <- rules(
        lhs = lhs,
        rhs = rhs,
        quality = qual
      )

      # fim4r uses LHS support so we need to filter
      if (originalSupport) {
        r <- r[quality(r)$support >= support]
      }

      if (verbose) {
        cat("Result:", length(r), "rules\n")
      }

      return(r)
    } else {
      # for itemsets
      items <- lapply(res, "[[", 1)
      items <-
        encode(items, itemLabels = itemLabels(transactions))
      qual <-
        do.call(rbind.data.frame, lapply(res, "[[", 2))[, 1, drop = FALSE]
      qual <-
        cbind(qual, as.integer(qual[[1]] * length(transactions)))
      colnames(qual) <- c("support", "count")

      if (verbose) {
        cat("Result:", length(items), "itemsets\n")
      }

      return(itemsets(items = items, quality = qual))
    }
  }
