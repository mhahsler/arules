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





#' Simulate a Random Transactions
#'
#' Simulate random [transactions] using different methods.
#'
#' Currently two simulation methods are implemented:
#'
#' * `"independent"` (Hahsler et al, 2006): All items
#'   are treated as independent. The transaction size is determined by
#'   `rpois(lambda - 1) + 1`, where `lambda` can be specified (defaults to 3).
#'   Note that one subtracted from lambda and added to the size to avoid
#'   empty transactions. The items in the transactions are randomly chosen using
#'   the numeric probability vector `iProb` of length `nItems`
#'   (default: 0.01 for each item).
#'
#' * `"agrawal"` (see Agrawal and Srikant, 1994): This
#'   method creates transactions with correlated items using `random.patters()`.
#'   The simulation is a two-stage process. First, a set of `nPats` patterns
#'   (potential maximal frequent itemsets) is generated.  The length of the
#'   patterns is Poisson distributed with mean `lPats` and consecutive
#'   patterns share some items controlled by the correlation parameter
#'   `corr`.  For later use, for each pattern a pattern weight is generated
#'   by drawing from an exponential distribution with a mean of 1 and a
#'   corruption level is chosen from a normal distribution with mean `cmean`
#'   and variance `cvar`.
#'   The function returns the patterns as an `itemsets` objects which can be
#'   supplied to `random.transactions()` as the argument `patterns`.  If
#'   no argument `patterns` is supplied, the default values given above are
#'   used.
#'
#'   In the second step, the transactions are generated using the patterns.  The
#'   length the transactions follows a Poisson distribution with mean
#'   `lPats`. For each transaction, patterns are randomly chosen using the
#'   pattern weights till the transaction length is reached. For each chosen
#'   pattern, the associated corruption level is used to drop some items before
#'   adding the pattern to the transaction.
#'
#' @family itemMatrix and transactions functions
#'
#' @param nItems an integer. Number of items to simulate
#' @param nTrans an integer. Number of transactions to simulate
#' @param method name of the simulation method used (see Details Section).
#' @param ... further arguments used for the specific simulation method (see
#' details).
#' @param verbose report progress?
#' @param nPats number of patterns (potential maximal frequent itemsets) used.
#' @param lPats average length of patterns.
#' @param corr correlation between consecutive patterns.
#' @param cmean mean of  the corruption level (normal distribution).
#' @param cvar variance of the corruption level.
#' @param iWeight item selection weights to build patterns.
#'
#' @return Returns a `ntrans x nitems` [transactions] object.
#' @author Michael Hahsler
#' @references Michael Hahsler, Kurt Hornik, and Thomas Reutterer (2006).
#' Implications of probabilistic data modeling for mining association rules. In
#' M. Spiliopoulou, R. Kruse, C. Borgelt, A. Nuernberger, and W. Gaul, editors,
#' _From Data and Information Analysis to Knowledge Engineering, Studies
#' in Classification, Data Analysis, and Knowledge Organization_, pages
#' 598--605. Springer-Verlag.
#'
#' Rakesh Agrawal and Ramakrishnan Srikant (1994). Fast algorithms for mining
#' association rules in large databases. In Jorge B. Bocca, Matthias Jarke, and
#' Carlo Zaniolo, editors, _Proceedings of the 20th International
#' Conference on Very Large Data Bases, VLDB_, pages 487--499, Santiago, Chile.
#' @keywords datagen
#' @examples
#' ## generate random 1000 transactions for 200 items with
#' ## a success probability decreasing from 0.2 to 0.0001
#' ## using the method described in Hahsler et al. (2006).
#' trans <- random.transactions(
#'   nItems = 200, nTrans = 1000,
#'   lambda = 5, iProb = seq(0.2, 0.0001, length.out = 200)
#' )
#'
#' ## size distribution
#' summary(size(trans))
#'
#' ## display random data set
#' image(trans)
#'
#' ## use the method by Agrawal and Srikant (1994) to simulate transactions
#' ## which contains correlated items. This should create data similar to
#' ## T10I4D100K (we just create 100 transactions here to speed things up).
#' patterns <- random.patterns(nItems = 1000)
#' summary(patterns)
#'
#' trans2 <- random.transactions(
#'   nItems = 1000, nTrans = 100,
#'   method = "agrawal", patterns = patterns
#' )
#' image(trans2)
#'
#' ## plot data with items ordered by item frequency
#' image(trans2[, order(itemFrequency(trans2), decreasing = TRUE)])
random.transactions <- function(
    nItems,
    nTrans,
    method = "independent",
    ...,
    verbose = FALSE) {
  builtin_methods <- c("independent", "agrawal", "dependent")

  if (is.na(ind <- pmatch(
    tolower(method),
    tolower(builtin_methods)
  ))) {
    stop(gettextf(
      "Value '%s' is not a valid method.",
      method
    ), domain = NA)
  }

  if (ind == 1) {
    # method == independent
    return(
      .random.transactions_independent(
        nItems = nItems,
        nTrans = nTrans,
        ...,
        verbose = verbose
      )
    )
  }

  if (ind == 2) {
    # method == agrawal
    return(
      .random.transactions_agrawal(
        nItems = nItems,
        nTrans = nTrans,
        ...,
        verbose = verbose
      )
    )
  }

  if (ind == 3) {
    # method == dependent
    return(
      .random.transactions_dependent(
        nItems = nItems,
        nTrans = nTrans,
        ...,
        verbose = verbose
      )
    )
  }
}


## method by Hahsler, Hornik, Reutterer
## Each item
## has the same success probability (iProb) to be part
## of a transactions.

.random.transactions_independent <- function(
    nItems,
    nTrans,
    lambda = 3,
    iProb = 0.01,
    verbose = FALSE) {
  ## check iProb
  if (any(iProb > 1 |
    iProb < 0)) {
    stop("Illegal probability given (>1 or <0)!")
  }

  ## case if only one value for all items is given
  if (length(iProb) == 1) {
    iProb <- rep(iProb, nItems)
  }

  ## check length of iProb
  if (length(iProb) != nItems) {
    stop("Number of items and number of given probabilities do not match!")
  }

  ## simulate data (create a list with indices)
  simList <- replicate(
    nTrans,
    sample(1:nItems,
      min(stats::rpois(1, lambda - 1) + 1L, nItems),
      prob = iProb
    )
  )
  #  which(stats::runif(nItems) <= iProb))

  ## avoid empty transactions

  new("transactions",
    encode(simList, paste("item", 1:nItems, sep = "")),
    itemsetInfo = data.frame(
      transactionID =
        paste("trans", 1:nTrans, sep = "")
    )
  )
}


## new method by Hahsler uses patterns generated with the method by
## Arawal and Srikant

.random.transactions_dependent <- function(
    nItems,
    nTrans,
    iProb = 0.01,
    # lTrans = 2,
    patterns = NULL,
    ...,
    verbose = FALSE) {
  ## figure out what iWeight to use for pattern generation
  ## all items have the same weight
  if (length(iProb) == 1) {
    iWeight <- rep(1 / nItems, nItems)
  } ## normalize as a weight.
  else {
    iWeight <- iProb / sum(iProb)
  }

  if (is.null(patterns)) {
    patterns <-
      random.patterns(
        nItems = nItems,
        method = "agrawal",
        iWeight = iWeight,
        ...,
        verbose = verbose
      )
  }

  ## number of patterns to use is Poisson distr.
  # lTrans <- stats::rpois(nTrans, lTrans)

  ## every transaction gets for sure one pattern
  lTrans <- rep(1, nTrans)

  ## get info from patterns
  pWeights <- quality(patterns)$pWeights
  pCorrupts <- quality(patterns)$pCorrupts

  ## not used
  # nPatterns <- length(patterns)
  patterns <- LIST(items(patterns), decode = FALSE)


  ## corrupt the pattern with index i
  corrPattern <- function(i) {
    if (pCorrupts[i] >= 1) {
      return(numeric(0))
    }

    patternToAdd <- patterns[[i]]
    patLen <- length(patternToAdd)
    # while (stats::runif(1) < pCorrupts[i] && patLen > 0) patLen <- patLen -1
    ## faster
    if (pCorrupts[i] > 0) {
      patLen <- patLen - stats::rgeom(1, 1 - pCorrupts[i])
    }

    if (patLen < 1) {
      return(numeric(0))
    }

    return(patternToAdd[sample(seq_len(length(patternToAdd)), patLen)])
  }


  ## choose n patterns, corrupt them and return the resulting vector of items
  choosePatterns <- function(n) {
    if (n == 0) {
      return(numeric(0))
    }

    take <-
      sample(seq_len(length(patterns)), n, prob = pWeights)
    trans <- c()
    for (i in 1:n) {
      trans <- c(trans, corrPattern(take[i]))
    }

    if (length(trans) == 0) {
      return(numeric(0))
    }

    sort(unique(trans))
  }

  ## create dependent items from patterns
  transactions <- list()
  for (i in 1:nTrans) {
    if (verbose) {
      cat("creating transaction", i, "\n")
    }
    transactions[[i]] <- choosePatterns(lTrans[i])
  }

  ## create independent noise
  noise <- .random.transactions_independent(
    nItems = nItems,
    nTrans = nTrans,
    iProb = iProb,
    verbose = verbose
  )
  noise <- LIST(noise, decode = FALSE)

  ## merge dependent items with noise
  tn <- list()
  for (i in 1:nTrans) {
    tn[[i]] <- sort(union(transactions[[i]], noise[[i]]))
  }

  new("transactions",
    encode(tn, paste("item", 1:nItems, sep = "")),
    itemsetInfo = data.frame(
      transactionID =
        paste("trans", 1:nTrans, sep = "")
    )
  )
}



##******************************************************************
## R Implementation of the IBM Quest transaction data generator
##
## described in R. Agrawal, R. Srikant, "Fast Algorithms for Mining
## 	Association Rules," Procs. 20th int'l Conf. Very Large DB, 1994
##

##
## nTrans ... number of transactions
## lTrans ... agv. length of transactions
##
## nItems ... number of items
##
## nPats  ... number of patterns (potential maximal frequent itemsets)
## lPats  ... avg. length of patterns
##
## corr   ... correlation between consecutive patterns
## cmean  ... mean of the corruption level (norm distr.)
## cvar   ... variance of the corruption level
##
##
## the length the transactions and patterns (potential maximal frequ. itemsets,
## 	PMFIs) follow a Poisson distribution with mean ltans and lPats
##
## the weights of the patterns are chosen from a exponential distribution with
## 	a mean of 1
##
## corr (chance of an item in a pattern to be also in the next pattern)
## 	is set by default to 0.5

#' @rdname random.transactions
random.patterns <- function(
    nItems,
    nPats = 2000,
    method = NULL,
    # method is unused for now
    lPats = 4,
    corr = 0.5,
    cmean = 0.5,
    cvar = 0.1,
    iWeight = NULL,
    verbose = FALSE) {
  ## iWeight are used for item selection to build PMFIs
  ## the original implementation used exponential weights (the default here).
  if (is.null(iWeight)) {
    iWeight <- stats::rexp(nItems, rate = 1)
  }
  iWeight <- iWeight / sum(iWeight)


  ## pattern lengths (we want no empty patterns)
  ## that's how they for it in the code to get no 0 lenght patterns
  pLengths <- stats::rpois(nPats, lPats - 1) + 1

  ## pattern weights (weights need to sum up to 1)
  pWeights <- stats::rexp(nPats, rate = 1)
  pWeights <- pWeights / sum(pWeights)

  ## corruption levels (cannot be neg.)
  pCorrupts <-
    stats::rnorm(nPats, mean = cmean, sd = sqrt(cvar))
  pCorrupts[pCorrupts < 0] <- 0
  pCorrupts[pCorrupts > 1] <- 1

  ## PMFIs
  patterns <- list()

  for (i in 1:nPats) {
    if (verbose) {
      cat("Creating pattern #", i, "\n")
    }
    pattern <- c()

    if (i > 1) {
      ## correlation: take some items from the previous pattern
      ## in the paper they say the mean of the exp dist. is corr but
      ## in the implementation they used 1 in the following way:
      nTake <-
        min(c(
          trunc(pLengths[i] * corr * stats::rexp(1, rate = 1) + 0.5),
          pLengths[i - 1],
          pLengths[i]
        ))

      if (nTake > 0) {
        take <- sample(1:pLengths[i - 1], nTake)
        pattern <- patterns[[i - 1]][take]
      }
    }

    ## fill rest random items using iWeight
    if (is.null(pattern)) {
      take <- sample(c(1:nItems),
        pLengths[i],
        prob = iWeight
      )
    } else {
      take <- sample(c(1:nItems)[-pattern],
        pLengths[i] - length(pattern),
        prob = iWeight[-pattern]
      )
    }

    pattern <- sort(c(pattern, take))
    patterns[[i]] <- pattern
  }

  ## create itemMatrix w/o recoding
  new(
    "itemsets",
    items = encode(
      patterns,
      paste("item", as.character(1:nItems), sep = "")
    ),
    quality = data.frame(pWeights = pWeights, pCorrupts = pCorrupts)
  )
}


## create transactions
.random.transactions_agrawal <- function(
    nItems,
    nTrans,
    lTrans = 10,
    patterns = NULL,
    ...,
    verbose = FALSE) {
  if (is.null(patterns)) {
    patterns <-
      random.patterns(
        nItems = nItems,
        method = "agrawal",
        ...,
        verbose = verbose
      )
  }


  if (!is.null(nItems) && nItems != nitems(items(patterns))) {
    stop("nItems in patterns and the given nItems do not match!\n")
  }


  ## get patterns and weights form arg
  pWeights <- quality(patterns)$pWeights
  pCorrupts <- quality(patterns)$pCorrupts
  nPatterns <- length(patterns)
  patterns <- LIST(items(patterns), decode = FALSE)

  ## transaction lengths
  ## that's how AS do it to get no transaction of length 0
  tLengths <- stats::rpois(nTrans, lTrans - 1) + 1


  ## transactions
  transactions <- list()


  for (i in 1:nTrans) {
    if (verbose) {
      cat("creating transaction", i, "\n")
    }
    trans <- c()

    while (length(trans) < tLengths[i]) {
      ## choose pattern with weights
      j <- sample(1:nPatterns, 1, prob = pWeights)
      patternToAdd <- patterns[[j]]

      ## corrupting pattern
      ## corruption level is norm distr.
      if (pCorrupts[j] == 1) {
        next
      }

      patLen <- length(patternToAdd)

      ## while (stats::runif(1) < pCorrupts[j] && patLen > 0) patLen <- patLen -1
      ## do it the fast way -- results in a geometric distribution
      if (pCorrupts[j] > 0) {
        patLen <- patLen - stats::rgeom(1, 1 - pCorrupts[j])
        if (patLen < 1) {
          next
        }
      }

      ## get out 50% of the cases if transaction would be overfull
      ## we depart from AS by not allowing to generate empty transactions
      if (length(trans) > 0 &&
        (length(trans) + patLen) > tLengths[i] &&
        stats::runif(1) > 0.5) {
        break
      }

      ## pick the items and add them to the transactions
      patternToAdd <-
        patternToAdd[sample(seq_len(length(patternToAdd)), patLen)]
      trans <- unique(sort(c(trans, patternToAdd)))
    }
    transactions[[i]] <- trans
  }

  new(
    "transactions",
    encode(transactions, paste("item", 1:nItems, sep = "")),
    itemsetInfo = data.frame(
      transactionID =
        paste("trans", 1:nTrans, sep = "")
    )
  )
}
