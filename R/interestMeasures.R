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



##*******************************************************
## Functions for additional interest measures
##


## measures for itemsets

setMethod("interestMeasure",  signature(x = "itemsets"),
  function(x, measure, transactions = NULL, reuse = TRUE, ...) {
    
    ## backward comp.
    add <- list(...)
    if(!is.null(add$method)) {
      warning("interestMeasure: parameter method is now deprecated! Use measure instead!")
      measure <- add$method
    }
    
    builtin_measures <- c("support", 
      "allConfidence", "crossSupportRatio", "lift")
    
    if(missing(measure)) measure <- builtin_measures
    
    ## check and expand measure
    if(any(is.na(ind <- pmatch(tolower(measure),
      tolower(builtin_measures)))))
      stop(gettextf("Value '%s' is an invalid measure for itemsets.", 
        measure[is.na(ind)][1]), domain = NA)
    
    measure <- builtin_measures[ind]
   
    ## deal with multiple measures
    if(length(measure) > 1) return(as.data.frame(sapply(measure, FUN = 
        function(m) interestMeasure(x, m, transactions, reuse, ...))))
    
    ## first see if we already have it:
    if(reuse && !is.null(quality(x)[[measure]])) return(quality(x)[[measure]])
    
    ## calculate measures
    if(measure == "support") return(support(x, transactions))
   
    ## all other measures are basic measures
    return(.basicItemsetMeasures(x, measure, transactions, reuse, ...))
  })



.basicItemsetMeasures <- function(x, measure, transactions = NULL, 
  reuse = TRUE, ...) {
  
  if(is.null(transactions)) stop("transaction data needed. Please specify the transactions used to mine the itemsets!")
  
  itemSupport <- itemFrequency(transactions)  
  if (length(itemSupport) != nitems(items(x))) 
    stop("number of items in itemsets and transactions do not match.")
  
  ## create an itemset list
  itemset_list <- LIST(items(x), decode = FALSE)
  
  ## calculate all-confidence for existing (frequent) itemsets.
  ##
  ## Edward R. Omiecinski. Alternative interest measures for mining 
  ## associations in databases. IEEE Transactions on Knowledge and 
  ## Data Engineering, 15(1):57-69, Jan/Feb 2003.
  ##
  ## calculate all-confidence using itemsets support and the 
  ## singleton support of the most frequent item in the itemset
  ## all-confidence(Z) = supp(Z) / max(supp(i elem Z))
  
  if(measure == "allConfidence") {
    supp <- interestMeasure(x, "support", transactions, reuse)
    m <- supp / sapply(itemset_list, function(i) max(itemSupport[i]))
  
    ### deal with 1-itemsets
    is1 <- size(x)==1
    m[is1] <- supp[is1]  
  }
  
  ## calculate the cross-support ratio 
  ## used to eliminate cross support patterns which contain item with 
  ## extremely differnt support. These patterns tend to be spurious 
  ## (i.e., one item which occurs in virtually all transactions and some very 
  ##  rare items)
  ##
  ## Hui Xiong, Pang-Ning Tan, Vipin Kumar. Mining Strong Affinity Association 
  ## Patterns in Data Sets with Skewed Support. Third IEEE International
  ## Conference on Data Mining, Melbourne, Florida, November 19 - 22, 2003.
  ##
  
  if(measure == "crossSupportRatio")
    m <-  
    sapply(itemset_list, function(i) min(itemSupport[i])) /
    sapply(itemset_list, function(i) max(itemSupport[i]))
  
  if(measure == "lift")
    m <- interestMeasure(x, "support", transactions, reuse) /
    sapply(itemset_list, function(i) prod(itemSupport[i]))

  m[!is.finite(m)] <- NA
  
  return(m)
}

## measures for rules

setMethod("interestMeasure",  signature(x = "rules"),
  function(x, measure, transactions = NULL, reuse = TRUE, ...) {
   
    ## backward comp.
    add <- list(...)
    if(!is.null(add$method)) {
      warning("interestMeasure: parameter method is now deprecated! Use measure instead!")
      measure <- add$method
    }
    
    builtin_measures <- c("support", "coverage", "confidence", "lift",
      "leverage", "hyperLift", "hyperConfidence", "fishersExactTest", 
      "improvement",
      "chiSquared", "cosine", "conviction", "gini", "oddsRatio", "phi",
      "doc", "RLD", "imbalance", "kulczynski", "collectiveStrength",
      "jaccard", "kappa",
      "mutualInformation", "lambda", "jMeasure", "laplace",
      "certainty", "addedValue",
      
      ### FIXME: add to man page!
      "ralambrodrainy",
      "descriptiveConfirm",
      "confirmedConfidence",
      "sebag",
      "counterexample",
      "casualSupport",
      "casualConfidence",
      "leastContradiction",
      "centeredConfidence",
      "varyingLiaison",
      "yuleQ",
      "yuleY",
      "lerman",
      "implicationIndex"
    )
    
    if(missing(measure)) measure <- builtin_measures
    
    ## check and expand measure
    if(any(is.na(ind <- pmatch(tolower(measure),
      tolower(builtin_measures)))))
      stop(gettextf("Value '%s' is an invalid measure for rules.", 
        measure[is.na(ind)][1]), domain = NA)
    
    measure <- builtin_measures[ind]
   
    ## deal with multiple measures
    if(length(measure) > 1) return(as.data.frame(sapply(measure, FUN = 
        function(m) interestMeasure(x, m, transactions, reuse, ...))))
    
    ## first see if we already have it:
    if(reuse && !is.null(quality(x)[[measure]])) return(quality(x)[[measure]])
    
    ## calculate measure 
    if(measure == "support") return(support(generatingItemsets(x), 
      transactions, type = "relative"))
    if(measure == "coverage") return(coverage(x, transactions, reuse))
    if(measure == "confidence") return(
      interestMeasure(x, "support", transactions, reuse)/
        interestMeasure(x, "coverage", transactions, reuse))
    if(measure == "lift") return(
      interestMeasure(x, "support", transactions, reuse)/
        (interestMeasure(x, "coverage", transactions, reuse) 
          * support(rhs(x), transactions)))
    if(measure == "improvement") return(.improvement(x, transactions, reuse))
    if(measure == "hyperLift") return(
      .hyperLift(x, transactions, reuse, ...))
    if(measure == "hyperConfidence") return(
      .hyperConfidence(x, transactions, reuse, ...))
    if(measure == "fishersExactTest") return(
      .hyperConfidence(x, transactions, reuse, significance=TRUE, ...))
    if(measure == "RLD") return(.RLD(x, transactions, reuse))
    if(measure == "imbalance") return(.imbalance(x, transactions, reuse))
    if(measure == "kulczynski") return(.kulc(x, transactions, reuse))
    
    
    ## all other measures are implemented here (counts is in ...)
    ret <- .basicRuleMeasure(x, measure, 
      counts = .getCounts(x, transactions, reuse), ...)
    
    ## make all bad values NA
    ret[!is.finite(ret)] <- NA
    return(ret)
    
    stop("Specified measure not implemented.")
  })


## calculate hyperlift for existing rules.
##
## Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
## Implications of probabilistic data modeling for rule mining. 
## Report 14, Research Report Series, Department of Statistics and 
## Mathematics, Wirtschaftsuniversitaet Wien, Augasse 2-6, 1090 Wien, 
## Austria, March 2005.

## hyperlift(X => Y) = c_X,Y / Q_d[C_X,Y] 
##
## where Q_d[C_X,Y] = qhyper(d, m = c_Y, n = length(trans.) - c_Y, k = c_X)
##
## c_X,Y = count(X => Y)
## c_X = count(X)
## c_Y = count(Y)
##
## this implements only hyperlift for rules with a single item in the consequent


.hyperLift <- function(x, transactions, reuse, d = 0.99, ...) {
  
  counts <- .getCounts(x, transactions, reuse)
  
  t <- counts$N
  c_XY <- counts$f11
  c_X <- counts$f1x
  c_Y <- counts$fx1
  t <- length(transactions)
  
  Q <- stats::qhyper(d, m = c_Y, n = t - c_Y, k = c_X, lower.tail = TRUE)
  hyperlift <- c_XY / Q
  
  hyperlift
}


## calculate hyperconfidence for existing rules.
## (confidence level that we observe too high/low counts)
## 
## uses the model from:
## Michael Hahsler, Kurt Hornik, and Thomas Reutterer. 
## Implications of probabilistic data modeling for rule mining. 
## Report 14, Research Report Series, Department of Statistics and 
## Mathematics, Wirtschaftsuniversitaet Wien, Augasse 2-6, 1090 Wien, 
## Austria, March 2005.


.hyperConfidence <- function(x, transactions, reuse = TRUE, complements = TRUE, 
  significance = FALSE, ...) {
  
  ## significance: return significance levels instead of
  ##   confidence levels
  
  counts <- .getCounts(x, transactions, reuse)
  
  t <- counts$N
  c_XY <- counts$f11
  c_X <- counts$f1x
  c_Y <- counts$fx1
  
  if(complements == TRUE)
    ## c_XY - 1 so we get P[C_XY < c_XY] instead of P[C_XY <= c_XY]
    res <- stats::phyper(c_XY - 1, m=c_Y, n=t-c_Y, k=c_X, lower.tail = !significance)
  
  else
    ## substitutes; Pr[C_XY > c_XY]
    res <- stats::phyper(c_XY, m=c_Y, n=t-c_X, k=c_X, lower.tail = significance)
  
  ## TODO: check resulting NaN
  res
}

## Minimum Improvement (Bayardo et al. 1999)
## Let the improvement of a rule be defined as the minimum
## difference between its confidence and the confidence of any
## proper sub-rule with the same consequent.

.improvement <- function(x, transactions = NULL, reuse = TRUE, 
  measure = "confidence") {
  
  ## Note: improvement is defined for confidence, but could also used with 
  ## other measures
  conf <- interestMeasure(x, measure, transactions, reuse)
  #conf <- quality(x)$confidence
  imp <- numeric(length(x))
  
  ## find sets of rules w/the same rhs 
  rhsList <- LIST(rhs(x), decode = FALSE)
  rhsUnique <- unique(rhsList)
  
  for(i in rhsUnique) {
    sameRhs <- which(is.element(rhsList, i))
    lhsSameRhs <- lhs(x)[sameRhs]
    
    for(j in sameRhs) {
      ## find subsets
      subRules <- sameRhs[is.superset(lhs(x)[j], 
        lhsSameRhs, proper = TRUE)]
      
      ## calculate improvement
      imp[j] <- conf[j] - suppressWarnings(max(conf[subRules]))
    }
  }
  
  ### no improvement with missing smaller rule 
  imp[!is.finite(imp)] <- NA
  
  return(imp)
}

## count helpers
.getCounts <- function(x, transactions, reuse = TRUE){
  N <- length(transactions)
  f11 <- interestMeasure(x, "support", transactions, reuse) * N
  f1x <- interestMeasure(x, "coverage", transactions, reuse) * N
  fx1 <- .rhsSupport(x, transactions, reuse) * N
  f0x <- N - f1x
  fx0 <- N - fx1
  f10 <- f1x - f11
  f01 <- fx1 - f11
  f00 <- f0x - f01
  list(f11 = f11, f1x = f1x, fx1 = fx1, 
    f0x = f0x, fx0= fx0, 
    f10 = f10, f01 = f01, f00=f00, 
    N = N)
}

.rhsSupport <- function(x, transactions, reuse = TRUE){
  
  if(is.null(transactions)) stop("transactions missing. Please specify the data used to mine the rules as transactions!")
  
  #N <- length(transactions)
  
  q <- quality(x)
  if(reuse && !is.null(q$confidence) && !is.null(q$lift)) 
    rhsSupport <- q$confidence / q$lift
  else rhsSupport <- support(rhs(x), transactions)
  
  ## for consequents with only one item this might be faster
  ## cons <- unlist(LIST(rhs(x), decode = FALSE))
  ## that's low-level but way faster!
  #cons <- x@rhs@data@i+1
  ## check that the consequents are all singletons
  #if (length(cons) != length(x)) stop("this implementation only works for
  #    rules with one item in the rhs.")
  #c_Y <- itemFrequency(transactions, type = "absolute")[cons]
  #names(c_Y) <- NULL
  
  return(rhsSupport)
}


## more measures
## see Tan et al. Introduction to Data Mining, 2006

.basicRuleMeasure <- function(x, measure, counts, significance = FALSE, ...) {
  ### significance is only used by chi-squared
  
  N   <- counts$N
  f1x <- counts$f1x
  fx1 <- counts$fx1
  f11 <- counts$f11
  f0x <- counts$f0x 
  fx0 <- counts$fx0
  f10 <- counts$f10
  f01 <- counts$f01
  f00 <- counts$f00
  
  if(measure == "cosine") return(f11 / sqrt(f1x*fx1))
  if(measure == "conviction") return(f1x*fx0 /(N*f10))
  if(measure == "gini") return(
    f1x/N * ((f11/f1x)^2 + (f10/f1x)^2) - (fx1/N)^2 +
      f0x/N * ((f01/f0x)^2 + (f00/f0x)^2) - (fx0/N)^2)
  if(measure == "oddsRatio") return(f11*f00/(f10*f01))
  if(measure == "phi") return((N*f11-f1x*fx1) / sqrt(f1x*fx1*f0x*fx0))
  if(measure == "leverage") return(f11/N - (f1x*fx1/N^2))
  if(measure == "collectiveStrength") return(f11*f00/(f1x*fx1+f0x+fx0) * 
      (N^2 -f1x*fx1-f0x*fx0)/(N-f11-f00))
  if(measure == "jaccard") return(f11/(f1x+fx1-f11))
  if(measure == "kappa") return((N*f11+N*f00-f1x*fx1-f0x*fx0)/N^2-f1x*fx1-f0x*fx0)
  if(measure == "lambda") {
    max_x0x1 <- apply(cbind(fx1, fx0), 1, max)
    lambda <- (apply(cbind(f11, f10), 1, max) + apply(cbind(f01, f00), 1, max) -
        max_x0x1) / (N - max_x0x1)
    return(lambda)
  }
  if(measure == "mutualInformation") return(
    (f00/N * log(N*f00/(f0x*fx0)) +
        f01/N * log(N*f01/(f0x*fx1)) +
        f01/N * log(N*f01/(f0x*fx1)) +
        f11/N * log(N*f11/(f1x*fx1))) / 
      (f0x/N * log(f0x/N) + f1x/N * log(f1x/N)))
  if(measure == "jMeasure") return(f11/N * log(N*f11/(f1x*fx1)) + 
      f10/N * log(N*f10/(f1x*fx0)))
  if(measure == "laplace") return((f11 + 1)/(f1x + 2))
  if(measure == "certainty") return((f11/f1x - fx1/N)/(1 - fx1/N))
  if(measure == "addedValue") return(f11/f1x - fx1/N)
  if(measure == "ralambrodrainy") return(f10/N)
  if(measure == "descriptiveConfirm") return((f1x-2*f10)/N)
  if(measure == "sebag") return((f1x-f10)/f10)
  if(measure == "counterexample") return((f11-f10)/f11)
  # needs alpha
  #if(measure == "wang") return(1/N * (1-alpha) * f1x - f10)
  if(measure == "confirmedConfidence") return(1 - 2*f10/f1x)
  if(measure == "casualSupport") return((f1x+fx1-2*f10)/N)
  if(measure == "casualConfidence") return(1 - f10/N * (1/f1x + 1/fx1))
  if(measure == "leastContradiction") return((f1x - 2*f10)/fx1)
  if(measure == "centeredConfidence") return(fx0/N - f10/f1x)
  if(measure == "varyingLiaison") return((f1x-f10)/(f1x*fx1/N) - 1)
  if(measure == "yuleQ") {
    OR <- f11*f00/(f10*f01)
    return((OR-1)/(OR+1))
  }
  if(measure == "yuleY") {
    OR <- f11*f00/(f10*f01)
    return((sqrt(OR)-1)/(sqrt(OR)+1))
  }
  if(measure == "lerman") return((f11 - f1x*fx1/N)/sqrt(f1x*fx1/N))
  if(measure == "implicationIndex") return((f10 - f1x*fx0/N)/sqrt(f1x*fx0/N))
  
  ## difference in confidence (conf(X -> Y) - conf(not X -> Y))
  ## Heike Hofmann and Adalbert Wilhelm. Visual comparison of association 
  ## rules. Computational Statistics, 16(3):399-415, 2001.
  if(measure == "doc") return((f11/f1x)-(f01/f0x))
  
  
  ## this one is from Bing Liu, Wynne Hsu, and Yiming Ma (1999) 
  if(measure == "chiSquared") {
    
    chi2 <- c()
    for(i in 1:length(x)) {
      fo <- matrix(c(f00[i], f01[i], f10[i], f11[i]), ncol=2)
      fe <- tcrossprod(c(fx0[i], fx1[i]), c(f0x[i], f1x[i])) /N
      ## check if approximation is ok
      ## we don't do this now
      ##if(any(fe < 5)) chi2[i] <- NA
      ##else 
      chi2[i] <- sum((fo - fe)^2 / fe)
    }
    
    ## the chi square test has 1 df for a 2x2 contingency table. 
    ## The critical value at alpha=0.05 is:
    ## qchisq(0.05, df =1, lower.tail=FALSE)
    ## [1] 3.841459
    if(!significance) return(chi2)
    else return(stats::pchisq(q=chi2, df=1, lower.tail=FALSE))
  }
  
  stop("Specified measure not implemented.")
}


## RLD see Kenett and Salini 2008
## RLD code contributed by Silvia Salini
.RLD <- function(x, transactions, reuse = TRUE) {
  
  counts <- .getCounts(x, transactions, reuse)
  N   <- counts$N
  f11 <- counts$f11
  f10 <- counts$f10
  f01 <- counts$f01
  f00 <- counts$f00
  
  RLD <- numeric(length(x))
  for(i in 1:length(x)) {
    D <- (f11[i]*f00[i]-f10[i]*f01[i])/N
    if (D > 0) 
      if (f01[i] < f10[i]) RLD[i] <- D/(D+f01[i])
    else RLD[i] <- D/(D+f10[i])
    else 
      if (f11[i] < f00[i]) RLD[i] <- D/(D-f11[i])
    else RLD[i] <- D/(D-f00[i]) 
  }
  
  RLD[!is.finite(RLD)] <- NA
  
  RLD
}

## Imbalance ratio see Wu, Chen and J. Han 2010
# IR = abs(P(Y|X)-P(X|Y))/(P(Y|X)+P(X|Y) - P(Y|X)P(X|Y))
# IR = abs(supp(X)-supp(Y))/(supp(X)+supp(Y)-supp(X->Y)) 
.imbalance <- function(x, transactions, reuse = TRUE) {
  if(is.null(transactions)) stop("transactions missing. Please specify the data used to mine the rules as transactions!")
  
  XY <- interestMeasure(x, measure = "support", 
    transactions = transactions, reuse = reuse)
  
  ## lhs support
  X <- interestMeasure(x, measure = "coverage", 
    transactions = transactions, reuse = reuse)
  
  ## rhs support
  Y <- .rhsSupport(x, transactions = transactions, reuse = reuse)
  
  imbalance <- abs(X-Y)/(X+Y-XY)
  imbalance
}

## Kulczynski measure see Wu, Chen and Han (2007)
# Kulc = 1/2 (conf(X->Y) + conf(Y->X))
# Kulc = supp(X->Y)/2  (1/supp(X) + 1/supp(Y))
.kulc <- function(x, transactions, reuse = TRUE) {
  if(is.null(transactions)) stop("transactions missing. Please specify the data used to mine the rules as transactions!")

  XY <- interestMeasure(x, measure = "support", 
    transactions  = transactions, reuse = reuse)
  ## lhs support
  X <- interestMeasure(x, measure = "coverage", 
    transactions = transactions, reuse = reuse)
  ## rhs support
  Y <- .rhsSupport(x, transactions = transactions, reuse = reuse)
  
  kulc <- XY/2 * (1/X + 1/Y)
  kulc
}