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
## Functions for calculating confidence intervals for interest measures
##

confint.rules <- function(object,
  parm = "oddsRatio",
  level = 0.95,
  smoothCounts = 0,
  transactions = NULL,
  reuse = TRUE,
  ...) {
  measures <- c("count",
    "confidence",
    "confidence_normal",
    "lift",
    "oddsRatio",
    "oddsRatio_normal",
    "support",
    "support_normal"
    )
  
  addl <- list(...)
  if (length(addl) > 0L)
    warning("additional parameters ignored: ",
      paste(names(addl), collapse = ", "))
  
  measure <- match.arg(parm, measures)
  
  if (level < 0 ||
      level > 1)
    stop("the confidence level needs to be in [0, 1].")
  
  counts <-
    .getCounts(object, transactions = transactions, reuse = reuse, smoothCounts = smoothCounts)
  
  # Poisson distribution for counts
  if (measure %in% c("count", "support")) {
    ci <- with(counts,
      data.frame(
        lowerLimit = stats::qpois((1 - level) / 2, n11, lower.tail = TRUE),
        upperLimit = stats::qpois((1 - level) / 2, n11, lower.tail = FALSE)
      ))
    
    if (measure == "support")
      ci <- ci / counts$n
    
    return(ci)
  }
  
  # Normal approximation
  if (measure == "support_normal") {
    z <- stats::qnorm(1 - (1 - level) / 2)
    p11 <- with(counts, n11 / n)
    w <- z * sqrt((1 - p11) / counts$n)
    
    return(data.frame(lowerLimit = p11 - w,
      upperLimit = p11 + w))
  }
  
  # Binomial proportion confidence interval (Clopper and Pearson)
  # Note: can be done faster using stats::qbinom
  if (measure == "confidence") {
    cnts <- with(counts, data.frame(n11, n1x))
    
    ci <- apply(
      cnts,
      MARGIN = 1,
      FUN = function(ns)
        stats::binom.test(ns["n11"], ns["n1x"] , conf.level = level)$conf.int
    )
    
    return(data.frame(lowerLimit = ci[1,],
      upperLimit = ci[2,]))
  }
  
  # Normal approximation: N(np, np(1-p))
  if (measure == "confidence_normal") {
    z <- stats::qnorm(1 - (1 - level) / 2)
    n <- counts$n1x
    p <- counts$n11 / counts$n1x
    np <- n * p
    
     
    w <- np*(1-p) / sqrt(n) 
    
    return(data.frame(lowerLimit = (np - w) / n,
      upperLimit = (np + w) / n))
  }
  
  
  # FIXME: currently the confidence interval for lift is just the confidence divided by px1
  if (measure == "lift") {
    warning("The confidence interval for lift is currently not correcly implemented and just returns the confidence interval for confidence divided by supp(Y).")
    cnts <- with(counts, data.frame(n11, n10, n1x, px1 = nx1 / n))
    
    ci <- apply(
      cnts,
      MARGIN = 1,
      FUN = function(ns)
        stats::binom.test(ns["n11"], ns["n1x"], conf.level = level)$conf.int / ns["px1"]
    )
    
    return(data.frame(lowerLimit = ci[1,],
      upperLimit = ci[2,]))
  }
  
  # Exact Fisher's confidence interval
  if (measure == "oddsRatio") {
    cnts <- with(counts, data.frame(n11, n01, n10, n00))
    # round up in case of a non-integer smoothCounts value
    cnts <- ceiling(cnts)
     
    ci <- apply(
      cnts,
      MARGIN = 1,
      FUN = function(ns)
        stats::fisher.test(matrix(ns, ncol = 2), conf.level = level)$conf.int
    )
    
    return(data.frame(lowerLimit = ci[1,],
      upperLimit = ci[2,]))
  }
  
  # normal approximation
  if (measure == "oddsRatio_normal") {
    or <- with(counts, n11 * n00 / (n01 * n10))
    z <- stats::qnorm(1 - (1 - level) / 2)
    
    w <- with(counts,
       z * sqrt(1 / n11 + 1 / n01 + 1 /
          n10 + 1 / n00))
    
    return(data.frame(
      lowerLimit = or * exp(-1 * w),
      upperLimit = or * exp(w)
    ))
  }
  
  stop("Measure not implemented!")
}
