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
  measure = NULL,
  method = "wald",
  smoothCounts = 0,
  transactions = NULL,
  reuse = TRUE,
  ...) {
  # measure can be used instead of parm
  if (!is.null(measure))
    parm <- measure
  
  measures <- c("count",
    "confidence",
    "lift",
    "oddsRatio",
    "phi",
    "support")
  addl <- list(...)
  if (length(addl) > 0L)
    warning("additional parameters ignored: ",
      paste(names(addl), collapse = ", "))
  
  measure <- match.arg(parm, choices = measures)
  
  if (level < 0 ||
      level > 1)
    stop("the confidence level needs to be in [0, 1].")
  z <- stats::qnorm((1 + level) / 2)
  
  ci.prop <- function(f, n, z) {
    p <- f / n
    se <- sqrt(p * (1 - p) / n)
    
    data.frame(lowerLimit = pmax(p - z * se, 0),
      upperLimit = pmin(p + z * se, 1))
  }
  
  ci.prop.binom <- Vectorize(function(f, n, level) stats::binom.test(f, n, conf.level = level)$conf.int)
  
  counts <-
    .getCounts(
      object,
      transactions = transactions,
      reuse = reuse,
      smoothCounts = smoothCounts
    )
  
  ci <- NULL
  desc <- NULL
  
  ###########################################################################
  if (measure == "count") {
    method <- match.arg(method, choices = c("wald", "exact"))
    
    if (method == "exact") {
      ci <- ci.prop.binom(counts[["n11"]], counts[["n"]], level) * counts[["n"]]
      ci <- data.frame(lowerLimit = ci[1, ], upperLimit = ci[2, ])
      desc <-
        "Exact binomial proportion confidence interval for support (Clopper and Pearson)."
      
    } else {
      ci <- ci.prop(counts[["n11"]], counts[["n"]], z) * counts[["n"]]
      desc <-
        "Adjusted Wald population proportion confidence interval for support."
    }
  }
  
  #####################################################3
  else if (measure == "support") {
    method <- match.arg(method, choices = c("wald", "exact"))
    
    if (method == "exact") {
      ci <- ci.prop.binom(counts[["n11"]], counts[["n"]], level)
      ci <- data.frame(lowerLimit = ci[1, ], upperLimit = ci[2, ])
      desc <-
        "Exact binomial proportion confidence interval for support (Clopper and Pearson)."
    }
    
    else if (method == "wald") {
      ci <- ci.prop(counts[["n11"]], counts[["n"]], z)
      desc <-
        "Adjusted Wald population proportion confidence interval for support."
    }
  }
  
  #####################################################3
  else if (measure == "confidence") {
    method <- match.arg(method, choices = c("wald", "wilson", "exact"))
    
    if (method == "exact") {
      ci <- ci.prop.binom(counts[["n11"]], counts[["n1x"]], level)
      ci <- data.frame(lowerLimit = ci[1, ], upperLimit = ci[2, ])
      desc <-
        "Exact binomial proportion confidence interval for confidence (Clopper and Pearson)."
    }
    
    else if (method  == "wald") {
      ci <- ci.prop(counts[["n11"]], counts[["n1x"]], z)
      desc <-
        "Wald population proportion confidence interval for confidence."
    }
  }
  
  #####################################################3
  else if (measure == "lift") {
    method <- match.arg(method, choices = c("wald"))
    
    if (method == "wald") {
      lift <- with(counts, n * n11 / (n1x * nx1))
      
      # estimate SE using the delta method
      se <- with(counts,
        sqrt(n11 / n * ((n11 / n) ^ 2 * n00 / n + n10 / n * n01 / n) / ((n1x /
            n) ^ 2 * (nx1 / n) ^ 2)) / sqrt(n))
      
      ci <- data.frame(lowerLimit = lift - z * se,
        upperLimit = lift + z * se)
      desc <- "Wald confidence interval for lift."
    }
  }
  
  #####################################################3
  else if (measure == "oddsRatio") {
    method <- match.arg(method, choices = c("wald", "exact"))
    
    if (method == "wald") {
      cnts <- with(counts, data.frame(n00, n01, n10, n11))
      
      # use:
      # smoothCounts = 0 for Woolf interval
      # smoothCounts = 0.5 for Haldane-Anscombe-Gart interval
      
      or <- with(cnts, n11 * n00 / (n01 * n10))
      se.log <-
        with(cnts, sqrt(1 / n00 + 1 / n01 + 1 / n10 + 1 / n11))
      
      ci <- data.frame(
        lowerLimit = exp(log(or) - z * se.log),
        upperLimit = exp(log(or) + z * se.log)
      )
      desc <-
        "Wald confidence interval for odds ratio."
    }
    
    else if (method == "exact") {
      # Exact Fisher's confidence interval
      cnts <- with(counts, data.frame(n11, n01, n10, n00))
      # round up in case of a non-integer smoothCounts value
      cnts <- ceiling(cnts)
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          stats::fisher.test(matrix(ns, ncol = 2), conf.level = level)$conf.int
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Confidence interval for the  odds ratio (Fisher's exact test)."
    }
  }
  
  #####################################################3
  else if (measure == "phi") {
    method <- match.arg(method, choices = c("wald"))
    
    cnts <- with(
      counts,
      data.frame(
        p00 = n00 / n,
        p01 = n01 / n,
        p10 = n10 / n,
        p11 = n11 / n,
        
        p0x = n0x / n,
        p1x = n1x / n,
        px0 = nx0 / n,
        px1 = nx1 / n
      )
    )
    n <- counts$n
    
    phi <-
      with(cnts, (p11 * p00 - p10 * p01) / sqrt(p1x * p0x * px1 * px0))
    v1 <- 1 - phi ^ 2
    v2 <- phi + .5 * phi ^ 3
    v3 <-
      with(cnts, (p0x - p1x) * (px0 - px1) / sqrt(p0x * p1x * px0 * px1))
    v4 <- with(cnts,
      (.75 * phi ^ 2) * ((p0x - p1x) ^ 2 / (p0x * p1x) + (px0 - px1) ^ 2 / (px0 *
          px1)))
    se <- sqrt((v1 + v2 * v3 + v4) / n)
    
    ci <- data.frame(lowerLimit = phi - z * se,
      upperLimit = phi + z * se)
    
    desc <-
      "Wald confidence interval for the phi coefficient."
  }
  
  if (is.null(ci) || is.null(desc))
    stop("Problem with calculating the CI for ", measure)
  
  structure(
    ci,
    measure = measure,
    level = level,
    method = method,
    desc = desc
  )
}
