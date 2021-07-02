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

## FIXME: we use smoothCount twice for simulation!

confint.rules <- function(object,
  parm = "oddsRatio",
  level = 0.95,
  measure = NULL,
  method = NULL,
  replications = 1000,
  smoothCounts = 0,
  transactions = NULL,
  ...) {
  # measure can be used instead of parm
  if (is.null(measure))
    measure <- parm
  
  measure <- match.arg(measure, choices = measuresRules)
  
  if (level < 0 ||
      level > 1)
    stop("the confidence level needs to be in [0, 1].")
  
  reuse <- ifelse(is.null(transactions), TRUE, FALSE)
  counts <-
    .getCounts(
      object,
      transactions = transactions,
      reuse = reuse,
      smoothCounts = smoothCounts
    )
  
  ci <- .confint_approx(counts, measure, method, level)
  
  # fall back to simulation?
  if (is.null(ci))  
    ci <- .confint_simulation(counts, measure, level, smoothCounts = smoothCounts, 
      replications = replications, ...)
  
  ci
}
 
.confint_simulation <- function(counts, measure, level = 0.95, smoothCounts = 0, 
  replications = 1000, ...) {
  method <- "simulation"
  desc <- "Simulated confidence interval using random draws from a multinomial distribution."
  
  qs <- c((1 - level) / 2, (1 + level) / 2)
  
  n <- counts$n
  p <- cbind(counts$n11 / n, 
    counts$n10 / n, 
    counts$n01 / n,
    counts$n00 / n
  )
  
  ci <- matrix(NA_real_, nrow = nrow(p), ncol = 2, dimnames = list(NULL, c("LL", "UL"))) 
  ### FIXME: we smooth counts twice!!!
  for (i in seq(nrow(p))) {
    ns <- t(stats::rmultinom(replications, n, p[i, ])) + smoothCounts
    colnames(ns) <- c("n11", "n10", "n01", "n00")
    vals <- .basicRuleMeasure(ns, measure, ...)
    if (any(is.na(vals)))
      ci[i, ] <- c(NA_real_, NA_real_)
    else
      ci[i, ] <- stats::quantile(vals, probs = qs)
  }
  
  structure(
    ci,
    measure = measure,
    level = level,
    method = method,
    desc = desc
  )
}

# Tests
ci.prop <- function(f, n, level) {
  z <- stats::qnorm((1 + level) / 2)
  p <- f / n
  se <- sqrt(p * (1 - p) / n)
  
  cbind(LL = pmax(p - z * se, 0),
    UL = pmin(p + z * se, 1))
}

# https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm
ci.prop.wilson <- function(f, n, level) {
  z_UL <- stats::qnorm((1 + level) / 2)
  z_LL <- stats::qnorm((1 - level) / 2)
  
  p <- f / n
  
  cbind(
    LL = (p + z_LL^2 / (2 * n) + 
        z_LL * sqrt(p * (1 - p) / n + z_LL^2 / (4 * n^2))) / (1 + z_LL^2 / n),
    UL = (p + z_UL^2 / (2 * n) + 
        z_UL * sqrt(p * (1 - p) / n + z_UL^2 / (4 * n^2))) / (1 + z_UL^2 / n))
}

ci.prop.binom <- Vectorize(function(f, n, level) stats::binom.test(f, n, conf.level = level)$conf.int)

# Special CIs
.confint_approx <- function(counts, measure, method = NULL, level = 0.95) {
  ci <- NULL
  desc <- NULL
  
  # make sure counts is a data.frame with all the needed info
  if (is.matrix(counts)) {
    nms <- colnames(counts)
    counts <- lapply(seq_len(ncol(counts)), function(i) counts[,i])
    names(counts) <- nms
  }
  if (is.null(counts$n)) counts$n <- counts$n11 + counts$n10 + counts$n01 + counts$n00
  if (is.null(counts$n1x)) counts$n1x <- counts$n11 + counts$n10
  if (is.null(counts$nx1)) counts$nx1 <- counts$n11 + counts$n01
  if (is.null(counts$n0x)) counts$n0x <- counts$n - counts$n1x
  if (is.null(counts$nx0)) counts$nx0 <- counts$n - counts$nx1
  
  ###########################################################################
  if (measure == "count") {
    method <- match.arg(method, choices = c("wilson", "normal", "exact", "simulation"))
    
    if (method == "exact") {
      desc <- "Exact binomial proportion confidence interval for support (Clopper and Pearson, 1934)."
      ci <- ci.prop.binom(counts[["n11"]], counts[["n"]], level) * counts[["n"]]
      ci <- cbind(LL = ci[1, ], UL = ci[2, ])
      
    } else if (method == "normal"){
      desc <- "Normal approximation population proportion confidence interval for support (see Wilson, 1927)."
      ci <- ci.prop(counts[["n11"]], counts[["n"]], level) * counts[["n"]]
      
    } else if (method == "wilson"){
      desc <- "Wilson population proportion confidence interval for support (Wilson, 1927)."
      ci <- ci.prop.wilson(counts[["n11"]], counts[["n"]], level) * counts[["n"]]
    }
  }
  
  #####################################################3
  else if (measure == "support") {
    method <- match.arg(method, choices = c("wilson", "normal", "exact", "simulation"))
    
    if (method == "exact") {
      desc <- "Exact binomial proportion confidence interval for support (Clopper and Pearson, 1934)."
      ci <- ci.prop.binom(counts[["n11"]], counts[["n"]], level)
      ci <- cbind(LL = ci[1, ], UL = ci[2, ])
      
    } else if (method == "normal") {
      desc <- "Normal approximation population proportion confidence interval for support (see Wilson, 1927)."
      ci <- ci.prop(counts[["n11"]], counts[["n"]], level)
      
    } else if (method == "wilson") {
      desc <- "Wilson population proportion confidence interval for support (Wilson, 1927)."
      ci <- ci.prop.wilson(counts[["n11"]], counts[["n"]], level)
    }
  }
  
  #####################################################3
  else if (measure == "confidence") {
    method <- match.arg(method, choices = c( "wilson", "normal", "exact", "simulation"))
    
    if (method == "exact") {
      desc <- "Exact binomial proportion confidence interval for confidence (Clopper and Pearson, 1934)."
      ci <- ci.prop.binom(counts[["n11"]], counts[["n1x"]], level)
      ci <- cbind(LL = ci[1, ], UL = ci[2, ])
      
    } else if (method  == "normal") {
      desc <- "Normal approximation population proportion confidence interval for confidence (see Wilson, 1927)."
      ci <- ci.prop(counts[["n11"]], counts[["n1x"]], level)
      
    } else if (method  == "wilson") {
      desc <- "Wilson population proportion confidence interval for confidence (Wilson, 1927)."
      ci <- ci.prop(counts[["n11"]], counts[["n1x"]], level)
    }
  }
  
  #####################################################3
  else if (measure == "lift") {
    method <- match.arg(method, choices = c("delta", "log_delta", "simulation"))
    
    n <-  counts$n
    p1 <-  counts$n11 / n 
    p2 <-  counts$n10 / n
    p3 <-  counts$n01 / n
    p4 <-  counts$n00 / n
    
    if (method == "delta") {
      desc <- "Delta method confidence interval for lift (Doob, 1935)."
      lift <- p1 / ((p1 + p2) * (p1 + p3)) 
      z <- stats::qnorm((1 + level) / 2)
      se <- sqrt(p1 * (p1^2 * (1 - (p1 + p2 + p3)) + p2 * p3) / ((p1 + p2)^3 * (p1 + p3)^3)) / sqrt(n) 
      ci <- cbind(LL = lift - z * se,
        UL = lift + z * se)
      
    } else if (method == "log_delta") {
      desc <- "Delta method confidence interval for log of lift (Doob, 1935)."
      lift <- p1 / ((p1 + p2) * (p1 + p3)) 
      z <- stats::qnorm((1 + level) / 2)
      se.log <- sqrt( p1^2 * (1 - p1 - p2 - p3) + p2 * p3 / (p1 * (p1 + p2) * (p1 + p3))) / sqrt(n)
      ci <- cbind(LL = lift / exp(z * se.log),
        UL = lift * exp(z * se.log))
    }
  }   
  
  #####################################################3
  else if (measure == "oddsRatio") {
    method <- match.arg(method, choices = c("woolf", "exact", "simulation"))
    
    if (method == "woolf") {
      desc <- "Woolf method confidence interval for log of the odds ratio (Woolf, 1955)."
      n00 <- counts$n00
      n01 <- counts$n01
      n10 <- counts$n10
      n11 <- counts$n11
      
      # use:
      # smoothCounts = 0 for Woolf interval
      # smoothCounts = 0.5 for Haldane-Anscombe-Gart interval
      
      or <- n11 * n00 / (n01 * n10)
      se.log <- sqrt(1 / n00 + 1 / n01 + 1 / n10 + 1 / n11)
      z <- stats::qnorm((1 + level) / 2)
      
      ci <- cbind(
        LL = or / exp(z * se.log),
        UL = or * exp(z * se.log)
      )
      
    } else if (method == "exact") {
      desc <- "Exact confidence interval for the odds ratio (Fisher, 1962)."
      cnts <- with(counts, data.frame(n11, n01, n10, n00))
      # round up in case of a non-integer smoothCounts value
      cnts <- ceiling(cnts)
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          stats::fisher.test(matrix(ns, ncol = 2), conf.level = level)$conf.int
      )
      
      ci <- cbind(LL = ci[1,], UL = ci[2,])
    }
  }
  
  #####################################################3
  else if (measure == "phi") {
    desc <- "Delta method confidence interval for the phi coefficient."
    method <- match.arg(method, choices = c("wald", "simulation"))
    
    n <- counts$n
    p00 <- counts$n00 / n
    p01 <- counts$n01 / n
    p10 <- counts$n10 / n
    p11 <- counts$n11 / n
    
    p0x <- counts$n0x / n
    p1x <- counts$n1x / n
    px0 <- counts$nx0 / n
    px1 <- counts$nx1 / n
    
    phi <- (p11 * p00 - p10 * p01) / sqrt(p1x * p0x * px1 * px0)
    v1 <- 1 - phi ^ 2
    v2 <- phi + .5 * phi ^ 3
    v3 <- (p0x - p1x) * (px0 - px1) / sqrt(p0x * p1x * px0 * px1)
    v4 <- (.75 * phi ^ 2) * ((p0x - p1x) ^ 2 / (p0x * p1x) + (px0 - px1) ^ 2 / (px0 * px1))
    se <- sqrt((v1 + v2 * v3 + v4) / n)
    z <- stats::qnorm((1 + level) / 2)
    
    ci <- cbind(LL = phi - z * se,
      UL = phi + z * se)
  }
  
  ### if we have no method, then we need to do simulation 
  if (is.null(ci)) return(NULL)
  
  structure(
    ci,
    measure = measure,
    level = level,
    method = method,
    desc = desc
  )
}

