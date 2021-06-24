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
    #"chiSquared",
    "kappa",
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
      cnts <- with(counts, data.frame(n11, n))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          stats::binom.test(ns["n11"], ns["n"] , conf.level = level)$conf.int * ns["n"]
          #stats::poisson.test(ns["n11"], conf.level = level)$conf.int
      )
      
      ci <- data.frame(lowerLimit = ci[1, ], upperLimit = ci[2, ])
      desc <-
        "Exact binomial proportion confidence interval for support (Clopper and Pearson)."
      
    } else {
      cnts <- with(counts, data.frame(n11, n))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          CIprop1(alpha = 1 - level, ns["n11"], ns["n"])[1, c("LL", "UL")] * ns["n"]
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Adjusted Wald population proportion confidence interval for support."
    }
  }
  
  #####################################################3
  else if (measure == "support") {
    method <- match.arg(method, choices = c("wald", "exact"))
    
    if (method == "exact") {
      cnts <- with(counts, data.frame(n11, n))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          stats::binom.test(ns["n11"], ns["n"] , conf.level = level)$conf.int
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Exact binomial proportion confidence interval for support (Clopper and Pearson)."
    }
    
    else if (method == "wald") {
      cnts <- with(counts, data.frame(n11, n))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          CIprop1(alpha = 1 - level, ns["n11"], ns["n"])[1, c("LL", "UL")]
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Adjusted Wald population proportion confidence interval for support."
    }
  }
  
  #####################################################3
  else if (measure == "confidence") {
    method <- match.arg(method, choices = c("wald", "wilson", "exact"))
    
    if (method == "exact") {
      cnts <- with(counts, data.frame(n11, n1x))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          stats::binom.test(ns["n11"], ns["n1x"] , conf.level = level)$conf.int
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Exact binomial proportion confidence interval for confidence (Clopper and Pearson)."
    }
    
    else if (method  == "wald") {
      cnts <- with(counts, data.frame(n11, n1x))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          CIprop1(alpha = 1 - level, ns["n11"], ns["n1x"])[1, c("LL", "UL")]
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Adjusted Wald population proportion confidence interval for confidence."
    }
    
    
    else if (method == "wilson") {
      cnts <- with(counts, data.frame(n11, n1x))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          CIprop1(alpha = 1 - level, ns["n11"], ns["n1x"])[2, c("LL", "UL")]
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Wilson confidence interval for population proportion with continuity correction."
      
    }
  }
  
  #####################################################3
  else if (measure == "kappa") {
    method <- match.arg(method, choices = c("wald"))
    
    cnts <- with(counts, data.frame(n00, n01, n10, n11)) + .5
    
    ci <- apply(
      cnts,
      MARGIN = 1,
      FUN = function(ns)
        CIagree2x2(alpha = 1 - level, ns["n00"], ns["n01"], ns["n10"], ns["n11"])[3, c("LL", "UL")]
    )
    
    ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
    desc <-
      "Adjusted Wald confidence interval for Cohen's kappa."
  }
  
  else if (measure == "lift") {
    method <- match.arg(method, choices = c("wald"))
     
    if (method == "wald") {
       lift <- with(counts, n * n11 / (n1x * nx1))
       z <- stats::qnorm(1 - (1 - level) / 2)
       
       # estimate SE using the delta method
       w <- z * with(counts,
         sqrt(n11 / n * ((n11 / n) ^ 2 * n00 / n + n10 / n * n01 / n) / ((n1x /
             n) ^ 2 * (nx1 / n) ^ 2)) / sqrt(n))
       
       ci <- data.frame(
            lowerLimit = lift - w,
            upperLimit = lift + w
          )
       desc <- "Wald confidence interval for lift." 
      }
  }
  
  #####################################################3
  # we calculate confidence intervals for the proportions in the transactions with x and the proportions in
  # the transactions for Y and return the larger interval.
  # else if (measure == "lift") {
  #   method <- match.arg(method, choices = c("wald", "exact"))
  #   
  #   if (method == "exact") {
  #     n <- counts$n
  #     cnts <- with(counts, data.frame(n11, n1x, nx1))
  #     
  #     ci <- apply(
  #       cnts,
  #       MARGIN = 1,
  #       FUN = function(ns)
  #         stats::binom.test(ns["n11"], min(ns["n1x"], ns["nx1"]), conf.level = level)$conf.int /
  #         (max(ns["n1x"], ns["nx1"]) / n)
  #       
  #     )
  #     
  #     ci <- data.frame(lowerLimit = ci[1, ], upperLimit = ci[2, ])
  #     desc <-
  #       "Smaller of the two exact binomial proportion confidence intervals (Clopper and Pearson)."
  #   } else {
  #     ### Wald
  #     n <- counts$n
  #     cnts <- with(counts, data.frame(n11, n1x, nx1))
  #     
  #     ci <- apply(
  #       cnts,
  #       MARGIN = 1,
  #       FUN = function(ns) {
  #         # min/max chooses the narrower CI
  #         CIprop1(alpha = 1 - level, ns["n11"], min(ns["n1x"], ns["nx1"]))[1, c("LL", "UL")] /
  #           (max(ns["n1x"], ns["nx1"]) / n)
  #       }
  #     )
  #     
  #     ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
  #     desc <- "Smaller of the two adjusted Wald proportion confidence intervals (proportion with the narrower interval)."
  #     
  #   }
  # }
  
  #####################################################3
  else if (measure == "oddsRatio") {
    method <- match.arg(method, choices = c("wald", "exact"))
    
    if (method == "wald") {
      cnts <- with(counts, data.frame(n00, n01, n10, n11))
      
      ci <- apply(
        cnts,
        MARGIN = 1,
        FUN = function(ns)
          CIodds(alpha = 1 - level, ns["n00"], ns["n01"], ns["n10"], ns["n11"])[1, c("LL", "UL")]
      )
      
      ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
      desc <-
        "Adjusted Wald confidence interval for odds ratio."
      
    }
    
    # # normal approximation
    # if (measure == "oddsRatio_normal") {
    #   or <- with(counts, n11 * n00 / (n01 * n10))
    #   z <- stats::qnorm(1 - (1 - level) / 2)
    #
    #   w <- with(counts,
    #     z * sqrt(1 / n11 + 1 / n01 + 1 /
    #         n10 + 1 / n00))
    #
    #   return(data.frame(
    #     lowerLimit = or * exp(-1 * w),
    #     upperLimit = or * exp(w)
    #   ))
    # }
    
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
    
    cnts <- with(counts, cbind(n00, n01, n10, n11))
    
    ci <- apply(
      cnts,
      MARGIN = 1,
      FUN = function(ns) {
        # Haldane-Anscombe correction
        if (any(ns == 0))
          ns <- ns + .5
        CIphi(alpha = 1 - level, ns["n00"], ns["n01"], ns["n10"], ns["n11"])[1, c("LL", "UL")]
      }
    )
    
    ci <- data.frame(lowerLimit = ci[1,], upperLimit = ci[2,])
    desc <-
      "Adjusted Wald confidence interval for the phi coefficient."
  }
  
  
  # #####################################################3
  # else if (measure == "chiSquared") {
  #   method <- match.arg(method, choices = c("wald"))
  #   
  #   chi2 <- numeric(length(object))
  #   
  #   for (i in seq_len(length(object))) {
  #     # Haldane-Anscombe correction (+.5)
  #     fo <-
  #       matrix(c(counts$n11[i], counts$n01[i], counts$n10[i], counts$n00[i]),
  #         ncol = 2) + .5
  #     
  #     #fe <- tcrossprod(c(nx0[i], nx1[i]), c(n0x[i], n1x[i])) / n
  #     ## check if approximation is ok
  #     ## we don't do this now
  #     ##if(any(fe < 5)) chi2[i] <- nA
  #     ##else
  #     #chi2[i] <- sum((fo - fe) ^ 2 / fe)
  #     
  #     # warning about approximation
  #     suppressWarnings(chi2[i] <-
  #         stats::chisq.test(fo, correct = FALSE)$statistic)
  #   }
  #   
  #   z <- stats::qnorm(1 - (1 - level) / 2)
  #   
  #   # SE(chi-squared(k)) = \sqrt(2k)
  #   # k = 1 for 2 x 2 tables
  #   w <- z * sqrt(2) / sqrt(counts$n)
  #   
  #   ci <-
  #     data.frame(lowerLimit = chi2 - w,
  #       upperLimit = chi2 + w)
  #   desc <-
  #     "Adjusted Wald confidence interval for the chi-squared statistic."
  #   
  # }
   
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
