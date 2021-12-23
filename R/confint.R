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


# NOTE: prop.test can test differences in proportions (could be used for DOC)


#' Confidence Intervals for Interest Measures for Association Rules
#'
#' Computes confidence intervals for interest measures used for mining association [rules].
#'
#' This method creates a contingency table for each rule and then constructs a
#' confidence interval for the specified measures.
#'
#' Fast confidence interval approximations are currently available for the
#' measures "support", "count", "confidence", "lift", "oddsRatio", and "phi".
#' For all other measures, bootstrap sampling from a multinomial distribution
#' is used.
#'
#' Haldan-Anscombe correction (Haldan, 1940; Anscombe, 1956) to avoids issues
#' with zero counts can be specified by `smoothCounts = 0.5`. Here .5 is
#' added to each count in the contingency table.
#'
#' @name confint
#' @family interest measures
#' 
#' @aliases confint confint.rules
#' @param object an object of class [rules].
#' @param parm,measure name of the interest measures (i.e., parameter).
#' `measure` can be used instead of `parm`.
#' @param level the confidence level required.
#' @param side Should a two-sided confidence interval or a one-sided limit be
#' returned? Lower returns an interval with only a lower limit and upper
#' returns an interval with only an upper limit.
#' @param method method to construct the confidence interval. The available
#' methods depends on the measure and the most common method is used by
#' default.
#' @param smoothCounts pseudo count for addaptive smoothing (Laplace
#' smoothing). Often a pseudo counts of .5 is used for smoothing (see Detail
#' Section).
#' @param replications number of replications for method "simulation". Ignored
#' for other methods.
#' @param transactions if the rules object does not contain sufficient quality
#' information, then a set of transactions to calculate the confidence interval
#' for can be specified.
#' @param ... Additional parameters are ignored with a warning.
#' @return Returns a matrix with with one row for each rule and the two columns
#' "LL" and "UL" with the interval. The matrix has the additional attributes:
#'
#' \item{measure}{ the interest measure.} 
#' \item{level}{ the confidence level}
#' \item{side}{ the confidence level} 
#' \item{smoothCounts}{ used count
#' smoothing. } 
#' \item{method}{ name of the method to create the interval }
#' \item{desc}{ desciption of the used method to calculate the confidence
#' interval. The mentioned references can be found below. }
#' @author Michael Hahsler
#' @references Wilson, E. B. (1927). "Probable inference, the law of
#' succession, and statistical inference". 
#' _Journal of the American Statistical Association,_ 22 (158): 209-212.
#' \doi{10.1080/01621459.1927.10502953}
#'
#' Clopper, C.; Pearson, E. S. (1934). "The use of confidence or fiducial
#' limits illustrated in the case of the binomial". _Biometrika,_ 26 (4):
#' 404-413. 
#' \doi{10.1093/biomet/26.4.404}
#'
#' Doob, J. L. (1935). "The Limiting Distributions of Certain Statistics".
#' _Annals of Mathematical Statistics,_ 6: 160-169.
#' \doi{10.1214/aoms/1177732594}
#'
#' Fisher, R.A. (1962). "Confidence limits for a cross-product ratio".
#' _Australian Journal of Statistics,_ 4, 41.
#'
#' Woolf, B. (1955). "On estimating the relation between blood group and
#' diseases". _Annals of Human Genetics,_ 19, 251-253.
#'
#' Haldane, J.B.S. (1940). "The mean and variance of the moments of chi-squared
#' when used as a test of homogeneity, when expectations are small".
#' _Biometrika,_ 29, 133-134.
#'
#' Anscombe, F.J. (1956). "On estimating binomial response relations".
#' _Biometrika,_ 43, 461-464.
#' @keywords manip
#' @examples
#'
#' data("Income")
#'
#' # mine some rules with the consequent "language in home=english"
#' rules <- apriori(Income, parameter = list(support = 0.5),
#'   appearance = list(rhs = "language in home=english"))
#'
#' # calculate the confidence interval for the rules' odds ratios.
#' # note that we use Haldane-Anscombe correction (with smoothCounts = .5)
#' # to avoid issues with 0 counts in the contingency table.
#' ci <- confint(rules, "oddsRatio",  smoothCounts = .5)
#' ci
#'
#' # We add the odds ratio (with Haldane-Anscombe correction)
#' # and the confidence intervals to the quality slot of the rules.
#' quality(rules) <- cbind(
#'   quality(rules),
#'   oddsRatio = interestMeasure(rules, "oddsRatio", smoothCounts = .5),
#'   oddsRatio = ci)
#'
#' rules <- sort(rules, by = "oddsRatio")
#' inspect(rules)
#'
#' # use confidence intervals for lift to find rules with a lift significantly larger then 1.
#' # We set the confidence level to 95%, create a one-sided interval and check
#' # if the interval does not cover 1 (i.e., the lower limit is larger than 1).
#' ci <- confint(rules, "lift", level = 0.95, side = "lower")
#' ci
#'
#' inspect(rules[ci[, "LL"] > 1])
#'
confint.rules <- function(object,
  parm = "oddsRatio",
  level = 0.95,
  measure = NULL,
  side = c("two.sided", "lower", "upper"),
  method = NULL,
  replications = 1000,
  smoothCounts = 0,
  transactions = NULL,
  ...) {
  # measure can be used instead of parm
  if (is.null(measure))
    measure <- parm
  
  measure <- match.arg(measure, choices = measuresRules)
  
  # one-sided CI (adjust level)
  level_orig <- level
  side <- match.arg(side)
  if (side != "two.sided")
    level <- 1 - (1 - level) * 2
  
  if (level < 0 ||
      level > 1)
    stop("the confidence level needs to be in [0, 1].")
  
  counts <-
    as.data.frame(
      .getCounts(
        object,
        transactions = transactions,
        reuse = ifelse(is.null(transactions), TRUE, FALSE),
        smoothCounts = smoothCounts
      )
    )
  
  # try fast approximate intervals first
  ci <- .confint_approx(counts, measure, method, level)
  
  # fall back to bootstrap if .confint_approx does not return anything?
  if (is.null(ci))
    ci <-
    .confint_bootstrap(counts,
      measure,
      level,
      smoothCounts = smoothCounts,
      replications = replications,
      ...)
  
  if (side == "lower")
    ci[, "UL"] <- +Inf
  if (side == "upper")
    ci[, "LL"] <- -Inf
  
  structure(
    ci,
    measure = measure,
    level = level_orig,
    side = side,
    smoothCounts = smoothCounts
  )
}

.confint_bootstrap <-
  function(counts,
    measure,
    level = 0.95,
    smoothCounts = 0,
    replications = 1000,
    ...) {
    method <- "bootstrap"
    desc <-
      "Confidence interval using bootstrapping (random draws from a multinomial distribution)."
    
    qs <- c((1 - level) / 2, (1 + level) / 2)
    
    # smooth for better probability estimates
    counts <- counts[, c("n11", "n10", "n01", "n00")] + smoothCounts
    n <- sum(counts[1,])
    p <- counts / n
    
    ci <-
      matrix(
        NA_real_,
        nrow = nrow(p),
        ncol = 2,
        dimnames = list(NULL, c("LL", "UL"))
      )
    
    ### simulated counts also need to be smoothed
    for (i in seq(nrow(p))) {
      ns <- t(stats::rmultinom(replications, n, p[i,]))
      colnames(ns) <- c("n11", "n10", "n01", "n00")
      vals <-
        .basicRuleMeasure(ns, measure, smoothCounts = smoothCounts, ...)
      if (!any(is.na(vals)))
        ci[i,] <- stats::quantile(vals, probs = qs)
    }
    
    structure(
      ci,
      measure = measure,
      level = level,
      method = method,
      desc = desc
    )
  }


# CIs

# helper to construct a CI from sample mean and variance.
# for log = TRUE, var needs the log of the sample variance
ci.norm <- function(mean,
  var,
  n,
  level = 0.95,
  log = FALSE,
  desc = NULL) {
  z <- stats::qnorm((1 + level) / 2)
  se <- sqrt(var) / sqrt(n)
  if (log)
    ci <- cbind(LL = mean / exp(z * se),
      UL = mean * exp(z * se))
  else
    ci <- cbind(LL = mean - z * se,
      UL = mean + z * se)
  
  structure(ci,
    desc = desc)
}

# Normal approximation population proportion confidence interval (see Wilson, 1927)
ci.prop <- function(f, n, level) {
  p <- f / n
  ci.norm(p, p * (1 - p), n, level, desc = "Normal approximation population proportion confidence interval (see Wilson, 1927)")
}

# Wilson score confidence interval (Wilson, 1927)
# https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm
# NOTE: prop.test can also be used for the Wilson Score interval
ci.prop.wilson <- function(f, n, level) {
  z_UL <- stats::qnorm((1 + level) / 2)
  z_LL <- stats::qnorm((1 - level) / 2)
  
  p <- f / n
  
  ci <- cbind(
    LL = (p + z_LL ^ 2 / (2 * n) +
        z_LL * sqrt(p * (1 - p) / n + z_LL ^ 2 / (4 * n ^ 2))) / (1 + z_LL ^
            2 / n),
    UL = (p + z_UL ^ 2 / (2 * n) +
        z_UL * sqrt(p * (1 - p) / n + z_UL ^ 2 / (4 * n ^ 2))) / (1 + z_UL ^
            2 / n)
  )
  
  structure(ci, desc = "Wilson score confidence interval (Wilson, 1927)")
}

# Exact binomial proportion confidence interval (Clopper and Pearson, 1934)
.vector.prop.binom <- Vectorize(function(f, n, level)
  stats::binom.test(f, n, conf.level = level)$conf.int)

ci.prop.binom <-
  function(f, n, level)
    structure(t(.vector.prop.binom(f, n, level)),
      dimnames = list(NULL, c("LL", "UL")),
      desc = "Exact binomial proportion confidence interval (Clopper and Pearson, 1934)")

# Exact CI for the odds ratio
.vector_OR <- Vectorize(function(n11, n01, n10, n00, level)
  stats::fisher.test(ceil(rbind(
    c(n11, n01), c(n10, n00)
  )), conf.level = level)$conf.int,
)
# ceil rounds up in case of a non-integer smoothCounts value

ci.or.exact <-
  function(n11, n01, n10, n00, level)
    structure(t(.vector_OR(n11, n01, n10, n00, level)),
      dimnames = list(NULL, c("LL", "UL")),
      desc = "Exact confidence interval for the odds ratio (Fisher, 1962).")



# Approximate CIs
.confint_approx <-
  function(counts,
    measure,
    method = NULL,
    level = 0.95,
    smoothCounts = 0) {
    ci <- NULL
    
    # we only use these 4 counts and recompute all other counts/probabilities
    n11 <- counts[, "n11"] + smoothCounts
    n10 <- counts[, "n10"] + smoothCounts
    n01 <- counts[, "n01"] + smoothCounts
    n00 <- counts[, "n00"] + smoothCounts
    
    n <- n11 + n10 + n01 + n00
    n1x <- n11 + n10
    nx1 <- n11 + n01
    n0x <- n - n1x
    nx0 <- n - nx1
    
    p1 <-  n11 / n
    p2 <-  n10 / n
    p3 <-  n01 / n
    p4 <-  n00 / n
    
    p00 <- n00 / n
    p01 <- n01 / n
    p10 <- n10 / n
    p11 <- n11 / n
    
    p0x <- n0x / n
    p1x <- n1x / n
    px0 <- nx0 / n
    px1 <- nx1 / n
    
    ###########################################################################
    if (measure == "count") {
      method <-
        match.arg(method, choices = c("wilson", "normal", "exact", "bootstrap"))
      
      ci <- switch(
        method,
        exact = ci.prop.binom(n11, n, level) * n,
        normal = ci.prop(n11, n, level) * n,
        wilson = ci.prop.wilson(n11, n, level) * n,
        bootstrap = NULL
      )
      
      ci[ci < 0] <- 0
    }
    
    #####################################################
    else if (measure == "support") {
      method <-
        match.arg(method, choices = c("wilson", "normal", "exact", "bootstrap"))
      
      ci <- switch(
        method,
        exact = ci.prop.binom(n11, n, level),
        normal = ci.prop(n11, n, level),
        wilson = ci.prop.wilson(n11, n, level),
        bootstrap = NULL
      )
      
      ci[ci < 0] <- 0
      ci[ci > 1] <- 1
    }
    
    #####################################################3
    else if (measure == "confidence") {
      method <-
        match.arg(method,
          choices = c(
            "delta",
            "log_delta",
            "wilson",
            "normal",
            "exact",
            "bootstrap"
          ))
      
      ci <- switch(
        method,
        delta = ci.norm(p1 / (p1 + p2), p1 * p2 / (p1 + p2) ^ 3, n, level,
          desc = "Delta method confidence interval for confidence (Doob, 1935)."),
        log_delta = ci.norm(
          p1 / (p1 + p2),
          p2 / (p1 * (p1 + p2)),
          n,
          level,
          log = TRUE,
          desc = "Delta method confidence interval for log of confidence (Doob, 1936)."
        ),
        exact = ci.prop.binom(n11, n1x, level),
        normal = ci.prop(n11, n1x, level),
        wilson = ci.prop.wilson(n11, n1x, level),
        bootstrap = NULL
      )
      
      ci[ci < 0] <- 0
      ci[ci > 1] <- 1
    }
    
    #####################################################3
    else if (measure == "lift") {
      method <-
        match.arg(method, choices = c("delta", "log_delta", "bootstrap"))
      
      ci <- switch(
        method,
        delta = ci.norm(p1 / ((p1 + p2) * (p1 + p3)),
          p1 * (p1 ^ 2 * (1 - (
            p1 + p2 + p3
          )) + p2 * p3) / ((p1 + p2) ^ 3 * (p1 + p3) ^ 3),
          n, level, desc = "Delta method confidence interval for lift (Doob, 1935)."),
        
        log_delta = ci.norm(
          p1 / ((p1 + p2) * (p1 + p3)),
          (p1 ^ 2 * (1 - p1 - p2 - p3) + p2 * p3) / (p1 * (p1 + p2) * (p1 + p3)),
          n,
          level,
          log = TRUE,
          desc = "Delta method confidence interval for log of lift (Doob, 1935)."
        ),
        bootstrap = NULL
      )
      
      ci[ci < 0] <- 0
    }
    
    #####################################################3
    else if (measure == "oddsRatio") {
      method <-
        match.arg(method, choices = c("woolf", "gart", "exact", "bootstrap"))
      
      ci <- switch(
        method,
        woolf = {
          # smoothCounts = 0 for Woolf interval
          n00 <- counts[, "n00"]
          n01 <- counts[, "n01"]
          n10 <- counts[, "n10"]
          n11 <- counts[, "n11"]
          
          ci.norm(
            n11 * n00 / (n01 * n10),
            n * (1 / n00 + 1 / n01 + 1 / n10 + 1 / n11),
            n,
            level,
            log = TRUE,
            desc = "Woolf method confidence interval for log of the odds ratio (Woolf, 1955). Delta method without count smoothing."
          )
        },
        
        gart = {
          # smoothCounts = 0.5 for Haldane-Anscombe-Gart interval
          n00 <- counts[, "n00"] + .5
          n01 <- counts[, "n01"] + .5
          n10 <- counts[, "n10"] + .5
          n11 <- counts[, "n11"] + .5
          
          ci.norm(
            n11 * n00 / (n01 * n10),
            n * (1 / n00 + 1 / n01 + 1 / n10 + 1 / n11),
            n,
            level,
            log = TRUE,
            desc = "Gart method confidence interval for log of the odds ratio (Gart and Thomas, 1972). Delta method with count smoothing of .5."
          )
        },
        
        exact = ci.or.exact(n11, n01, n10, n00, level),
        bootstrap = NULL
      )
      
      ci[ci < 0] <- 0
    }
    
    #####################################################3
    else if (measure == "phi") {
      desc <- "Delta method confidence interval for the phi coefficient."
      method <- match.arg(method, choices = c("delta", "bootstrap"))
      
      phi <- (p11 * p00 - p10 * p01) / sqrt(p1x * p0x * px1 * px0)
      v1 <- 1 - phi ^ 2
      v2 <- phi + .5 * phi ^ 3
      v3 <- (p0x - p1x) * (px0 - px1) / sqrt(p0x * p1x * px0 * px1)
      v4 <-
        (.75 * phi ^ 2) * ((p0x - p1x) ^ 2 / (p0x * p1x) + (px0 - px1) ^ 2 / (px0 * px1))
      
      ci <- switch(
        method,
        delta = ci.norm(phi, (v1 + v2 * v3 + v4), n, level,
          desc = "Delta method confidence interval for the phi coefficient."),
        bootstrap = NULL
      )
      
      ci[ci < -1] <- -1
      ci[ci > 1] <- 1
    }
    
    ci
  }
