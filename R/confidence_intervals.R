# Code by Douglas G. Bonett, 5/2018
# R Functions

#Part 1. Confidence Intervals
#Function 1:  Confidence interval for one proportion

CIprop1 <- function(alpha, f, n) {
  # Computes adjusted Wald confidence interval for a population proportion
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f:      number of participants with attribute
  #   n:      sample size
  # Returns:
  #   row 1: adjusted MLE, SE of adjusted MLE, adjusted Wald CI
  #   row 2: MLE, SE of MLE, Wilson CI with continuity correction
  z <- stats::qnorm(1 - alpha / 2)
  p.mle <- f / n
  se.mle <- sqrt(p.mle * (1 - p.mle) / n)
  b1 <- 2 * n * p.mle + z ^ 2
  b2 <- 2 * (n + z ^ 2)
  LL.wil <-
    (b1 - 1 - z * sqrt(z ^ 2 - 2 - 1 / n + 4 * p.mle * (n * (1 - p.mle) + 1))) /
    b2
  UL.wil <-
    (b1 + 1 + z * sqrt(z ^ 2 + 2 - 1 / n + 4 * p.mle * (n * (1 - p.mle) - 1))) /
    b2
  if (LL.wil < 0) {
    LL.wil = 0
  }
  if (UL.wil > 1) {
    UL.wil = 1
  }
  p.adj <- (f + 2) / (n + 4)
  se.adj <- sqrt(p.adj * (1 - p.adj) / (n + 4))
  LL.adj <- p.adj - z * se.adj
  UL.adj <- p.adj + z * se.adj
  if (LL.adj < 0) {
    LL.adj = 0
  }
  if (UL.adj > 1) {
    UL.adj = 1
  }
  out1 <- t(c(p.adj, se.adj, LL.adj, UL.adj))
  out2 <- t(c(p.mle, se.mle, LL.wil, UL.wil))
  out <- rbind(out1, out2)
  colnames(out) <- c("prop", "SE", "LL", "UL")
  rownames(out) <- c("Adjusted Wald", "Wilson with cc")
  return(out)
}

# Example:
#
# CIprop1(.05, 12, 100)
#                     prop         SE         LL        UL
# Adjusted Wald  0.1346154 0.03346842 0.06901848 0.2002123
# Wilson with cc 0.1200000 0.03249615 0.06625153 0.2039772

# Function 2:  Confidence interval for a proportion difference (2-group design)
CIprop2 <- function(alpha, f1, f2, n1, n2) {
  # Computes adjusted Wald confidence interval for difference of
  # population proportions in 2-group design
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f1:     number of participants in group 1 with attribute
  #   f2:     number of participants in group 2 with attribute
  #   n1:     sample size of group 1
  #   n2:     sample size of group 2
  # Returns:
  #   MLEs, SE, confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  mle1 <- f1 / n1
  mle2 <- f2 / n2
  p1 <- (f1 + 1) / (n1 + 2)
  p2 <- (f2 + 1) / (n2 + 2)
  se <- sqrt(p1 * (1 - p1) / (n1 + 2) + p2 * (1 - p2) / (n2 + 2))
  LL <- p1 - p2 - z * se
  UL <- p1 - p2 + z * se
  out <- t(c(mle1, mle2, se, LL, UL))
  colnames(out) <- c("prop1", "prop2", "SE", "LL", "UL")
  return(out)
}


# Example:
#
# CIprop2(.05, 35, 21, 150, 150)
#          prop1 prop2         SE          LL        UL
# [1,] 0.2333333  0.14 0.04476077 0.004375769 0.1798348

# Function 7:  Confidence interval for a proportion difference (within-subjects design)
CIpropWS <- function(alpha, f12, f21, n) {
  # Computes adjusted Wald confidence interval for difference of
  # population proportions in 2-level within-subjects design
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f12:    number of participants who have attribute
  #           in condition 1 but not condition 2
  #   f21:    number of participants who have attribute
  #           in condition 2 but not in condition 1
  #   n:      sample size
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  p12 <- (f12 + 1) / (n + 2)
  p21 <- (f21 + 1) / (n + 2)
  se <- sqrt(((p12 + p21) - (p12 - p21) ^ 2) / (n + 2))
  LL <- p12 - p21 - z * se
  UL <- p12 - p21 + z * se
  CI <- c(LL, UL)
  return(CI)
}

# Example:
#
# CIpropWS(.05, 26, 4, 40)
# [1] 0.3126438 0.7349752


# Function 8:  Confidence interval for a proportion ratio (within-subjects design)
CIpropRatioWS <- function(alpha, f1, f2, f12) {
  # Computes Bonett-Price confidence interval for a ratio of
  # population proportions in a within-subjects design
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f12:    number of participants who have attribute
  #           in condition 1 and condition 2
  #   f1:     number of participants who have attribute
  #           in condition 1
  #   f2:     number of participants who have attribute
  #           in condition 2
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  n0 <- f1 + f2 - f12
  p1 <- f1 / n0
  p2 <- f2 / n0
  f12 <- f1 - f12
  f21 <- f2 - f12
  p1a <- (f1 + 1) / (n0 + 2)
  p2a <- (f2 + 1) / (n0 + 2)
  se.lnp1 <- sqrt((1 - p1a) / ((n0 + 2) * p1a))
  se.lnp2 <- sqrt((1 - p2a) / ((n0 + 2) * p2a))
  se.diff <- sqrt((f12 + f21 + 2) / ((f1 + 1) * (f2 + 1)))
  k <- se.diff / (se.lnp1 + se.lnp2)
  z0 <- k * z
  b = 2 * (n0 + z0 ^ 2)
  LL1 <-
    (2 * f1 + z0 ^ 2 - z0 * sqrt(z0 ^ 2 + 4 * f1 * (1 - p1))) / b
  UL1 <-
    (2 * f1 + z0 ^ 2 + z0 * sqrt(z0 ^ 2 + 4 * f1 * (1 - p1))) / b
  LL2 <-
    (2 * f2 + z0 ^ 2 - z0 * sqrt(z0 ^ 2 + 4 * f2 * (1 - p2))) / b
  UL2 <-
    (2 * f2 + z0 ^ 2 + z0 * sqrt(z0 ^ 2 + 4 * f2 * (1 - p2))) / b
  LL <- exp(log(LL1) - log(UL2))
  UL <- exp(log(UL1) - log(LL2))
  CI <- c(LL, UL)
  return(CI)
}

# Example:
#
# CIpropRatioWS(.05, 26, 4, 4)
# 1]  4.184526 10.096724

# Function 9:  Confidence interval for an odds ratio
CIodds <- function(alpha, f00, f01, f10, f11) {
  # Computes adjusted Wald confidence interval for odds ratio
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f00:     number of participants in cell 00
  #   f01:     number of participants in cell 01
  #   f10:     number of participants in cell 10
  #   f11:     number of participants in cell 11
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  or <- (f11 + .5) * (f00 + .5) / ((f01 + .5) * (f10 + .5))
  se.lor <-
    sqrt(1 / (f00 + .5) + 1 / (f01 + .5) + 1 / (f10 + .5) + 1 / (f11 + .5))
  LL <- exp(log(or) - z * se.lor)
  UL <- exp(log(or) + z * se.lor)
  out <- t(c(or, LL, UL))
  colnames(out) <- c("OR", "LL", "UL")
  return(out)
}

# Example:
#
# CIodds(.05, 229, 28, 96, 24)
#            OR       LL       UL
# [1,] 2.044451 1.133267 3.688254

# Function 10:  Confidence interval for Yules Q
CIYule <- function(alpha, f00, f01, f10, f11) {
  # Computes adjusted Wald confidence interval for odds ratio
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f00:     number of participants in cell 00
  #   f01:     number of participants in cell 01
  #   f10:     number of participants in cell 10
  #   f11:     number of participants in cell 11
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  or <- (f11 + .5) * (f00 + .5) / ((f01 + .5) * (f10 + .5))
  se.lor <-
    sqrt(1 / (f00 + .5) + 1 / (f01 + .5) + 1 / (f10 + .5) + 1 / (f11 + .5))
  LLor <- exp(log(or) - z * se.lor)
  ULor <- exp(log(or) + z * se.lor)
  Q <- (or - 1) / (or + 1)
  LL <- (LLor - 1) / (LLor + 1)
  UL <- (ULor - 1) / (ULor + 1)
  out <- t(c(Q, LL, UL))
  colnames(out) <- c("Yule's Q", "LL", "UL")
  return(out)
}

# Example:
#
# CIYule(.05, 229, 28, 96, 24)
#      Yule's Q         LL       UL
# [1,] 0.343067 0.06247099 0.573402

#Function 11:  Confidence interval for a phi coefficient
CIphi <- function(alpha, f00, f01, f10, f11) {
  # Computes Wald confidence interval for phi coefficient
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f00:     number of participants in cell 00
  #   f01:     number of participants in cell 01
  #   f10:     number of participants in cell 10
  #   f11:     number of participants in cell 11
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  n <- f00 + f01 + f10 + f11
  p00 <- f00 / n
  p01 <- f01 / n
  p10 <- f10 / n
  p11 <- f11 / n
  
  p0x <- (f00 + f01) / n
  p1x <- (f10 + f11) / n
  px0 <- (f00 + f10) / n
  px1 <- (f01 + f11) / n
  phi <- (p11 * p00 - p10 * p01) / sqrt(p1x * p0x * px1 * px0)
  v1 <- 1 - phi ^ 2
  v2 <- phi + .5 * phi ^ 3
  v3 <- (p0x - p1x) * (px0 - px1) / sqrt(p0x * p1x * px0 * px1)
  v4 <-
    (.75 * phi ^ 2) * ((p0x - p1x) ^ 2 / (p0x * p1x) + (px0 - px1) ^ 2 / (px0 *
        px1))
  se <- sqrt((v1 + v2 * v3 + v4) / n)
  LL <- phi - z * se
  UL <- phi + z * se
  out <- t(c(phi, se, LL, UL))
  colnames(out) <- c("phi", "SE", "LL", "UL")
  return(out)
}

# Example:
#
# CIphi(.05, 229, 28, 96, 24)
#            phi         SE         LL        UL
# [1,] 0.1229976 0.05746271 0.01037273 0.2356224

# Function 13:  Confidence interval for a tetrachoric correlation
CItetra <- function(alpha, f00, f01, f10, f11) {
  # Computes Wald confidence interval for a Bonett-Price
  # approximated tetrachoric correlation
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   f00:     number of participants in cell 00
  #   f01:     number of participants in cell 01
  #   f10:     number of participants in cell 10
  #   f11:     number of participants in cell 11
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  n <- f00 + f01 + f10 + f11
  or <- (f11 + .5) * (f00 + .5) / ((f01 + .5) * (f10 + .5))
  r1 <- (f00 + f01 + 1) / (n + 2)
  r2 <- (f10 + f11 + 1) / (n + 2)
  c1 <- (f00 + f10 + 1) / (n + 2)
  c2 <- (f01 + f11 + 1) / (n + 2)
  pmin <- min(c1, c2, r1, r2)
  c <- (1 - abs(r1 - c1) / 5 - (.5 - pmin) ^ 2) / 2
  lor <- log(or)
  se.lor <-
    sqrt(1 / (f00 + .5) + 1 / (f01 + .5) + 1 / (f10 + .5) + 1 / (f11 + .5))
  LL1 <- exp(lor - z * se.lor)
  UL1 <- exp(lor + z * se.lor)
  tetra <- cos(3.14159 / (1 + or ^ c))
  LL <- cos(3.14159 / (1 + LL1 ^ c))
  UL <- cos(3.14159 / (1 + UL1 ^ c))
  out <- t(c(tetra, LL, UL))
  colnames(out) <- c("Tetrachoric", "LL", "UL")
  return(out)
}

# Example:
# CItetra(.05, 46, 15, 54, 85)
#      Tetrachoric        LL        UL
# [1,]   0.5135167 0.3102345 0.6748546

#Function 14:  Confidence interval for 2x2 Agreement
CIagree2x2 <- function(alpha, f00, f01, f10, f11) {
  # Computes adjusted Wald confidence interval for G-index of
  # agreement and Wald confidence interval for intraclass kappa
  # and Cohen's kappa in 2x2 contingency table
  # Arguments:
  #   alpha:  alpha level for 1-alpha confidence
  #   fij:    number of participants who have i score (0 or 1) for
  #           measurement 1 and j score (0 or 1) for measurement 2
  # Returns:
  #   confidence interval for G-index, IC kappa, and Cohen kappa
  z <- stats::qnorm(1 - alpha / 2)
  n <- f00 + f01 + f10 + f11
  g.mle <- 2 * (f00 + f11) / n - 1
  p.adj <- (f00 + f11 + 2) / (n + 4)
  se.g <- 2 * sqrt(p.adj * (1 - p.adj) / n)
  LL.g <-
    2 * (p.adj - z * sqrt(p.adj * (1 - p.adj) / (n + 4))) - 1
  UL.g <-
    2 * (p.adj + z * sqrt(p.adj * (1 - p.adj) / (n + 4))) - 1
  p00 <- f00 / n
  p01 <- f01 / n
  p10 <- f10 / n
  p11 <- f11 / n
  p1 <- (2 * f00 + f01 + f10) / (2 * n)
  k1 <- 4 * (p00 * p11 - p01 * p10) - (p01 - p10) ^ 2
  k2 <- (2 * p00 + p01 + p10) * (2 * p11 + p01 + p10)
  k <- k1 / k2
  se.k <-
    sqrt(((1 - k) / n) * ((1 - k) * (1 - 2 * k) + k * (2 - k) / (2 * p1 * (1 - p1))))
  LL.k <- k - z * se.k
  UL.k <- k + z * se.k
  pr <- (p00 + p01) * (p00 + p10) + (p10 + p11) * (p01 + p11)
  c <- ((p00 + p11) - pr) / (1 - pr)
  a1 <-
    p11 * (1 - (p10 + p01 + 2 * p11) * (1 - c)) ^ 2 + p00 * (1 - (p10 + p01 + 2 *
        p00) * (1 - c)) ^ 2
  a2 <-
    p10 * (p11 + p00 + 2 * p01) ^ 2 * (1 - c) ^ 2 + p01 * (p11 + p00 + 2 *
        p10) ^ 2 * (1 - c) ^ 2
  a3 <- (c - pr * (1 - c)) ^ 2
  se.c <- sqrt(a1 + a2 - a3) / ((1 - pr) * sqrt(n))
  LL.c <- c - z * se.c
  UL.c <- c + z * se.c
  out1 <- c(g.mle, se.g, LL.g, UL.g)
  out2 <- c(k, se.k, LL.k, UL.k)
  out3 <- c(c, se.c, LL.c, UL.c)
  out <- rbind(out1, out2, out3)
  colnames(out) <- c("Estimate", "SE", "LL", "UL")
  rownames(out) <- c("G-index:", "IC kappa:", "Cohen kappa:")
  return(out)
}

# Example:
#
# CIagree2x2(.05, 31, 12, 4, 58)
#               Estimate         SE        LL        UL
# G-index:     0.6952381 0.07247126 0.5303143 0.8091352
# IC kappa:    0.6736597 0.07479965 0.5270551 0.8202643
# Cohen kappa: 0.6756757 0.07344761 0.5317210 0.8196303

#Function 16:  Fisher confidence interval for measures of association
FisherCI <- function(alpha, est, se) {
  # Computes a Fisher confidence interval for measures
  # of association that have a -1 to 1 range
  # Arguments:
  #   alpha: alpha value for 1-alpha confidence
  #   est:   estimate of association coefficient (e.g., phi, Somers D)
  #   se:    standard error of estimate
  # Returns:
  #   confidence interval
  z <- stats::qnorm(1 - alpha / 2)
  zr <- log((1 + est) / (1 - est)) / 2
  LL0 <- zr - z * se / (1 - est ^ 2)
  UL0 <- zr + z * se / (1 - est ^ 2)
  LL <- (exp(2 * LL0) - 1) / (exp(2 * LL0) + 1)
  UL <- (exp(2 * UL0) - 1) / (exp(2 * UL0) + 1)
  CI <- c(LL, UL)
  return(CI)
}

#
# Example:
#
# FisherCI(.05, .641, .052)
# [1] 0.5276396 0.7319293
