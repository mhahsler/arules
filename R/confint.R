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
  parm,
  level = 0.95,
  smoothCounts = 1,
  transactions = NULL,
  reuse = TRUE, ...) {
  measures <- c("support",
    #  "confidence",
    #  "lift",
    "oddsRatio")
  
  addl <- list(...)
  if (length(addl) > 0L) warning("additional parameters", paste(names(addl), collapse = ", "), "are ignored!")
  
  measure <- match.arg(parm, measures)
  if (level < 0 ||
      level > 1)
    stop("the confidence level needs to be in [0, 1].")
  
  ## remove quality information if we do not want to reuse! Then we can start reusing
  if (!reuse)
    quality(object) <-
    data.frame(support = support(object, transactions = transactions))
  else if (is.null(quality(object)[["support"]]))
    quality(object)[["support"]] <-
    support(object, transactions = transactions)
  reuse <- TRUE
  
  counts <-
    .getCounts(object, transactions, reuse, smoothCounts = smoothCounts)
  
  if (measure == "support") {
    stop("Not implemented yet!")
  }
  
  if (measure == "oddsRatio") {
    or <- with(counts, f11 * f00 / (f10 * f01))
    
    # normal approximation
    w <- with(counts,
      qnorm(1 - (1 - level) / 2) * sqrt(1 / f11 + 1 / f10 + 1 /
          f01 + 1 / f00))
    
    ### todo: fix name to be "2.5 %" and "97.5%"
    return(data.frame(
      lowerBound = or * exp(-1 * w),
      upperBound = or * exp(w)
    ))
  }
}
