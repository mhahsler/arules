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


## Find redundant rules
## redundant rules are rules which do not improve confidence over the
## confidence of their proper sub-rules (i.e., have a negative improvement).

setMethod("is.redundant", signature(x = "rules"),
  function(x,
    measure = "confidence",
    confint = FALSE,
    level = 0.95,
    smoothCounts = 1,
    ...) {
    if (confint)
      .improvementCI(x, measure = measure, level = level, smoothCounts = smoothCounts, ...) <= 0
    else
      interestMeasure(x, measure = "improvement",
        improvementMeasure = measure, smoothCounts = smoothCounts, ...) <= 0
  })

# Is the supersets oddsRatio sign. larger than all its subsets?
# I.e., the superset's lower bound needs to be larger than the subset's upper bound
.improvementCI <- function(x,
  measure,
  level = 0.95,
  smoothCounts = 0.5,
  ...) {
  q <- confint(x,
    measure,
    level = level,
    smoothCounts = smoothCounts,
    ...)
  
  # q is a data.frame with LL and UL
  imp <- numeric(length(x))
  
  ### do it by unique rhs
  rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE)
  
  for (r in unique(rr)) {
    pos <- which(rr == r)
    
    ### FALSE is for verbose
    qsubmax <-
      .Call(R_pnmax, lhs(x[pos])@data, q[pos, "UL"], FALSE)
    
    imp[pos] <- q[pos, "LL"] - qsubmax
  }
  
  imp
}
