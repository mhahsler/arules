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
  function(x, measure = "confidence", ...) {
    if (measure == "oddsRatioCI")
      .improvementCI(x, transactions = NULL, reuse = TRUE, ...) <= 0
    else
      interestMeasure(x, measure = "improvement", transactions = NULL, reuse = TRUE, 
        improvementMeasure = measure, ...) <= 0
  }
)


### TODO: The empty LHS rule has a OR of NaN! Should it be f11/f10?

# Is the supersets oddsRatio sign. larger than all its subsets?
# I.e., the superset's lower bound needs to be larger than the subset's upper bound
.improvementCI <- function(x,
  transactions = NULL,
  reuse = TRUE,
  confidenceLevel = 0.9,
  smoothCounts = 1) {

  q <- interestMeasure(x, "oddsRatioCI", transactions, reuse, 
    confidenceLevel = confidenceLevel, smoothCounts = smoothCounts)
  # q is a data.frame with lowerBound and upperBound
  
  imp <- numeric(length(x))
  
  ### do it by unique rhs
  rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE)
  
  for (r in unique(rr)) {
    pos <- which(rr == r)
    
    ### FALSE is for verbose
    qsubmax <- .Call(R_pnmax, lhs(x[pos])@data, q$upperBound[pos], FALSE)
    
    imp[pos] <- q$lowerBound[pos] - qsubmax
  }
  
  imp
}
