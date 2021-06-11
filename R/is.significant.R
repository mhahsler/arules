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


## uses fishers exact test to find significant rules (corrected)
setMethod("is.significant", signature(x = "rules"),
  function(x, transactions, method = "fisher", 
    alpha = 0.01, adjust = "bonferroni") {
    
    methods <- c("fisher", "chisq")
    m <- pmatch(tolower(method), methods)
    if(is.na(m)) stop("Unknown method.")
    method <- methods[m]
    
    if(method == "fisher") p <- interestMeasure(x, 
      measure = "fishersExactTest", 
      transactions = transactions,
      reuse = TRUE)
    
      ### chisq
    else p <- interestMeasure(x, 
        measure = "chiSquared", 
        transactions = transactions,
        reuse = TRUE, significance = TRUE)
    
    if(adjust != "none") p <- stats::p.adjust(p, method = adjust)
    p <= alpha
  })

