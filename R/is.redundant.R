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
#setMethod("is.redundant", signature(x = "rules"),
#  function(x, measure = "confidence") {
#    #i <- interestMeasure(x, measure = "improvement")
#    i <- .improvement(x, transactions = NULL, reuse = TRUE, measure = measure)
#    i <- i<0
#    i[is.na(i)] <- FALSE
#    i
#  })

setMethod("is.redundant", signature(x = "rules"),
  function(x, measure = "confidence") {
    q <- quality(x)[[measure]]
    if(is.null(q)) 
      stop("invalid 'measure'")
    if (any(is.na(q)))
      stop("missing values not implemented")
 
    ### do it by unique rhs
    res <- logical(length(x))
    rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE, PACKAGE = "arules")
    for(r in unique(rr)) {
      pos <- which(rr==r) 
      s <- is.subset(lhs(x[pos]), proper = TRUE, sparse = TRUE)
      s <- as(s, "dgCMatrix")
      i <- s@i + 1L
      j <- diff(s@p)
      j <- rep(seq_along(j), j)
      s@x[(q[pos[i]] <= q[pos[j]])] <- 0
      s <- selectMethod("colSums", class(s))(s) > 0L
      res[pos[s]] <- TRUE
    }
  
  res
  }
)