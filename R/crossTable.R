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
## Cross-tabulate joint purchases across pairs of items
##

setMethod("crossTable", signature(x = "itemMatrix"),
  function(x, measure = c("count", "support", "probability", "lift", "chiSquared"), 
    sort = FALSE) {
    
    measure <- match.arg(measure)
    
    m <- .Call("R_crosstab_ngCMatrix", x@data, NULL, TRUE,
      PACKAGE="arules")
    if (is.null(dimnames(m)))
      dimnames(m) <- list(itemLabels(x), itemLabels(x))
    
    if(sort) {
      o <- order(diag(m), decreasing = TRUE)
      m <- m[o,o]
    }
    
    if(measure=="count") return(m)
    
    p <- m/nrow(x)
    if(measure=="support" || measure=="probability") return(p)
    
    if(measure=="lift") {
      p_items <- diag(p)
      diag(p) <- NA
      e <- outer(p_items, p_items, "*")
      return(p/e)
    }
    
    if(measure=="chiSquared") {
      p_items <- diag(p)
      diag(p) <- NA
      e <- outer(p_items, p_items, "*")
      return((p-e)^2/e)
    }
    
    stop("Unknown measure!")
  }
)

###
