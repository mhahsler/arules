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
## A rule is redundant if a sub rule (with less items in the LHS) has the
## same or higher confidence. (Zaki, 2000)

setMethod("is.redundant", signature(x = "rules"),
  function(x, measure = "confidence") {
  
    ## sort by lift
    o <- sort(x, by=measure, order=TRUE)
    x_sorted <- x[o]
    
    ## find sub rules
    sub_matrix <- is.subset(x_sorted)
    sub_matrix[lower.tri(sub_matrix, diag=TRUE)] <- NA
    
    ## find the sub rules with higher support
    redundant <- colSums(sub_matrix, na.rm=TRUE) > 0L
    
    ## go back to original order 
    redundant[o] <- redundant
    #names(redundant)[o] <- names(redundant)
    unname(redundant)
  })

