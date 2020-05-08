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


setMethod("addComplement", signature(x = "transactions"),
  function(x, labels, complementLabels = NULL) {
    
    ### default names for complements
    if(is.null(complementLabels)) 
      complementLabels <- paste0("!", labels)
    
    ### find labels
    orig <- match(labels, itemLabels(x))
    
    ### add complements (this needs lots of memory for many items)
    add <- !as(x[, orig], "matrix")
    colnames(add) <- complementLabels
    tr <- as(add, "transactions")
    
    ### add variables and levels to original items
    if(is.null(itemInfo(x)$variables)) itemInfo(x)$variables <- itemLabels(x)
    if(is.null(itemInfo(x)$levels)) 
      itemInfo(x)$levels <- factor(rep(NA, ncol(x)), levels = c (TRUE, FALSE))
   
    ### add TRUE/FALSE levels to the items with compliments  
    itemInfo(x)$levels[orig] <- TRUE
    itemInfo(tr)$variables <- labels
    itemInfo(tr)$levels <- factor(rep(FALSE, ncol(tr)), levels = c (TRUE, FALSE))
    
    merge(x, tr)
  })

