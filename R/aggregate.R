#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
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



## aggregate items to item groups
setMethod("aggregate", signature(x = "itemMatrix"),
	function(x, itemLabels) {
	    
	    ## we can specify the name in itemInfo
	    if(length(itemLabels) == 1 && !is.null(itemInfo(x)[[itemLabels]])) {
	      itemLabels <- itemInfo(x)[[itemLabels]]
	    }
	  
	    itemLabels <- as.factor(itemLabels)

	    if(length(itemLabels) != nitems(x)) stop("Name not available in itemInfo or supplied number of itemLabels does not match number of items in x!")

	    aggrMat <- as(sapply(levels(itemLabels),
			    FUN = function(l) as.numeric(itemLabels == l)), "dgCMatrix")

	    x@data <- as(crossprod(aggrMat, as(as(x, "ngCMatrix"), "dgCMatrix")), 
	      "ngCMatrix")
	    
	    x@itemInfo <- data.frame(labels = levels(itemLabels))

	    validObject(x)
	    x


	})

setMethod("aggregate", signature(x = "itemsets"),
	function(x, itemLabels) {

	    new("itemsets", items=aggregate(items(x), itemLabels))
	    
      ## first support value is used
	    x <- unique(x)
	    x
	})

setMethod("aggregate", signature(x = "rules"),
	function(x, itemLabels) {

	    rhs <- aggregate(rhs(x), itemLabels)
	    lhs <- aggregate(lhs(x), itemLabels)

      ## check if lhs items have to be removed
      lhs <- itemSetdiff(lhs, rhs)
    
      ### remove non-unique rules
      ## first support value is used
      x <- new("rules", lhs=lhs, rhs=rhs)
      x <- unique(x)
	    x
	})



