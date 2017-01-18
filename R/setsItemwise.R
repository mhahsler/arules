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

### itemUnion
setMethod("itemUnion", signature(x = "itemMatrix", y = "itemMatrix"),
  function(x, y) {
    if(length(x)!=length(y)) stop("Length mismatch between x and y!")
    
    ### the C code does not deal well with a large number of dense rules.
    #x@data <- .Call(R_or_ngCMatrix", x@data, y@data) 
    
    x@data <- as(x@data+y@data, "ngCMatrix")
    
    x
  }
)

setMethod("itemSetdiff", signature(x = "itemMatrix", y = "itemMatrix"),
  function(x, y) {
    if(length(x)!=length(y)) stop("Length mismatch between x and y!")
    
    x@data <- as(drop0(as(x@data-y@data >0, "dgCMatrix")), "ngCMatrix")
    x
  }
)

setMethod("itemIntersect", signature(x = "itemMatrix", y = "itemMatrix"),
  function(x, y) {
    if(length(x)!=length(y)) stop("Length mismatch between x and y!")
    
    x@data <- as(drop0(x@data*y@data), "ngCMatrix")
    x
  }
)


