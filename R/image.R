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



## image plot

## NOTE Matrix coerces to dgTMatrix and then uses levelplot
## FIXME: Matrix does not correctly pass on labels from ngCMatrix so
## 		we manually coerce to dgTMatrix
setMethod("image", signature(x = "itemMatrix"),
    function(x, xlab = "Items (Columns)", ylab = "Elements (Rows)", ...) 
    Matrix::image(as(t(as(x, "ngCMatrix")), "dgCMatrix"), 
	sub = NULL, ylab = ylab, xlab = xlab, ...)
)

setMethod("image", signature(x = "transactions"),
    function(x, xlab = "Items (Columns)", ylab = "Transactions (Rows)" ,...)
    Matrix::image(as(t(as(x, "ngCMatrix")), "dgCMatrix"), 
	sub = NULL, ylab = ylab, xlab = xlab, ...)
)

setMethod("image", signature(x = "tidLists"),
    function(x, xlab="Transactions (Columns)",
        ylab="Items/itemsets (Rows)", ...)
    Matrix::image(as(t(as(x, "ngCMatrix")), "dgCMatrix"), 
            ylab = ylab, xlab = xlab, ...)
)


