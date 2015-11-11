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
## Function subset
##
## subset associations using constraints on interest measures 
## or items.
##
## Note: parent.frame(2) is used so eval uses variables from the 
##       calling function (fix by ceboo)
##

##****************************************************************
## subset for itemMatrix

setMethod("subset", signature(x = "itemMatrix"),
    function(x, subset, ...) {
        if (missing(subset)) return(x)
        i <- eval(substitute(subset), list(items=x),
            parent.frame(2))
        x[i,]
    })



##****************************************************************
## subset for associations

setMethod("subset", signature(x = "itemsets"),
    function(x, subset, ...) {
        if (missing(subset)) return(x)
        i <- eval(substitute(subset), c(quality(x),
                list(items=items(x))),
            parent.frame(2))
        x[i,]
    })

setMethod("subset", signature(x = "rules"),
    function(x, subset, ...) {
        if (missing(subset)) return(x)
        
        i <- eval(substitute(subset), c(quality(x),
                list(lhs=lhs(x), rhs=rhs(x), items=items(x))),
            parent.frame(2))
        x[i,]
    })



