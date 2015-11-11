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
## Classes AScontrol, APcontrol, ECcontrol
##
## control parameters for the apriori and eclat functions
## + superclass (AScontol)


##**********************************************************
## coercion
setAs("NULL", "APcontrol",
    function(from, to) { new(to) })

setAs("list", "APcontrol", function(from, to) .list2object(from, to))

setAs("NULL", "ECcontrol",
    function(from, to) { new(to) })

setAs("list", "ECcontrol", function(from, to) .list2object(from, to))

##**********************************************************
## initialize
setMethod("initialize", "AScontrol",
    function(.Object, sort, ...) {
        if (!missing(sort)) {
            if (sort - as.integer(sort)) stop("sort = ", sort, 
                " can not be coerced to integer without error.")
            sort = as.integer(sort)
            .Object <- callNextMethod(.Object, sort = sort, ...)
        }
        else .Object <- callNextMethod(.Object, ...)
        .Object
    })


##**********************************************************
## show

setMethod("show", signature(object = "AScontrol"),
    function(object) {
        print(data.frame(sapply(slotNames(object), 
                    function(x) slot(object, x), simplify = FALSE), 
                        row.names = ""))
        invisible(NULL)
    })




