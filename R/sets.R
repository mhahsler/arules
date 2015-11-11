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



##*****************************************************************
## Basic set operations:  union, intersect, setequal, ... 
## as defined in base; worke now for all classes which implement 
## unique, match and length (in arules associations and itemMatrix). 
##


setMethod("union", signature(x = "associations", y = "associations"),
    function(x, y) unique(c(x, y)) 
) 

setMethod("union", signature(x = "itemMatrix", y = "itemMatrix"),
    function(x, y) unique(c(x, y)) 
) 

setMethod("intersect", signature(x = "associations", y = "associations"),
    function(x, y) unique(y[match(x, y, 0L)])
)

setMethod("intersect", signature(x = "itemMatrix", y = "itemMatrix"),
    function(x, y) unique(y[match(x, y, 0L)])
)

setMethod("setequal", signature(x = "associations", y = "associations"),
    function(x, y) all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
)

setMethod("setequal", signature(x = "itemMatrix", y = "itemMatrix"),
    function(x, y) all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
)

setMethod("setdiff", signature(x = "associations", y = "associations"),
    function(x, y) 
    unique(if (length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)
)

setMethod("setdiff", signature(x = "itemMatrix", y = "itemMatrix"),
    function(x, y) 
    unique(if (length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)
)

setMethod("is.element", signature(el = "associations", set = "associations"),
    function(el, set) match(el, set, 0L) > 0L 
)

setMethod("is.element", signature(el = "itemMatrix", set = "itemMatrix"),
    function(el, set) match(el, set, 0L) > 0L 
)

## 

