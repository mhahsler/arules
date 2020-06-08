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
## Function sample
##
## sample from transactions or associations

setMethod("sample", signature(x = "itemMatrix"),
    function(x, size, replace = FALSE, prob = NULL, ...) {
        index <- sample(c(seq_len(length(x))), size = size, 
            replace = replace, prob = prob)
        
        x[index]
    })

setMethod("sample", signature(x = "associations"),
    function(x, size, replace = FALSE, prob = NULL, ...) {
        index <- sample(c(seq_len(length(x))), size = size, 
            replace = replace, prob = prob)
        
        x[index]
    })

