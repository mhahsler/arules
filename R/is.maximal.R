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



## find maximal itemsets
setMethod("is.maximal", signature(x = "itemMatrix"),
    function(x) {
        ## 
        u <- unique(x)
        m <- .Call("R_pncount", u@data, u@data, TRUE, TRUE, FALSE, 
		PACKAGE="arules") == 1
        i <- match(x, u)
        m[i]
})

setMethod("is.maximal", signature(x = "itemsets"),
    function(x) is.maximal(items(x)) 
)

setMethod("is.maximal", signature(x = "rules"),
    function(x) is.maximal(items(x)) 
)


## old code w/o prefix tree
#setMethod("is.maximal", signature(x = "itemsets"),
    #    function(x, blocksize = 200) is.maximal(items(x), blocksize) 
    #)

#setMethod("is.maximal", signature(x = "itemMatrix"),
#    function(x, blocksize = 200) {
#        ## rowSums(is.subset(x, x, proper = TRUE)) == 0
#
#        ## for large x we use blocks now
#        ## memory requirements for is.subset (see is.super.R) 
#        ## is approx. nx^2 * (8 + 4) byte  
#
#        nx <- length(x)
#        blocksize <- floor(blocksize * 1024 * 1024 / nx / 12)
#
#        if(blocksize < 1)
#        stop("Length of x is to large. Increase usable memory blocksize!")
#
#        ## do it in one run
#        if(nx <= blocksize) 
#        return(rowSums(is.subset(x, proper = TRUE)) == 0)
#
#        ## do it in blocks
#        ismax <- logical(nx)
#
#        blockStart <- 1
#        while(blockStart < nx) {
#            cat(blockStart,"\n")
#            blockEnd <- min(blockStart+blocksize, nx)
#            ismax[blockStart:blockEnd] <- 
#            rowSums(is.subset(x[blockStart:blockEnd], x, 
#                    proper = TRUE)) == 0
#            blockStart <- blockEnd
#        }
#
#        return(ismax)
#    })
#
