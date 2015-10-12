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



##*******************************************************
## predict
##
## object ... medoids (no labels needed) or examples (labels needed)
## newdata ... objects to predict labels for
## ... ... for dissimilarity, e.g., method

setMethod("predict", signature(object = "itemMatrix"),
    function(object, newdata, labels = NULL, blocksize = 200, ...) {

        lenOb <- length(object) 
        lenNew <- length(newdata)

        ## memory requirements for dissimilarity (see proximities.R)
        ## total w/o input: about 5 * nx * ny * 8 byte
        ## required memory in MB
        ## reqMemMB <- 5 * lenOb * lenNew * 8 / 1024 / 1024
        blocksize <- floor(blocksize * 1024 * 1024 / 5 / lenOb / 8)

        if(blocksize < 1) 
        stop("Too many examples in object. Increase usable memory blocksize!") 

        if(is.null(labels)) labels <- 1 : lenOb

        # do it in one run
        if(lenOb*lenNew <= blocksize) {
            xd <- dissimilarity(newdata, object, ...)
            return(labels[max.col(-xd)])
        }

        # do it in blocks
        newLabels <- integer(lenNew)

        blockStart <- 1
        while(blockStart < lenNew) {
            blockEnd <- min(blockStart+blocksize, lenNew)
            xd <- dissimilarity(newdata[blockStart:blockEnd], object, ...)
            newLabels[blockStart:blockEnd] <- labels[max.col(-xd)] 
            blockStart <- blockEnd
        }

        return(newLabels)	


        ## check labels stuff
    })



