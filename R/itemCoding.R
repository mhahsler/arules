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



## IDs -> labels
setMethod("decode", signature(x = "numeric"),
    function(x, itemLabels) itemLabels[x])

setMethod("decode", signature(x = "list"),
    function(x, itemLabels) lapply(x, function(x) itemLabels[x]))

## labels -> IDs
setMethod("encode", signature(x = "character"),
    function(x, itemLabels, itemMatrix = TRUE) {
        ## itemMatrix always is created from list
        if (itemMatrix == TRUE) 
            return(encode(list(x), itemLabels, itemMatrix == TRUE))

        ## regular encoding
        r <- which(itemLabels %in% x)
        if (length(r) < length(x))
            stop("Unknown item label(s) in ", deparse(x))
        r
    }
)

setMethod("encode", signature(x = "numeric"),
    function(x, itemLabels, itemMatrix = TRUE) {
        ## itemMatrix always is created from list
        if (itemMatrix == TRUE) 
            return(encode(list(x), itemLabels, itemMatrix == TRUE))

        
        ## handle empty sets
        if (length(x)==0) return(integer(0))
            
        ## regular encoding
        r <- range(x)
        if (r[1] < 1 || r[2] > length(itemLabels))
            stop("Invalid range in ", deparse(x))
        if (!is.integer(x)) {
            if (!all.equal(x, (i <- as.integer(x))))
                stop("Invalid numeric values in ", deparse(x))
            i
        } else
            x
    }
)

## NOTES this is less error prone than creating ngCMatrix
##       directly in internal code.
setMethod("encode", signature(x = "list"),
    function(x, itemLabels, itemMatrix = TRUE) {
        i <- lapply(x, encode, itemLabels, itemMatrix = FALSE)
        if (itemMatrix == FALSE) 
            return(i)

        ## yuck
        if (!length(i))
            return(recode(new("itemMatrix"), itemLabels))

        ## fix Matrix mess  (ceeboo 2009)
        i <- lapply(i, sort)

        p <- sapply(i, length)
        names(p) <- NULL
        p <- cumsum(p)
        i <- unlist(i, use.names = FALSE)

        i <- new("ngCMatrix", p   = c(0L, p), 
                              i   = i - 1L,
                              Dim = c(length(itemLabels), length(p)))

        ## item labels must be character
        new("itemMatrix", 
            data     = i,  
            itemInfo = data.frame(labels = as.character(itemLabels), 
              stringsAsFactors = FALSE))
    }
)

## recode to make compatible
setMethod("recode", signature(x = "itemMatrix"),
    function(x, itemLabels = NULL, match = NULL) {
        if (!is.null(match)) {
            if (!is(match, "itemMatrix"))
                stop("'match' not of class itemMatrix")
            if (!is.null(itemLabels))
                stop("'match' and 'itemLabels' cannot both be specified")
            itemLabels <- itemLabels(match)
        }

        k <- match(itemLabels(x), itemLabels)
        if (any(is.na(k)))
            stop ("All item labels in x must be contained in ",
                  "'itemLabels' or 'match'.")

        ## recode items
        if (any(k != seq(length(k))))
            x@data <- .Call(R_recode_ngCMatrix, x@data, k)

        ## enlarge
        if (x@data@Dim[1] <  length(itemLabels))
            x@data@Dim[1] <- length(itemLabels)

        if (!is.null(match)) 
            itemInfo(x) <- itemInfo(match)
        else 
            itemInfo(x) <- data.frame(labels = as.character(itemLabels), 
              stringsAsFactors = FALSE)

        validObject(x)
        x
    }
)	

###
