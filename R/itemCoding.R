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
            warning("The following item labels are not available in itemLabels: ",
                paste(setdiff(x, itemLabels), collapse = ", "), 
                "\nItems with missing labels are dropped!", call. = FALSE)
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
            stop("Invalid item ID in ", deparse(x), call. = FALSE)
        
        ## deal with numeric
        if (!is.integer(x)) {
            if (any(x %% 1 != 0))
                stop("Invalid item ID (needs to be integer) in ", deparse(x), call. = FALSE)
            x <- as.integer(x)
        }
        x
    }
)

## NOTES this is less error prone than creating ngCMatrix
##       directly in internal code.
setMethod("encode", signature(x = "list"),
    function(x, itemLabels, itemMatrix = TRUE) {
        if(is(itemLabels, "itemMatrix") || 
                is(itemLabels, "association")) itemLabels <- itemLabels(itemLabels)
        
        # this calls encode for character
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
        
        ### FIXME: Deprecated
        if(!is.null(match)) message("recode: parameter 'match' is deprecated. Use 'itemLabels' instead.")
        
        if(!is.null(itemLabels) && !is.null(match))
            stop("'match' and 'itemLabels' cannot both be specified")
        if(is.null(itemLabels)) 
            if(is.null(match))  stop("Either 'match' or 'itemLabels' has to be specified")
        else itemLabels <- itemLabels(match)            
        ### END 
        
        if(is(itemLabels, "itemMatrix") || 
                is(itemLabels, "association")) itemLabels <- itemLabels(itemLabels)
        
	    ## nothing to do
	    if(identical(itemLabels(x), itemLabels)) return(x)

        k <- match(itemLabels(x), itemLabels)
        if (any(is.na(k)))
            stop ("All item labels in x must be contained in 'itemLabels'.", call. = FALSE)

        ## recode items
        if (any(k != seq(length(k))))
            x@data <- .Call(R_recode_ngCMatrix, x@data, k)

        ## enlarge matrix for additional items
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

setMethod("recode", signature(x = "itemsets"),
    function(x, itemLabels = NULL, match = NULL) {
        x@items <- recode(x@items, itemLabels, match)
        x
    }
)

setMethod("recode", signature(x = "rules"),
    function(x, itemLabels = NULL, match = NULL) {
        x@lhs <- recode(x@lhs, itemLabels, match)
        x@rhs <- recode(x@rhs, itemLabels, match)
        x
    }
)   

setMethod("compatible", signature(x = "itemMatrix"),
    function(x, y) identical(itemLabels(x), itemLabels(y))
)

setMethod("compatible", signature(x = "associations"),
    function(x, y) identical(itemLabels(x), itemLabels(y))
)

###
