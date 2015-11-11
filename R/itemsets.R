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
## Class itemsets
##
## a set of itemsets, subclass of associations


##************************************************
## dimensions
setMethod("length", signature(x = "itemsets"),
    function(x) length(x@items))

setMethod("size", signature(x = "itemsets"),
    function(x) size(x@items))

##***********************************************
## coercion
setAs("itemsets", "data.frame", 
    function(from) {
        if (!length(from)) 
            return (data.frame())
        if (!length(from@quality)) 
            return(data.frame(itemsets = labels(from)))
        data.frame(items = labels(from), from@quality)
    }
)

##***********************************************
## labels

setMethod("labels", signature(object = "itemsets"),
    function(object, ...) labels(object@items, ...))

setMethod("itemLabels", signature(object = "itemsets"),
    function(object) itemLabels(object@items))

##************************************************
## accessors

setMethod("itemInfo", signature(object = "itemsets"),
    function(object) object@items@itemInfo)

setMethod("items", signature(x = "itemsets"),
    function(x) x@items)

## fixme
setReplaceMethod("items", signature(x = "itemsets"),
    function(x, value) {
        x@items <- value
        validObject(x)
        x
    }
)

setMethod("tidLists", signature(x = "itemsets"),
    function(x) x@tidLists)

##****************************************************
## subset, combine, duplicated, match

setMethod("[", signature(x = "itemsets", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        if (!missing(j)) 
            stop("incorrect number of dimensions (j not possible)")
        if (missing(i)) 
            return(x)
       
        if(any(is.na(i))) {
          warning("Subsetting with NAs. NAs are omitted!")
          if(is.logical(i)) i[is.na(i)] <- FALSE
          else i <- i[!is.na(i)]
        } 
       
        slots <- intersect(slotNames(x), c("items", "tidLists"))
        for (sl in slots) 
            slot(x, sl) <- slot(x, sl)[i]
        
        x@quality <- x@quality[i,, drop = FALSE]
        
        validObject(x)
        x
    }
)


## FIXME: tidList not handled
setMethod("c", signature(x = "itemsets"),
    function(x, ..., recursive = FALSE){
        args <- list(...)
       
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!is(y, "itemsets"))
                stop("can combine itemsets only")
           
            ## retain identical info attributes
            info <- y@info
            if (length(info)) {
                k <- match(names(info), names(x@info))
                k <- mapply(identical, info, x@info[k])
                info <- info[k]
            }

            x <- new("itemsets", items   = c(x@items, y@items), 
                                 quality = .combineMeta(x, y, "quality"),
                                 info    = info)
        }
        x
}
)

setMethod("duplicated", signature(x = "itemsets"),
    function(x, incomparables = FALSE)
        duplicated(x@items, incomparables = incomparables))

setMethod("match", signature(x = "itemsets", table = "itemsets"),
    function(x,  table, nomatch = NA_integer_, incomparables = NULL)
        match(x@items, table@items, nomatch = nomatch,
              incomparables = incomparables))

##************************************************
## show / summary

setMethod("summary", signature(object = "itemsets"), 
    function(object, ...) {
        new("summary.itemsets", 
            length   = length(object),
            items    = summary(object@items,  ...),
            quality  = if (length(object@quality)) summary(object@quality)
                       else                        summary(NULL),
            info     = object@info,
            tidLists = !is.null(object@tidLists)
        )
    }
)

setMethod("show", signature(object = "summary.itemsets"), 
    function(object) {
        cat("set of", object@length,"itemsets\n")

        if (object@length) {     
            cat("\nmost frequent items:\n")
            print(object@items@itemSummary)
            cat("\nelement (itemset/transaction) length distribution:")
            print(object@items@lengths)

            cat("\n")
            print(object@items@lengthSummary)

            cat("\nsummary of quality measures:\n")
            print(object@quality)
            cat("\nincludes transaction ID lists:",object@tidLists,"\n")
        
            if (length(object@info)) {
                info <- object@info  
                if(is(info$data, "language"))
                                  info$data <- deparse(info$data)

                cat("\nmining info:\n")
                print(data.frame(info, row.names=""))
            }

        }
        invisible(NULL)
    }
)

###
