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
## Class rules
##
## a set of rules, subclass of associations

## initialize (to make sure lhs and rhs agree!)

setMethod("initialize", "rules",
  function(.Object, lhs, rhs, ...) {
    if(!identical(colnames(lhs), colnames(rhs))) {
      warning("item labels in lhs and rhs do not match. recoding rhs!")
      rhs <- recode(rhs, match=lhs)
    }
    
    .Object@lhs <- lhs
    .Object@rhs <- rhs
    
    .Object <- callNextMethod(.Object, ...)
    
    .Object
  })



##************************************************
## dimensions
setMethod("length", signature(x = "rules"),
    function(x) length(x@lhs))

setMethod("size", signature(x = "rules"),
    function(x) size(x@lhs) + size(x@rhs))

##***********************************************
## coercion
setAs("rules", "data.frame",
    function(from) {
        if (!length(from)) 
            return (data.frame())
        if (!length(from@quality)) 
            return(data.frame(rules = labels(from)))
        data.frame(rules = labels(from), from@quality)
    }
)

##***********************************************
## labels

setMethod("labels", signature(object = "rules"),
    function(object, ruleSep = " => ", ...)
        paste(labels(object@lhs, ...), ruleSep,
              labels(object@rhs, ...), sep = ""))

setMethod("itemLabels", signature(object = "rules"),
    function(object)itemLabels(lhs(object)))

##************************************************
## accessors

setMethod("itemInfo", signature(object = "rules"),
    function(object) itemInfo(object@lhs))

setMethod("lhs", signature(x = "rules"),
    function(x) x@lhs)

setReplaceMethod("lhs", signature(x = "rules"),
    function(x, value) {
        x@lhs <- value
        validObject(x)
        x
    }
)

setMethod("rhs", signature(x = "rules"),
    function(x) x@rhs)

setReplaceMethod("rhs", signature(x = "rules"),
    function(x, value) {
        x@rhs <- value
        validObject(x)
        x
    }
)

## get the union of rhs and lhs
setMethod("items", signature(x = "rules"),
    function(x) itemUnion(x@lhs, x@rhs)
)

# get the generating itemsets
setMethod("generatingItemsets", signature(x = "rules"),
    function(x)
        new("itemsets", 
            items   = items(x), 
            quality = data.frame(support = x@quality[["support"]]),
            info = x@info)
    )

##****************************************************
## subset, combine

setMethod("[", signature(x = "rules", i = "ANY", j = "ANY", drop = "ANY"),
    function(x, i, j, ..., drop) {
        if (!missing(j)) 
            stop("incorrect dimension (j not possible)")
        if (missing(i))
            return(x)
        
        if(any(is.na(i))) {
          warning("Subsetting with NAs. NAs are omitted!")
          if(is.logical(i)) i[is.na(i)] <- FALSE
          else i <- i[!is.na(i)]
        } 
      
        slots <- intersect(slotNames(x), c("lhs", "rhs"))
        for (s in slots) 
            slot(x, s) <- slot(x, s)[i]
        
        x@quality <- x@quality[i,, drop = FALSE]
        
        validObject(x)
        x
    }
)

setMethod("c", signature(x = "rules"),
    function(x, ..., recursive = FALSE) {
        args <- list(...)
        
        if (recursive)
            args <- unlist(args)
        for (y in args) {
            if (!is(y, "rules"))
                stop("can combine rules only")

        ## retain identical info attributes
        info <- y@info
        if (length(info)) {
            k <- match(names(info), names(x@info))
            k <- mapply(identical, info, x@info[k])
            info <- info[k]
        }

        x <- new("rules", 
            lhs     = c(x@lhs, y@lhs), 
            rhs     = c(x@rhs, y@rhs),
            quality = .combineMeta(x, y, "quality"),
            info    = info)
    }
    x
}
)

## this utility function joins the lhs and rhs so it can be
## used for duplicated, unique, etc. 0 is used as separator
## which avoids coercion to character.
.joinedList <- function(x) {
    if (class(x) != "rules")
        stop("not of class rules")

    mapply(function(l, r) c(l, 0, r),
           LIST(x@lhs, decode = FALSE),
           LIST(x@rhs, decode = FALSE), SIMPLIFY = FALSE)
}

setMethod("duplicated", signature(x = "rules"),
    function(x, incomparables = FALSE)
        duplicated(.joinedList(x), incomparables = incomparables))

setMethod("match", signature(x = "rules", table = "rules"),
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
        match(.joinedList(x), .joinedList(table),
              nomatch = nomatch, incomparables = incomparables))

##************************************************
## summary

setMethod("summary", signature(object = "rules"), 
    function(object, ...) {
        sizes <- size(object@lhs) + size(object@rhs)
        
        new("summary.rules", 
            length        = length(object),
            lengths       = table(sizes),
            lengthSummary = summary(sizes),
            quality       = 
                if (length(object@quality)) summary(object@quality)
                else                        summary(NULL),
            info          = object@info 
        )
    }
)

setMethod("show", signature(object = "summary.rules"), 
    function(object) {
        cat("set of", object@length, "rules\n\n")
        
        if(object@length) {
            cat("rule length distribution (lhs + rhs):")
            print(object@lengths)

            cat("\n")
            print(object@lengthSummary)

            cat("\nsummary of quality measures:\n")
            print(object@quality)

            if(length(object@info)) {
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
