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

#' Set Operations
#' 
#' Provides the generic functions and the S4 methods for the set operations
#' `union()`, `intersect()`, `setequal()`, `setdiff()` and
#' `is.element()` on sets of [associations] (e.g., [rules], [itemsets]) and
#' [itemMatrix].
#' 
#' All S4 methods for set operations are defined for the class name
#' \code{"ANY"} in the signature, so they should work for all S4 classes for
#' which the following methods are available: [match()], [length()] and
#' [unique()].
#' 
#' @name sets
#' @aliases setOperations sets 
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param x,y,el,set sets of associations or itemMatrix objects.
#' @param ...  Other arguments are unused.
#' @return `union()`, `intersect()`, `setequal()` and `setdiff()`
#' return an object of the same class as \code{x} and \code{y}.
#' 
#' `is.element()` returns a logic vector of length \code{el} indicating for
#' each element if it is included in \code{set}.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#' 
#' ## mine some rules
#' r <- apriori(Adult)
#' 
#' ## take 2 subsets
#' r1 <- r[1:10]
#' r2 <- r[6:15]
#' 
#' union(r1, r2)
#' intersect(r1, r2)
#' setequal(r1, r2)
#' 
NULL

setGeneric("setdiff")
setGeneric("setequal")
setGeneric("intersect")
setGeneric("is.element")
setGeneric("union")

# we now export S3 and S4 versions

#' @rdname sets
#' @method union itemMatrix
union.itemMatrix <- function(x, y, ...) 
  unique(c(x, y)) 
  
#' @rdname sets
#' @method union associations
union.associations <- union.itemMatrix

#' @rdname sets
setMethod("union", "associations", union.associations)

#' @rdname sets
setMethod("union", "itemMatrix", union.itemMatrix)

#' @rdname sets
#' @method intersect itemMatrix
intersect.itemMatrix <- function(x, y, ...) 
    unique(y[match(x, y, 0L)]) 

#' @rdname sets
#' @method intersect associations
intersect.associations <- intersect.itemMatrix 

#' @rdname sets
setMethod("intersect", "associations", intersect.associations)

#' @rdname sets
setMethod("intersect", "itemMatrix", intersect.itemMatrix)


#' @rdname sets
#' @method setequal itemMatrix
setequal.itemMatrix <- function(x, y, ...) 
    all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))

#' @rdname sets
#' @method setequal associations
setequal.associations <- setequal.itemMatrix

#' @rdname sets
setMethod("setequal", "associations", setequal.associations)

#' @rdname sets
setMethod("setequal", "itemMatrix", setequal.itemMatrix)

#' @rdname sets
#' @method setdiff itemMatrix
setdiff.itemMatrix <- function(x, y, ...) 
    unique(if (length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)

#' @rdname sets
#' @method setdiff associations
setdiff.associations <- setdiff.itemMatrix 

#' @rdname sets
setMethod("setdiff", "associations", setdiff.associations)

#' @rdname sets
setMethod("setdiff", "itemMatrix", setdiff.itemMatrix)

#' @rdname sets
#' @method is.element itemMatrix
is.element.itemMatrix <-  function(el, set, ...) 
    match(el, set, 0L) > 0L

#' @rdname sets
#' @method is.element associations
is.element.associations <- is.element.itemMatrix 

#' @rdname sets
setMethod("is.element", "associations", is.element.associations)

#' @rdname sets
setMethod("is.element", "itemMatrix", is.element.itemMatrix)

# setMethod("union", signature(x = "associations", y = "associations"),
#     function(x, y) unique(c(x, y)) 
# ) 
# 
# setMethod("union", signature(x = "itemMatrix", y = "itemMatrix"),
#     function(x, y) unique(c(x, y)) 
# ) 
# 
# setMethod("intersect", signature(x = "associations", y = "associations"),
#     function(x, y) unique(y[match(x, y, 0L)])
# )
# 
# setMethod("intersect", signature(x = "itemMatrix", y = "itemMatrix"),
#     function(x, y) unique(y[match(x, y, 0L)])
# )
# 
# setMethod("setequal", signature(x = "associations", y = "associations"),
#     function(x, y) all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
# )
# 
# setMethod("setequal", signature(x = "itemMatrix", y = "itemMatrix"),
#     function(x, y) all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
# )
# 
# setMethod("setdiff", signature(x = "associations", y = "associations"),
#     function(x, y) 
#     unique(if (length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)
# )
# 
# setMethod("setdiff", signature(x = "itemMatrix", y = "itemMatrix"),
#     function(x, y) 
#     unique(if (length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)
# )
# 
# setMethod("is.element", signature(el = "associations", set = "associations"),
#     function(el, set) match(el, set, 0L) > 0L 
# )
# 
# setMethod("is.element", signature(el = "itemMatrix", set = "itemMatrix"),
#     function(el, set) match(el, set, 0L) > 0L 
# )


