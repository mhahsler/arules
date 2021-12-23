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



#' Classes AScontrol, APcontrol, ECcontrol ---
#' Specifying the control Argument of Apriori and Eclat
#'
#' The `AScontrol` class holds the algorithmic parameters for the used
#' mining algorithms.  `APcontrol` and `ECcontrol` directly extend
#' `AScontrol` with additional slots for parameters only suitable for the
#' algorithms Apriori (`APcontrol`) and Eclat (`ECcontrol`).
#'
#'
#' @name AScontrol-classes
#' @aliases AScontrol AScontrol-class control
#' @family mining algorithms
#'
#' @section Objects from the Class: A suitable default control object will be
#' automatically created by the [apriori] or the
#' [eclat] function.  By specifying a named list (names equal to
#' slots) as `control` argument for the [apriori] or the
#' [eclat] function, default values can be replaced by the values
#' in the list.  Objects can also be created via coercion.
#'
#' @slot sort an integer scalar indicating how to 
#'   sort items with respect to their frequency: (default: 2)
#'   
#'   *  1: ascending
#'   * -1: descending
#'   *  0: do not sort
#'   *  2: ascending
#'   * -2: descending with respect to transaction size sum
#'
#' @slot verbose a logical indicating whether to report progress
#' 
#' @slot filter a numeric scalar indicating how to
#'   filter unused items from transactions (default: 0.1)
#'   
#'   * \eqn{=0}: do not filter items with respect to. usage in sets
#'   * \eqn{<0}: fraction of removed items for filtering
#'   * \eqn{>0}: take execution times ratio into account
#'   
#' @slot tree a logical indicating whether to
#'   organize transactions as a prefix tree (default: `TRUE`)
#'   
#' @slot heap a logical indicating whether to
#'   use heapsort instead of quicksort to sort the transactions
#'   (default: `TRUE`)
#'   
#' @slot memopt a logical indicating whether to 
#'   minimize memory usage instead of maximize speed (default: `FALSE`)
#'   
#' @slot load a logical indicating whether to
#'   load transactions into memory (default: `TRUE`)
#'   
#' @slot sparse a numeric value for the
#'   threshold for sparse representation (default: 7)
#'
#' @author Michael Hahsler and Bettina Gruen
#' @references Christian Borgelt (2004) _Apriori --- Finding Association
#' Rules/Hyperedges with the Apriori Algorithm_.
#' \url{https://borgelt.net/apriori.html}
#' @keywords classes
#' @aliases initialize,AScontrol-method show,AScontrol-method
setClass(
  "AScontrol",
  representation(sort    = "integer",
    verbose = "logical"),
  
  prototype(verbose = TRUE,
    sort    = 2L),
  
  validity = function(object) {
    if (object@sort > 2 | object@sort < -2)
      return(
        paste(
          "sort =",
          object@sort,
          "not one of 1: ascending,",
          "-1: descending, 0: do not sort, 2: ascending,",
          "-2: descending w.r.t. transaction size sum"
        )
      )
    else
      return(TRUE)
  }
)

setMethod("initialize", "AScontrol",
  function(.Object, sort, ...) {
    if (!missing(sort)) {
      if (sort - as.integer(sort))
        stop("sort = ", sort,
          " can not be coerced to integer without error.")
      sort <- as.integer(sort)
      .Object <- callNextMethod(.Object, sort = sort, ...)
    }
    else
      .Object <- callNextMethod(.Object, ...)
    .Object
  })

setMethod("show", signature(object = "AScontrol"),
  function(object) {
    print(data.frame(
      sapply(slotNames(object),
        function(x)
          slot(object, x), simplify = FALSE),
      row.names = ""
    ))
    invisible(NULL)
  })

#' @rdname AScontrol-classes
#' @aliases APcontrol
setClass(
  "APcontrol",
  representation(
    filter  = "numeric",
    tree    = "logical",
    heap    = "logical",
    memopt  = "logical",
    load    = "logical"
  ),
  contains    = "AScontrol",
  
  prototype(
    new("AScontrol"),
    filter  = 0.1,
    sort    = 2L,
    tree    = TRUE,
    heap    = TRUE,
    memopt  = FALSE,
    load    = TRUE
  ),
  
  validity = function(object) {
    if (object@filter > 1 || object@filter < -1)
      return(paste("filter =", object@filter, "is not in [-1,1]"))
    else
      return(TRUE)
  }
)

#' @rdname AScontrol-classes
#' @aliases ECcontrol
setClass(
  "ECcontrol",
  representation(sparse = "numeric"),
  contains = "AScontrol",
  
  prototype(new("AScontrol"),
    sparse  = 7,
    sort    = -2L)
)

#' @rdname AScontrol-classes
#' @name coerce-AScontrol
#' @section Coercion:
#'
#' * as("NULL", "APcontrol")
#' * as("list", "APcontrol")
#' * as("NULL", "ECcontrol")
#' * as("list", "ECcontrol")
#'
#' @aliases 
#'   coerce,NULL,APcontrol-method 
#'   coerce,list,APcontrol-method
#'   coerce,NULL,ECcontrol-method 
#'   coerce,list,ECcontrol-method
#'

setAs("NULL", "APcontrol",
  function(from, to) {
    new(to)
  })

setAs("list", "APcontrol", function(from, to)
  .list2object(from, to))

setAs("NULL", "ECcontrol",
  function(from, to) {
    new(to)
  })

setAs("list", "ECcontrol", function(from, to)
  .list2object(from, to))

