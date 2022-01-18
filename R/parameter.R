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


#' Classes ASparameter, APparameter, ECparameter ---
#' Specifying the parameter Argument of APRIORI and ECLAT
#'
#' The `ASparameter` class holds the mining parameters (e.g., minimum
#' support) for the used mining algorithms.  `APparameter` and
#' `ECparameter` directly extend `ASparameter` with additional slots
#' for parameters only suitable for [apriori()] (`APparameter`) or [eclat()]
#'  (`ECparameter`).
#'
#' @name ASparameter-classes
#' @aliases parameter
#' @family mining algorithms
#' 
#' @section Available Slots by Subclass:
#' 
#' * `APparameter`: 
#'   `r paste(paste0('\\code{', names(getSlots("APparameter")), '}'), collapse = ", ")`
#' 
#' * `ECparameter`: 
#'   `r paste(paste0('\\code{', names(getSlots("ECparameter")), '}'), collapse = ", ")`
#' 
#' @slot support a numeric value for the 
#'   minimal support of an item set (default: \eqn{0.1})
#' 
#' @slot minlen an integer value for the
#'   minimal number of items per item set (default: 1 item)
#' 
#' @slot maxlen an integer value for the
#'   maximal number of items per item set (default: 10 items)
#' 
#' @slot target a character string indicating the type of association mined. 
#' Partial names are matched. Available targets are:
#' 
#' * `"frequent itemsets"`
#' * `"maximally frequent itemsets"`
#' * `"generator frequent itemsets"`
#' * `"closed frequent itemsets"`
#' * `"rules"` only available for [apriori]; 
#'   use [ruleInduction] for [eclat]. 
#' * `"hyperedgesets"` only available for [apriori]; 
#'   see references for the definition of association hyperedgesets.
#' 
#' @slot ext a logical indicating whether to report coverage (i.e., LHS-support) 
#'   as an extended quality measure (default: `TRUE`)
#' 
#' @slot confidence a numeric value for the
#'   minimal confidence of rules/association hyperedges (default:
#'    \eqn{0.8}). For frequent itemsets it is set to `NA`.
#'    
#' @slot smax a numeric value for the
#'   maximal support of itemsets/rules/hyperedgesets (default: 1)
#'   
#' @slot arem a character string indicating the used additional rule 
#'   evaluation measure (default: `"none"`) given by one of
#'   
#' * `"none"`: no additional evaluation measure
#' * `"diff"`: absolute confidence difference
#' * `"quot"`: difference of confidence quotient to 1
#' * `"aimp"`: absolute difference of improvement to 1
#' * `"info"`: information difference to prior
#' * `"chi2"`: normalized \eqn{\chi^2} measure
#' 
#' **Note:** The measure is only reported if `aval` is set to `TRUE`. 
#' Use `minval` to set minimum thresholds on the measures.
#' 
#' @slot aval a logical indicating whether to 
#'   return the additional rule evaluation measure selected with `arem`.
#'   
#' @slot minval a numeric value for the minimal value of additional 
#'   evaluation measure selected with `arem` (default: \eqn{0.1})
#'   
#' @slot originalSupport a logical indicating whether to
#'   use the original definition of minimum support 
#'   (support of the LHS and RHS of the rule). If set to `FALSE` 
#'   then the support of the LHS (also called coverage of the rule) is returned as support.
#'   The minimum support threshold is applied to this support. (default: `TRUE`)
#'   
#' @slot maxtime Time limit in seconds for checking subsets.
#'   `maxtime = 0` disables the time limit. (default: 5 seconds)
#'   
#' @slot tidLists a logical indicating whether [eclat()] should
#'   return also a list of supporting transactions IDs.
#'   (default: `FALSE`)
#'
#' @section Objects from the Class: 
#' A suitable default parameter object will be
#' automatically created by [apriori()] or
#' [eclat()].  By specifying a named list (names equal to
#' slots) as `parameter` argument for [apriori()] or
#' [eclat()], the default values can be replaced with the values
#' in the list.  
#'
#' Objects can also be created via coercion.
#'  
#' @author Michael Hahsler and Bettina Gruen
#' @references Christian Borgelt (2004) _Apriori --- Finding Association
#' Rules/Hyperedges with the Apriori Algorithm_.
#' \url{https://borgelt.net/apriori.html}
#' @keywords classes
#' @aliases initialize,ASparameter-method show,ASparameter-method
NULL

#' @rdname ASparameter-classes
#' @aliases ASparameter
setClass(
  "ASparameter",
  representation(
    support = "numeric",
    minlen  = "integer",
    maxlen  = "integer",
    target  = "character",
    ext     = "logical"
  ),
  
  prototype(
    target  = "frequent itemsets",
    support = 0.1,
    minlen  = 1L,
    maxlen  = 5L,
    ext     = TRUE
  ),
  
  validity = function(object) {
    if (!object@target %in% .types())
      return(paste("target =", object@target, "not supported."))
    if (object@support > 1)
      return(paste("support =", object@support, "> 1"))
    if (object@minlen <= 0)
      return(paste("minlen =", object@minlen, "<= 0"))
    if (object@minlen > object@maxlen)
      return(paste("minlen =", object@minlen,
        "> maxlen =", object@maxlen))
    return(TRUE)
  }
)

setMethod("initialize", "ASparameter",
  function(.Object, minlen = 1L, maxlen = 10L, ...) {
    
    minlen <- as.integer(minlen)
    maxlen <- as.integer(maxlen)
    
    if(!is.finite(minlen) || minlen<1) 
      stop("minlen needs to be finite and >0")    
    if(!is.finite(maxlen) || maxlen<minlen) 
      stop("maxlen needs to be finite and >minlen")    
    
    .Object@minlen <- minlen
    .Object@maxlen <- maxlen
    
    args = list(...)
    for (i in names(args)) slot(.Object, i, check = FALSE) <- args[[i]]
    validObject(.Object)
    .Object
  })

setMethod("show", signature(object = "ASparameter"),
  function(object) {
    print(data.frame(sapply(slotNames(object), 
      function(x) slot(object, x), 
      simplify = FALSE), row.names = ""))
    
    invisible(NULL)
  })

#' @rdname ASparameter-classes
#' @aliases APparameter initialize,APparameter-method
setClass(
  "APparameter",
  representation(
    confidence  = "numeric",
    minval      = "numeric",
    smax        = "numeric",
    arem        = "character",
    aval        = "logical",
    originalSupport = "logical",
    maxtime     = "numeric"
  ),
  contains = "ASparameter",
  
  prototype(
    new("ASparameter"),
    target      = "rules",
    confidence  = 0.8,
    minval      = 0.1,
    smax        = 1.0,
    arem        = "none",
    originalSupport = TRUE,
    aval = FALSE,
    maxtime = 5.0
  ),
  
  validity = function(object) {
    if (!object@arem %in% .aremtypes())
      return(paste("arem =", object@arem, "not supported."))
    
    if (pmatch(object@target, .types("apriori")) > 4) {
      if (is.na(object@confidence)
        || object@confidence > 1
        || object@confidence < 0)
        return(paste("confidence is not in [0,1]"))
    } else{
      if (!is.na(object@confidence))
        return(paste("frequent itemsets do not use confidence (should be NA)"))
    }
    
    
    if (object@smax < 0)
      return(paste("smax is not < 0"))
    
    return(TRUE)
  }
)

setMethod("initialize", "APparameter",
  function(.Object, minlen = 1L, maxlen = 10L, 
    target = "rules", arem = "none", confidence = .8, ...) {
    
    i <- pmatch(tolower(arem), .aremtypes())
    if (!is.na(i)) .Object@arem <- .aremtypes()[i] 
    else .Object@arem <- arem
    
    i <- pmatch(tolower(target), .types("apriori"))
    if (is.na(i)) stop("Unknown target type!") 
    target <- .types("apriori")[i] 
    
    # no confidence for frequent itemsets 
    if(i>4) .Object@confidence <- confidence
    else .Object@confidence <- as.numeric(NA)
    
    callNextMethod(.Object, minlen = minlen, 
      maxlen = maxlen, target = target, ...)
  })


#' @rdname ASparameter-classes
#' @aliases ECparameter initialize,ECparameter-method
setClass(
  "ECparameter",
  representation(tidLists = "logical"),
  contains = "ASparameter",
  
  prototype(new("ASparameter"),
    tidLists = FALSE),
  
  validity = function(object) {
    if (object@target %in% .types(method = "eclat"))
      return(TRUE)
    else
      return(paste(object@target, "not supported"))
  }
)

setMethod("initialize", "ECparameter",
  function(.Object, minlen = 1L, maxlen = 10L, 
    target = "frequent itemsets", ...) {
    
    i <- pmatch(tolower(target), .types("eclat"))
    if (is.na(i)) stop("Unknown target type!") 
    target <- .types("eclat")[i] 
    
    callNextMethod(.Object, minlen = minlen, 
      maxlen = maxlen, target = target, ...)
  })


#' @rdname ASparameter-classes
#' @name coercion
#' @aliases 
#' coerce,NULL,APparameter-method
#' coerce,list,APparameter-method
#' coerce,NULL,ECparameter-method 
#' coerce,list,ECparameter-method
#' 
#' @section Coercions:
#' 
#' * `as("NULL", "APparameter")`
#' * `as("list", "APparameter")`
#' * `as("NULL", "ECparameter")`
#' * `as("list", "ECparameter")`
NULL

setAs("NULL", "APparameter", function(from, to) new(to))
setAs("list", "APparameter", function(from, to) .list2object(from, to))
setAs("NULL", "ECparameter", function(from, to) new(to))
setAs("list", "ECparameter", function(from, to) .list2object(from, to))
  

