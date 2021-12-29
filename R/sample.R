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

setGeneric("sample")

#' Random Samples and Permutations
#' 
#' Provides the generic function `sample()` and the S4 method to take a
#' sample of the specified size from the elements of `x` using either with
#' or without replacement.  `sample()` can be used to sample from a set of
#' [transactions] or [associations].
#' 
#' @name sample
#' @aliases sample
#' @family preprocessing
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param x object to be sampled from (a set of [associations] or
#' [transactions]).
#' @param size sample size.
#' @param replace a logical. Sample with replacement?
#' @param prob a numeric vector of probability weights.
#' @param ...  further arguments.
#' @return An object of the same class as `x`.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#' 
#' ## sample with replacement
#' s <- sample(Adult, 500, replace = TRUE)
#' s
NULL

#' @rdname sample
setMethod("sample", signature(x = "itemMatrix"),
  function(x,
    size,
    replace = FALSE,
    prob = NULL,
    ...) {
    index <- sample(length(x),
      size = size,
      replace = replace,
      prob = prob)
    
    x[index]
  })

#' @rdname sample
setMethod("sample", signature(x = "associations"),
  function(x,
    size,
    replace = FALSE,
    prob = NULL,
    ...) {
    index <- sample(length(x),
      size = size,
      replace = replace,
      prob = prob)
    
    x[index]
  })
