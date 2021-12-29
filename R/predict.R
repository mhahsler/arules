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


#' Model Predictions
#'
#' Provides the S4 method `predict()` for [itemMatrix] (e.g.,
#' transactions).  Predicts the membership (nearest neighbor) of new data to
#' clusters represented by medoids or labeled examples.
#'
#' @family proximity classes and functions
#' 
#' @param object medoids (no labels needed) or examples (labels needed).
#' @param newdata objects to predict labels for.
#' @param labels an integer vector containing the labels for the examples in
#' `object`.
#' @param blocksize a numeric scalar indicating how much memory predict can use
#' for big `x` and/or `y` (approx. in MB). This is only a crude
#' approximation for 32-bit machines (64-bit architectures need double the
#' blocksize in memory) and using the default Jaccard method for dissimilarity
#' calculation.  In general, reducing `blocksize` will decrease the memory
#' usage but will increase the run-time.
#' @param ... further arguments passed on to [dissimilarity()]. E.g.,
#' `method`.
#' @return An integer vector of the same length as `newdata` containing
#' the predicted labels for each element.
#' @author Michael Hahsler
#' @keywords models cluster
#' @examples
#' data("Adult")
#'
#' ## sample
#' small <- sample(Adult, 500)
#' large <- sample(Adult, 5000)
#'
#' ## cluster a small sample
#' d_jaccard <- dissimilarity(small)
#' hc <- hclust(d_jaccard)
#' l <-  cutree(hc, k=4)
#'
#' ## predict labels for a larger sample
#' labels <- predict(small, large, l)
#'
#'
#' ## plot the profile of the 1. cluster
#' itemFrequencyPlot(large[labels==1, itemFrequency(large) > 0.1])
setGeneric("predict")

#' @rdname predict
setMethod("predict", signature(object = "itemMatrix"),
  function(object,
    newdata,
    labels = NULL,
    blocksize = 200,
    ...) {
    lenOb <- length(object)
    lenNew <- length(newdata)
    
    ## memory requirements for dissimilarity (see proximities.R)
    ## total w/o input: about 5 * nx * ny * 8 byte
    ## required memory in MB
    ## reqMemMB <- 5 * lenOb * lenNew * 8 / 1024 / 1024
    blocksize <- floor(blocksize * 1024 * 1024 / 5 / lenOb / 8)
    
    if (blocksize < 1)
      stop("Too many examples in object. Increase usable memory blocksize!")
    
    if (is.null(labels))
      labels <- 1:lenOb
    
    # do it in one run
    if (lenOb * lenNew <= blocksize) {
      xd <- dissimilarity(newdata, object, ...)
      return(labels[max.col(-xd)])
    }
    
    # do it in blocks
    newLabels <- integer(lenNew)
    
    blockStart <- 1
    while (blockStart < lenNew) {
      blockEnd <- min(blockStart + blocksize, lenNew)
      xd <-
        dissimilarity(newdata[blockStart:blockEnd], object, ...)
      newLabels[blockStart:blockEnd] <- labels[max.col(-xd)]
      blockStart <- blockEnd
    }
    
    return(newLabels)
  })
