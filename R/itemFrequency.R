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

#' Getting Frequency/Support for Single Items
#' 
#' Provides the generic function `itemFrequency()` and S4 methods to get the
#' frequency/support for all single items in an objects based on
#' [itemMatrix].  For example, it is used to get the single
#' item support from an object of class [transactions]
#' without mining.
#' 
#' @name itemFrequency
#' @family itemMatrix and transactions functions
#' 
#' @param x an object.
#' @param ... further arguments are passed on.
#' @param type a character string specifying if `"relative"`
#' frequency/support or `"absolute"` frequency/support (item counts) is
#' returned. (default: `"relative"`).
#' @param weighted should support be weighted by transactions weights stored as
#' column `"weight"` in transactionInfo?
#' @return `itemFrequency` returns a named numeric vector.  Each element
#' is the frequency/support of the corresponding item in object `x`.  The
#' items appear in the vector in the same order as in the binary matrix in
#' `x`.
#' @author Michael Hahsler
#' @seealso [itemFrequencyPlot()]
#' @keywords models
#' @examples
#' data("Adult")
#' itemFrequency(Adult, type = "relative")
#' 
setGeneric("itemFrequency",
  function(x, ...) standardGeneric("itemFrequency"))

#' @rdname itemFrequency
setMethod("itemFrequency", signature(x = "itemMatrix"),
  function(x, type = c("relative", "absolute"), weighted = FALSE) {
    type <- match.arg(type)
    
    if(weighted) {
      if(!is(x, "transactions")) 
        stop("weighted itemFrequency only available for transactions")
      if(!("weight" %in% colnames(transactionInfo(x)))) 
        stop("transactions do not contain weights. Add a weight column to transactionInfo.")
      
      weight <- as.numeric(transactionInfo(x)[["weight"]])
      support <- .Call(R_rowWSums_ngCMatrix, x@data, weight)
      total <- sum(weight)
      
    }else {
      ## we could also use rowSums
      ##support <- tabulate(x@data@i + 1L, nbins = x@data@Dim[1])
      
      support <- .Call(R_rowSums_ngCMatrix, x@data)
      total <- length(x)
    }
    
    names(support) <- itemLabels(x)
    
    switch(type,
      relative =  support/total,
      absolute =  support)
  })


#' @rdname itemFrequency
setMethod("itemFrequency", signature(x = "tidLists"),
  function(x, type= c("relative", "absolute")) {
    type <- match.arg(type)
    
    supports <-  size(x)
    names(supports) <- seq_len(length(supports))
    
    switch(type,
      relative =  supports/dim(x)[2],
      absolute =  supports)
  })


#' Creating a Item Frequencies/Support Bar Plot
#' 
#' Provides the generic function `itemFrequencyPlot()` and the S4 method to
#' create an item frequency bar plot for inspecting the item frequency
#' distribution for objects based on [itemMatrix] (e.g.,
#' [transactions], or items in [itemsets]
#' and [rules]).
#' 
#' 
#' @aliases itemFrequencyPlot
#' @family itemMatrix and transactions functions
#' 
#' @param x the object to be plotted.
#' @param \dots further arguments are passed on (see
#' [graphics::barplot()] from possible arguments).
#' @param type a character string indicating whether item frequencies should be
#' displayed relative of absolute.
#' @param weighted should support be weighted by transactions weights stored as
#' column `"weight"` in transactionInfo?
#' @param support a numeric value. Only display items which have a support of
#' at least `support`. If no population is given, support is calculated
#' from `x` otherwise from the population. Support is interpreted relative
#' or absolute according to the setting of `type`.
#' @param topN a integer value. Only plot the `topN` items with the
#' highest item frequency or lift (if `lift = TRUE`).  The items are
#' plotted ordered by descending support.
#' @param population object of same class as `x`; if `x` is a segment
#' of a population, the population mean frequency for each item can be shown as
#' a line in the plot.
#' @param popCol plotting color for population.
#' @param popLwd line width for population.
#' @param lift a logical indicating whether to plot the lift ratio between
#' instead of frequencies. The lift ratio is gives how many times an item is
#' more frequent in `x` than in `population`.
#' @param horiz a logical. If `horiz = FALSE` (default), the bars are
#' drawn vertically. If `TRUE`, the bars are drawn horizontally.
#' @param names a logical indicating if the names (bar labels) should be
#' displayed?
#' @param cex.names a numeric value for the expansion factor for axis names
#' (bar labels).
#' @param xlab a character string with the label for the x axis (use an empty
#' string to force no label).
#' @param ylab a character string with the label for the y axis (see xlab).
#' @param mai a numerical vector giving the plots margin sizes in inches (see
#' `? par').
#' @return A numeric vector with the midpoints of the drawn bars; useful for
#' adding to the graph.
#' @author Michael Hahsler
#' @seealso [itemFrequency()]
#' @keywords hplot
#' @examples
#' data(Adult)
#' 
#' ## the following example compares the item frequencies
#' ## of people with a large income (boxes) with the average in the data set
#' Adult.largeIncome <- Adult[Adult %in% "income=large"]
#' 
#' ## simple plot
#' itemFrequencyPlot(Adult.largeIncome)
#' 
#' ## plot with the averages of the population plotted as a line 
#' ## (for first 72 variables/items)
#' itemFrequencyPlot(Adult.largeIncome[, 1:72], 
#' 	population = Adult[, 1:72])
#' 
#' ## plot lift ratio (frequency in x / frequency in population)
#' ## for items with a support of 20% in the population
#' itemFrequencyPlot(Adult.largeIncome, 
#'         population = Adult, support = 0.2, 
#' 	lift = TRUE, horiz = TRUE)
#' 
setGeneric("itemFrequencyPlot",
  function(x, ...) standardGeneric("itemFrequencyPlot"))

#' @rdname itemFrequencyPlot
setMethod("itemFrequencyPlot", signature(x = "itemMatrix"),
  function(x, type = c("relative", "absolute"), weighted = FALSE, 
    support = NULL, topN = NULL, population = NULL, 
    popCol = "black", popLwd = 1, lift = FALSE, horiz = FALSE,
    names = TRUE, cex.names =  graphics::par("cex.axis"), 
    xlab = NULL, ylab = NULL, mai = NULL, ...) {
    
    type <- match.arg(type)
    
    ## force relative for lift
    if(lift == TRUE) type <- "relative"
    
    ## plot only items with support
    if(!is.null(support)) {
      if(!is.null(population)) {
        frequentItems <- itemFrequency(population, type, weighted= weighted) >= support
        population <- population[, frequentItems]
      } else frequentItems <- itemFrequency(x, type, weighted = weighted) >= support
      x <- x[, frequentItems]
    }
    
    ## get frequencies
    itemFrequency <- itemFrequency(x, type, weighted = weighted)
    if(!is.null(population))
      population.itemFrequency <- itemFrequency(population, type, weighted = weighted)
    
    ## regular plot
    if(lift == FALSE) {
      label <- paste("item frequency (", type, ")", sep="")
      offset <- 0
      
    }else{
      
      ## show lift instead of frequencies
      if(is.null(population)) 
        stop("population needed for plotting lift!")
      
      ## -1 and offset are used to draw bars smaller than one
      ## upside down
      itemFrequency <- (itemFrequency / population.itemFrequency) -1
      offset <- 1
      
      
      ## take care of div by zero
      itemFrequency[is.infinite(itemFrequency)] <- NaN   
      
      label <- "lift ratio"
    }
    
    ## plot only top n items (either itemFrequency or lift)
    if(!is.null(topN)) {
      take <- order(itemFrequency, decreasing = TRUE)[1:topN]
      itemFrequency <- itemFrequency[take] 
      if(!is.null(population))
        population.itemFrequency <- population.itemFrequency[take]
    }
    
    
    ## supress names
    if(names == FALSE) names(itemFrequency) <- NA
    
    if(horiz == FALSE) midpoints <- .barplot_vert(itemFrequency, ..., 
      offset = offset,
      cex.names = cex.names, xlab = xlab, 
      ylab = if(is.null(ylab)) label else ylab, 
      mai = mai) 
    
    else  midpoints <- .barplot_horiz(itemFrequency, ...,
      offset = offset,  
      cex.names = cex.names, xlab = if(is.null(xlab)) label else xlab, 
      ylab = ylab, mai = mai)
    
    
    
    ## add population means (we switch off clipping first!)
    if(!is.null(population) && lift == FALSE)
      if(horiz == FALSE) lines(midpoints, population.itemFrequency, 
        lwd = popLwd, col = popCol, xpd = TRUE)
    else lines(population.itemFrequency, midpoints, 
      lwd = popLwd, col = popCol, xpd = TRUE)
    
    ## return mitpoints
    invisible(midpoints)
  })


## helper functions for barplot
.barplot_vert <- function(height, ..., 
  cex.names = graphics::par("cex.axis"), 
  xlab = NULL, ylab = NULL, mai = NULL){
  
  labels <- names(height)
  
  ## for neg. heights we use straight labels
  if(min(height, na.rm = TRUE) < 0) straight <- TRUE
  else straight <- FALSE
  
  op.mai <- graphics::par("mai")
  if(is.null(mai)) {
    mai <- op.mai
    if (straight == TRUE) mai[1] <- max(graphics::strwidth(labels, units = "inches",
      cex = cex.names)) + min(graphics::par("fin")[2]*0.1, 0.5)
    else mai[1] <- max(graphics::strwidth(labels, units = "inches",
      cex = cex.names)) / 2^.5 + min(graphics::par("fin")[2]*0.1, 0.5)
  } 
  
  graphics::par(mai = mai) 
  on.exit(graphics::par(mai = op.mai))
  
  ## Create plot with no x axis and no x axis label
  
  if(straight == TRUE)
    bp <- graphics::barplot(height, ...,  las=2, cex.names = cex.names, 
      xlab = xlab, ylab = ylab)
  
  else {
    bp <- graphics::barplot(height, ..., xaxt = "n",  xlab = "", ylab = ylab)
    
    ## move down from the lower end of the plot by 1/20 of the plotting
    ## area for labels
    graphics::text(bp, graphics::par("usr")[3] - (graphics::par("usr")[4] - graphics::par("usr")[3]) / 20, 
      srt = 45, adj = 1,
      labels = labels, xpd = TRUE, cex = cex.names)
    
    ## Plot x axis label
    graphics::mtext(1, text = xlab, line = graphics::par("mar")[1]-1)
  }
  invisible(bp)
}

.barplot_horiz <- function(height, ..., 
  cex.names = graphics::par("cex.axis"), xlab = NULL, ylab = NULL, mai = NULL){
  
  ## make enough space for item labels
  op.mai <- graphics::par("mai")
  if(is.null(mai)) {
    mai <- op.mai
    mai[2] <- max(graphics::strwidth(names(height), units = "inches", 
      cex = cex.names)) + min(graphics::par("fin")[1]*0.1, 0.5)
    
  }
  graphics::par(mai = mai) 
  on.exit(graphics::par(mai = op.mai))
  
  midpoints <- graphics::barplot(height, 
    las = 2, cex.name = cex.names, horiz = TRUE,
    xlab = xlab, ylab = ylab, ...)
  
  invisible(midpoints)
}
