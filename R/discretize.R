#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta, 
#                       Bettina Gruen and Kurt Hornik
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

### discretize continuous variables

discretize <- function(x, method = "frequency", breaks = 3, 
  labels = NULL, include.lowest = TRUE, right = TRUE, dig.lab = 3,
  ordered_result = FALSE, infinity = FALSE, onlycuts = FALSE, ...) {
  
  methods = c("interval", "frequency", "cluster", "fixed")
  
  method <- methods[pmatch(tolower(method), methods)]
  if(is.na(method)) stop("Unknown method!")
  
  breaks <- switch(method,
    interval = seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), 
        length.out=breaks+1),

    frequency = quantile(x, probs = seq(0,1, length.out = breaks+1)),

    cluster = {
      cl <-  stats::kmeans(stats::na.omit(x), breaks, ...)
      centers <- sort(cl$centers[,1])
      as.numeric(c(min(x, na.rm=TRUE), head(centers, 
        length(centers)-1) + diff(centers)/2, max(x, na.rm=TRUE)))
    },
    
    fixed = breaks
  )
     
  ### fix first and last to -/+Inf
  if(infinity) {
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
  }
  
  if(onlycuts) return(as.vector(breaks))
  
  structure(
    cut(x, breaks = breaks, labels = labels, 
      include.lowest = include.lowest, right = right, 
      ordered_result = ordered_result),
    'discretized:breaks' = as.vector(breaks)
  )  
}


discretizeDF <- function(df, methods = NULL, default = NULL) {
  
  ### methods is a data.frame to get the discretization info from
  if(is.data.frame(methods)) return(.rediscretizeDF(methods, df))
  
  for(i in colnames(df)) {
    if(is.logical(df[[i]])) next
    if(is.numeric(df[[i]])) {
      args <- default
      if(!is.null(methods[[i]])) args <- methods[[i]]
      df[[i]] <- do.call("discretize", c(list(x = df[[i]]), args))
    }
  }
  
  df
}


.rediscretizeDF <- function(data, newdata) {
  
  if(!all(colnames(data) == colnames(newdata))) stop("columns in data and newdata do not conform!")
  
  cps <- lapply(data, FUN = function(x) { 
    list(breaks = attr(x, "discretized:breaks"), method = "fixed", 
      labels = levels(x))
    })
  
  discretizeDF(newdata, methods = cps)
}