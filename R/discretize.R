#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
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

discretize <- function(x, method="interval", categories=3, labels=NULL, 
  ordered =FALSE, onlycuts = FALSE, ...) {
  
  
  methods = c("interval", "frequency", "cluster", "fixed")
  
  method <- methods[pmatch(tolower(method), methods)]
  if(is.na(method)) stop("Unknown method!")
  
  res <- switch(method,
    interval = {
      categories <- seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), 
        length.out=categories+1)
      if(onlycuts) categories else .cut2(x, cuts=categories, 
        oneval=FALSE, ...)
    },
    
    frequency = .cut2(x, g=categories, onlycuts=onlycuts, ...),
    
    cluster = {
      cl <-  stats::kmeans(stats::na.omit(x), categories, ...)
      centers <- sort(cl$centers[,1])
      categories <- as.numeric(c(min(x, na.rm=TRUE), head(centers, 
        length(centers)-1) + diff(centers)/2, max(x, na.rm=TRUE)))
      if(onlycuts) categories else .cut2(x, cuts=categories, ...)
    },
    
    fixed = {
      x[x<min(categories) | x>max(categories)] <- NA
      if(onlycuts) categories else .cut2(x, cuts=categories, ...)
    }
  )
    
  if(onlycuts) return(res)
  
  if(ordered) res <- as.ordered(res)
  if(!is.null(labels)) levels(res) <- labels
  res
}

