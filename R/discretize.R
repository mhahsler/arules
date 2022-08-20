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


#' Convert a Continuous Variable into a Categorical Variable
#' 
#' This function implements several basic unsupervised methods to convert a
#' continuous variable into a categorical variable (factor) using different
#' binning strategies. For convenience, a whole data.frame can be discretized
#' (i.e., all numeric columns are discretized).
#' 
#' Discretize calculates breaks between intervals using various methods and
#' then uses [base::cut()] to convert the numeric values into intervals
#' represented as a factor.
#' 
#' Discretization may fail for several reasons. Some reasons are 
#' \itemize{
#' \item A variable contains only a single value. In this case, the variable
#' should be dropped or directly converted into a factor with a single level
#' (see [factor]). 
#' \item Some calculated breaks are not unique.
#' This can happen for method frequency with very skewed data (e.g., a large
#' portion of the values is 0). In this case, non-unique breaks are dropped
#' with a warning. It would be probably better to look at the histogram of the
#' data and decide on breaks for the method fixed. }
#' 
#' `discretize` only implements unsupervised discretization. See
#' [arulesCBA::discretizeDF.supervised()] in package \pkg{arulesCBA}
#' for supervised discretization.
#' 
#' `discretizeDF()` applies discretization to each numeric column.
#' Individual discretization parameters can be specified in the form:
#' `methods = list(column_name1 = list(method = ,...), column_name2 = list(...))`. 
#' If no discretization method is specified for a column, then the
#' discretization in default is applied (`NULL` invokes the default method
#' in `discretize()`). The special method `"none"` can be specified
#' to suppress discretization for a column.
#' 
#' @aliases discretize binning
#' @family preprocessing
#' 
#' @param x a numeric vector (continuous variable).
#' @param method discretization method. Available are: `"interval"` (equal
#' interval width), `"frequency"` (equal frequency), `"cluster"`
#' (k-means clustering) and `"fixed"` (categories specifies interval
#' boundaries).  Note that equal frequency does not achieve perfect equally
#' sized groups if the data contains duplicated values.
#' @param breaks,categories either number of categories or a vector with boundaries for
#' discretization (all values outside the boundaries will be set to NA). 
#' \bold{`categories` is deprecated, use `breaks` instead.}
#' @param labels character vector; labels for the levels of the resulting
#' category. By default, labels are constructed using "(a,b]" interval
#' notation. If `labels = FALSE`, simple integer codes are returned
#' instead of a factor..
#' @param include.lowest logical; should the first interval be closed to the
#' left?
#' @param right logical; should the intervals be closed on the right (and open
#' on the left) or vice versa?
#' @param dig.lab integer; number of digits used to create labels.
#' @param ordered_result logical; return a ordered factor?
#' @param infinity logical; should the first/last break boundary changed to
#' +/-Inf?
#' @param onlycuts logical; return only computed interval boundaries?
#' @param \dots for method "cluster" further arguments are passed on to
#' `kmeans`.
#' @param df data.frame; each numeric column in the data.frame is discretized.
#' @param methods named list of lists or a data.frame; the named list contains
#' lists of discretization parameters (see parameters of `discretize()`) for
#' each numeric column (see details). If no discretization is
#' specified for a column, then the default settings for `discretize()` are
#' used.  Note: the names have to match exactly.  If a data.frame is specified,
#' then the discretization breaks in this data.frame are applied to `df`.
#' @param default named list; parameters for `discretize()` used for all
#' columns not specified in `methods`.
#' @return `discretize()` returns a factor representing the 
#' categorized continuous variable with
#' attribute `"discretized:breaks"` indicating the used breaks or and
#' `"discretized:method"` giving the used method. If `onlycuts = TRUE` 
#' is used, a vector with the calculated interval boundaries is returned.
#' 
#' `discretizeDF()` returns a discretized data.frame.
#' @author Michael Hahsler
#' @seealso [base::cut()],
#' [arulesCBA::discretizeDF.supervised()].
#' @keywords manip
#' @examples
#' data(iris)
#' x <- iris[,1]
#' 
#' ### look at the distribution before discretizing
#' hist(x, breaks = 20, main = "Data")
#' 
#' def.par <- par(no.readonly = TRUE) # save default
#' layout(mat = rbind(1:2,3:4))
#' 
#' ### convert continuous variables into categories (there are 3 types of flowers)
#' ### the default method is equal frequency
#' table(discretize(x, breaks = 3))
#' hist(x, breaks = 20, main = "Equal Frequency")
#' abline(v = discretize(x, breaks = 3, 
#'   onlycuts = TRUE), col = "red")
#' # Note: the frequencies are not exactly equal because of ties in the data 
#' 
#' ### equal interval width
#' table(discretize(x, method = "interval", breaks = 3))
#' hist(x, breaks = 20, main = "Equal Interval length")
#' abline(v = discretize(x, method = "interval", breaks = 3, 
#'   onlycuts = TRUE), col = "red")
#' 
#' ### k-means clustering 
#' table(discretize(x, method = "cluster", breaks = 3))
#' hist(x, breaks = 20, main = "K-Means")
#' abline(v = discretize(x, method = "cluster", breaks = 3, 
#'   onlycuts = TRUE), col = "red")
#' 
#' ### user-specified (with labels)
#' table(discretize(x, method = "fixed", breaks = c(-Inf, 6, Inf), 
#'     labels = c("small", "large")))
#' hist(x, breaks = 20, main = "Fixed")
#' abline(v = discretize(x, method = "fixed", breaks = c(-Inf, 6, Inf), 
#'     onlycuts = TRUE), col = "red")
#' 
#' par(def.par)  # reset to default
#' 
#' ### prepare the iris data set for association rule mining
#' ### use default discretization
#' irisDisc <- discretizeDF(iris)
#' head(irisDisc)
#' 
#' ### discretize all numeric columns differently
#' irisDisc <- discretizeDF(iris, default = list(method = "interval", breaks = 2, 
#'   labels = c("small", "large")))
#' head(irisDisc)
#' 
#' ### specify discretization for the petal columns and don't discretize the others
#' irisDisc <- discretizeDF(iris, methods = list(
#'   Petal.Length = list(method = "frequency", breaks = 3, 
#'     labels = c("short", "medium", "long")),
#'   Petal.Width = list(method = "frequency", breaks = 2, 
#'     labels = c("narrow", "wide"))
#'   ),
#'   default = list(method = "none")
#'   )
#' head(irisDisc)
#' 
#' ### discretize new data using the same discretization scheme as the
#' ###   data.frame supplied in methods. Note: NAs may occure if a new 
#' ###   value falls outside the range of values observed in the 
#' ###   originally discretized table (use argument infinity = TRUE in 
#' ###   discretize to prevent this case.) 
#' discretizeDF(iris[sample(1:nrow(iris), 5),], methods = irisDisc)
#' @export discretize
discretize <- function(x, method = "frequency", breaks = 3, 
  labels = NULL, include.lowest = TRUE, right = FALSE, dig.lab = 3,
  ordered_result = FALSE, infinity = FALSE, onlycuts = FALSE, categories = NULL, ...) {
  
  if(!is.null(categories)) {
    warning("Parameter categories is deprecated. Use breaks instead! Also, the default method is now frequency!")
    breaks <- categories
  }
  
  methods <- c("interval", "frequency", "cluster", "fixed")
  
  method <- methods[pmatch(tolower(method), methods)]
  if(is.na(method)) stop("Unknown method!")
  
  if(method == "fixed" && length(breaks) < 2) 
    stop("fixed needs at least two values for breaks.")
  if(method != "fixed" && (length(breaks) != 1 || breaks < 1))
    stop("breaks needs to be a single positive integer for this method.")
  
  breaks <- switch(method,
    interval = seq(from=min(x, na.rm=TRUE), to=max(x, na.rm=TRUE), 
      length.out=breaks+1),
    
    frequency = stats::quantile(x, probs = seq(0,1, length.out = breaks+1), 
      na.rm = TRUE),
    
    cluster = {
      cl <-  stats::kmeans(stats::na.omit(x), breaks, ...)
      centers <- sort(cl$centers[,1])
      as.numeric(c(min(x, na.rm=TRUE), head(centers, 
        length(centers)-1) + diff(centers)/2, max(x, na.rm=TRUE)))
    },
    
    fixed = breaks
  )
  
  if(any(duplicated(breaks))){ 
    warning("The calculated breaks are: ", paste(breaks, collapse = ", "), "\n  Only unique breaks are used reducing the number of intervals. Look at ? discretize for details.")
    
    breaks <- unique(breaks)
    if(length(breaks) < 2) stop("Less than 2 uniques breaks left. Maybe the variable has only one value!")
  }

  
  ### fix first and last to -/+Inf
  if(infinity) {
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
  }
  
  if(onlycuts) return(as.vector(breaks))
  
  structure(
    cut(x, breaks = breaks, labels = labels, 
      include.lowest = include.lowest, right = right, dig.lab = dig.lab,
      ordered_result = ordered_result),
    "discretized:breaks" = as.vector(breaks),
    "discretized:method" = method
  )  
}

#' @rdname discretize
discretizeDF <- function(df, methods = NULL, default = NULL) {
  
  ### methods is a data.frame to get the discretization info from
  if(is.data.frame(methods)) return(.rediscretizeDF(methods, df))
  
  for(i in colnames(df)) {
    if(!is.numeric(df[[i]])) next
    args <- if(is.null(methods[[i]])) default else methods[[i]]
    
    ### skip columns with method na
    if(!is.null(args) && (is.null(args$method) || args$method == "none")) next
    
    if(is(err <- try(
      df[[i]] <- do.call("discretize", c(list(x = df[[i]]), args)), 
      silent = TRUE), "try-error")) stop("Problem with column ", i, "\n", err)
  }
  
  df
}

.rediscretizeDF <- function(data, newdata) {
  
  if(!all(colnames(data) == colnames(newdata))) stop("column names in the new data are not the same as in the discretized data.")
  
  cps <- lapply(data, FUN = function(x) {
    breaks <- attr(x, "discretized:breaks")
    if(is.null(breaks)) NULL
    else list(breaks = breaks, method = "fixed", labels = levels(x))
  })
  
  discretizeDF(newdata, methods = cps, default = list(method = "none"))
}

