\name{discretize}
\alias{discretize}
\alias{binning}
\alias{discretizeDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a Continuous Variable into a Categorical Variable}
\description{
This function implements several basic unsupervised methods to convert a continuous variable into a categorical variable (factor) using different binning strategies. For convenience, a whole data.frame 
can be discretized (i.e., all numeric columns are discretized).
}
\usage{
discretize(x, method = "frequency", breaks = 3, 
  labels = NULL, include.lowest = TRUE, right = FALSE, dig.lab = 3,
  ordered_result = FALSE, infinity = FALSE, onlycuts = FALSE, 
  categories, ...)

discretizeDF(df, methods = NULL, default = NULL)
}
\arguments{
  \item{x}{a numeric vector (continuous variable).}
  \item{method}{ discretization method. Available are: \code{"interval"} (equal interval width), \code{"frequency"} (equal frequency), \code{"cluster"} (k-means clustering) and
  \code{"fixed"} (categories specifies interval boundaries).
  Note that equal frequency does not achieve perfect equally sized groups if the data contains duplicated values.}
  \item{breaks, categories}{ 
  \bold{\code{categories} is deprecated, use \code{breaks}.}
  either number of categories or a vector with boundaries for 
  discretization (all values outside the boundaries will be set to NA). }
  \item{labels}{ character vector; labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation. If \code{labels = FALSE}, simple integer codes are returned instead of a factor..}
  \item{include.lowest}{ logical; should the first interval be closed to the left?}
  \item{right}{ logical; should the intervals be closed on the right (and open on the left) or vice versa?}
  \item{dig.lab}{ integer; number of digits used to create labels.}
  \item{ordered_result}{ logical; return a ordered factor? }
  \item{infinity}{ logical; should the first/last break boundary changed to 
  +/-Inf? }
  \item{onlycuts}{ logical; return only computed interval boundaries? }
  \item{\dots}{for method "cluster" further arguments are passed on to
  \code{kmeans}. }.
  \item{df}{data.frame; each numeric column in the data.frame is discretized.}
  \item{methods}{named list of lists or a data.frame; 
    the named list contains list of discretization parameters 
    (see parameters of \code{discretize}) for each numeric column
    (see details). If no specific discretization is specified for a column,
    then the default settings for \code{discretize} are used.
    Note: the names have to match exactly.
    If a data.frame is specified, then the discretization breaks in this 
    data.frame are applied to \code{df}.}
  \item{default}{named list; parameters for \code{discretize}
    used for all columns not specified in \code{methods}.}
}
\details{
Discretize calculates breaks between intervals using various methods and then uses 
\code{\link{cut}} to convert the numeric values into intervals represented as a factor.

Discretization may fail for several reasons. Some reasons are
\itemize{
\item A variable contains only a single value. In this case, the variable should be dropped or directly converted into a factor with a single level (see \code{\link{factor}}).
\item Some calculated breaks are not unique. This can happen for method frequency with very skewed data (e.g., a large portion of the values is 0). In this case, non-unique breaks are dropped with a warning. It would be probably better to look at the histogram of the data and decide on breaks for the method fixed.
}

\code{discretize} only implements unsupervised discretization. See 
\code{\link[arulesCBA]{discretizeDF.supervised}} in
package \pkg{arulesCBA} for supervised discretization.

\code{discretizeDF} applies discretization to each numeric column. 
Individual discretization parameters can be specified in the form:
\code{methods = list(column_name1 = list(method = ,...), column_name2 = list(...))}.
If no discretization method is specified for a column, then the discretization in default
is applied (\code{NULL} invokes the default method in \code{discretize()}). The special method \code{"none"} can be specified to suppress discretization for a column.
}
\value{
A factor representing the categorized continuous variable
  with attribute \code{"discretized:breaks"} indicating the used breaks
  or and \code{"discretized:method"} giving the used method. If 
  \code{onlycuts = TRUE} is used, a vector with the calculated 
  interval boundaries is returned.
  
\code{discretizeDF} returns a discretized data.frame.
}
%\references{
%}
\seealso{
\code{\link{cut}},
\code{\link[arulesCBA]{discretizeDF.supervised}}.
}
\examples{
data(iris)
x <- iris[,1]

### look at the distribution before discretizing
hist(x, breaks = 20, main = "Data")

def.par <- par(no.readonly = TRUE) # save default
layout(mat = rbind(1:2,3:4))

### convert continuous variables into categories (there are 3 types of flowers)
### the default method is equal frequency
table(discretize(x, breaks = 3))
hist(x, breaks = 20, main = "Equal Frequency")
abline(v = discretize(x, breaks = 3, 
  onlycuts = TRUE), col = "red")
# Note: the frequencies are not exactly equal because of ties in the data 

### equal interval width
table(discretize(x, method = "interval", breaks = 3))
hist(x, breaks = 20, main = "Equal Interval length")
abline(v = discretize(x, method = "interval", breaks = 3, 
  onlycuts = TRUE), col = "red")

### k-means clustering 
table(discretize(x, method = "cluster", breaks = 3))
hist(x, breaks = 20, main = "K-Means")
abline(v = discretize(x, method = "cluster", breaks = 3, 
  onlycuts = TRUE), col = "red")

### user-specified (with labels)
table(discretize(x, method = "fixed", breaks = c(-Inf, 6, Inf), 
    labels = c("small", "large")))
hist(x, breaks = 20, main = "Fixed")
abline(v = discretize(x, method = "fixed", breaks = c(-Inf, 6, Inf), 
    onlycuts = TRUE), col = "red")

par(def.par)  # reset to default

### prepare the iris data set for association rule mining
### use default discretization
irisDisc <- discretizeDF(iris)
head(irisDisc)

### discretize all numeric columns differently
irisDisc <- discretizeDF(iris, default = list(method = "interval", breaks = 2, 
  labels = c("small", "large")))
head(irisDisc)

### specify discretization for the petal columns and don't discretize the others
irisDisc <- discretizeDF(iris, methods = list(
  Petal.Length = list(method = "frequency", breaks = 3, 
    labels = c("short", "medium", "long")),
  Petal.Width = list(method = "frequency", breaks = 2, 
    labels = c("narrow", "wide"))
  ),
  default = list(method = "none")
  )
head(irisDisc)

### discretize new data using the same discretization scheme as the
###   data.frame supplied in methods. Note: NAs may occure if a new 
###   value falls outside the range of values observed in the 
###   originally discretized table (use argument infinity = TRUE in 
###   discretize to prevent this case.) 
discretizeDF(iris[sample(1:nrow(iris), 5),], methods = irisDisc)
}
\author{Michael Hahsler}
\keyword{manip}
