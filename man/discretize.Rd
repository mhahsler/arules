\name{discretize}
\alias{discretize}
\alias{discretizeDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a Continuous Variable into a Categorical Variable}
\description{
This function implements several basic unsupervised methods to convert continuous variables into a categorical variables (factor) suitable for association rule mining. For convenience, a whole data.frame 
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
\code{discretize} only implements unsupervised discretization. See packages \pkg{arulesCBA}, \pkg{discretization} or \pkg{RWeka} for supervised 
discretization.

\code{discretizeDF} applies discretization to each numeric column. 
Individual discretization parameters can be specified in the form:
\code{methods = list(column_name1 = list(method = ,...), column_name2 = list(...))}.
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
%\seealso{
%}
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

### specify discretization for the petal columns
irisDisc <- discretizeDF(iris, methods = list(
  Petal.Length = list(method = "frequency", breaks = 3, 
    labels = c("short", "medium", "long")),
  Petal.Width = list(method = "frequency", breaks = 2, 
    labels = c("narrow", "wide"))
  ))
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
