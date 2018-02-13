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
discretize(x, method = "frequency", categories = 3, labels = NULL,     
  ordered = FALSE, onlycuts = FALSE, ...)

discretizeDF(df, methods = list())
}
\arguments{
  \item{x}{a numeric vector (continuous variable).}
  \item{method}{ discretization method. Available are: "interval" (equal interval width), "frequency" (equal frequency), "cluster" (k-means clustering) and
  "fixed" (categories specifies interval boundaries).}
  \item{categories}{ number of categories or a vector with boundaries 
    (all values outside the boundaries will be set to NA). }
  \item{labels}{ character vector; names for categories.}
  \item{ordered}{ logical; return a factor with ordered levels? }
  \item{onlycuts}{ logical; return only computed interval boundaries? }
  \item{\dots}{for method "cluster" further arguments are passed on to
  \code{kmeans}. }.
  \item{df}{data.frame; each numeric column in the data.frame is discretized.}
  \item{methods}{named list of lists; discretization parameters 
    (see parameters of \code{discretize}) for each numeric column
    (see details). If no specific discretization is specified for a column,
    then the default settings for \code{discretize} are used.}
}
\details{
\code{discretize} only implements unsupervised discretization. See packages \pkg{arulesCBA}, \pkg{discretization} or \pkg{RWeka} for supervised 
discretization.

\code{discretizeDF} applies discretization to each numeric column. 
Individual discretization parameters can be specified in the form:
\code{methods = list(column_name1 = list(method = ,...), column_name2 = list(...))}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A factor representing the categorized continuous variable or, if 
  \code{onlycuts=TRUE}, a vector with the interval boundaries.
  
\code{discretizeDF} returns a discretized data.frame.
}
%\references{
%}
%\seealso{
%}
\examples{
data(iris)
x <- iris[,4]
hist(x, breaks = 20, main = "Data")

def.par <- par(no.readonly = TRUE) # save default
layout(mat = rbind(1:2,3:4))

### convert continuous variables into categories (there are 3 types of flowers)
### the default method is equal frequency
table(discretize(x, categories = 3))
hist(x, breaks = 20, main = "Equal Frequency")
abline(v = discretize(x, categories = 3, 
  onlycuts = TRUE), col = "red")
# Note: the frequencies are not exactly equal because of ties in the data 

### equal interval width
table(discretize(x, method = "interval", categories = 3))
hist(x, breaks = 20, main = "Equal Interval length")
abline(v = discretize(x, method = "interval", categories = 3, 
  onlycuts = TRUE), col = "red")

### k-means clustering 
table(discretize(x, method = "cluster", categories = 3))
hist(x, breaks = 20, main = "K-Means")
abline(v = discretize(x, method = "cluster", categories = 3, 
  onlycuts = TRUE), col = "red")

### user-specified (with labels)
table(discretize(x, method = "fixed", categories = c(-Inf,.8, Inf), 
    labels = c("small", "large")))
hist(x, breaks = 20, main = "Fixed")
abline(v = discretize(x, method = "fixed", categories = c(-Inf,.8,Inf), 
    onlycuts = TRUE), col = "red")

par(def.par)  # reset to default

### prepare the iris data set for association rule mining
### use default discretization
irisDisc <- discretizeDF(iris)
head(irisDisc)

### specify discretization for the petal columns
irisDisc <- discretizeDF(iris, methods = list(
  Petal.Length = list(method = "frequency", categories = 3, 
    labels = c("short", "medium", "long")),
  Petal.Width = list(method = "frequency", categories = 2, 
    labels = c("narrow", "wide"))
  ))
head(irisDisc)
}
\author{Michael Hahsler}
\keyword{manip}
