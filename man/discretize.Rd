\name{discretize}
\alias{discretize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a Continuous Variable into a Categorical Variable}
\description{
This function implements several basic unsupervized methods to convert continuous variables into a categorical variables (factor) suitable for association rule mining.
}
\usage{
discretize(x, method="interval", categories = 3, labels = NULL,     
  ordered=FALSE, onlycuts=FALSE, ...)
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
}
\details{
\code{discretize} only implements unsupervised discretization. See packages \pkg{discretization} or \pkg{RWeka} for supervised discretization.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A factor representing the categorized continuous variable or, if 
  \code{onlycuts=TRUE}, a vector with the interval boundaries.
}
%\references{
%}
%\seealso{
%}
\examples{
data(iris)
x <- iris[,4]
hist(x, breaks=20, main="Data")

def.par <- par(no.readonly = TRUE) # save default
layout(mat=rbind(1:2,3:4))

### convert continuous variables into categories (there are 3 types of flowers)
### default is equal interval width
table(discretize(x, categories=3))
hist(x, breaks=20, main="Equal Interval length")
abline(v=discretize(x, categories=3, onlycuts=TRUE), 
col="red")

### equal frequency
table(discretize(x, "frequency", categories=3))

hist(x, breaks=20, main="Equal Frequency")
abline(v=discretize(x, method="frequency", categories=3, onlycuts=TRUE), 
col="red")

### k-means clustering 
table(discretize(x, "cluster", categories=3))
hist(x, breaks=20, main="K-Means")
abline(v=discretize(x, method="cluster", categories=3, onlycuts=TRUE), 
col="red")


### user-specified
table(discretize(x, "fixed", categories = c(-Inf,.8,Inf)))
table(discretize(x, "fixed", categories = c(-Inf,.8, Inf), 
    labels=c("small", "large")))
hist(x, breaks=20, main="Fixed")
abline(v=discretize(x, method="fixed", categories = c(-Inf,.8,Inf), 
    onlycuts=TRUE), col="red")

par(def.par)  # reset to default

### prepare the iris data set for association rule mining
for(i in 1:4) iris[,i] <- discretize(iris[,i],  "frequency", categories=3)

trans <- as(iris, "transactions")
inspect(head(trans, 1))

as(head(trans, 3),"matrix")
}
\author{Michael Hahsler}
\keyword{manip}
