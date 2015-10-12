\name{predict}
\alias{predict}
\alias{predict,itemMatrix-method}
\title{Model Predictions}
\description{
  Provides the S4 method \code{predict} for \code{itemMatrix} (e.g.,
  transactions).  Predicts the membership (nearest neighbor) of new data to
  clusters represented by medoids or labeled examples.
  }
\usage{
\S4method{predict}{itemMatrix}(object, newdata, labels = NULL, blocksize = 200,\ldots)
}
\arguments{
  \item{object}{ medoids (no labels needed) or examples (labels needed). }
  \item{newdata}{ objects to predict labels for. }
  \item{labels}{an integer vector containing the labels for the examples in
    \code{object}. }
  \item{blocksize}{a numeric scalar indicating how much memory predict can
    use for big \code{x} and/or \code{y} (approx. in MB). This is only a crude
    approximation for 32-bit machines (64-bit architectures need double the
    blocksize in memory) and using the default Jaccard method for dissimilarity
    calculation.  In general, reducing \code{blocksize} will decrease the
    memory usage but will increase the run-time.}
  \item{\ldots}{ further arguments passed on to \code{dissimilarity}. E.g., 
    \code{method}.}
}
\value{
   An integer vector of the same length as \code{newdata}  
   containing the predicted labels for each element.
}
\seealso{
  \code{\link{dissimilarity}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

## sample
small <- sample(Adult, 500)
large <- sample(Adult, 5000)

## cluster a small sample
d_jaccard <- dissimilarity(small)
hc <- hclust(d_jaccard)
l <-  cutree(hc, k=4)

## predict labels for a larger sample
labels <- predict(small, large, l)


## plot the profile of the 1. cluster
itemFrequencyPlot(large[labels==1, itemFrequency(large) > 0.1])
}
\keyword{models}
\keyword{cluster}
