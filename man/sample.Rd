\name{sample}
\alias{sample}
\alias{sample,itemMatrix-method}
\alias{sample,associations-method}
\title{Random Samples and Permutations}
\description{
  Provides the generic function \code{sample} and the S4 method to take a
  sample of the specified size from the elements of \code{x} using either with
  or without replacement.  \code{sample} can be used to sample from a set of
  transactions or associations.
}
\usage{
sample(x, size, replace = FALSE, prob = NULL, ...)
}
\arguments{
  \item{x}{ object to be sampled from 
  	(a set of \code{associations} or \code{transactions}).}
  \item{size}{ sample size.}
  \item{replace}{a logical. Sample with replacement? }
  \item{prob}{  a numeric vector of probability weights. }
  \item{...}{  further arguments. }
}
\value{
  An object of the same class as \code{x}.
}
\seealso{
  \code{\link{associations-class}},
  \code{\link{transactions-class}},
  \code{\link{itemMatrix-class}}.
}
\author{Michael Hahsler}
\examples{
data("Adult")

## sample with replacement
s <- sample(Adult, 500, replace = TRUE)
s
}
\keyword{manip}
