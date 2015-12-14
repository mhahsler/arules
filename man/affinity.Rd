\name{affinity}
\alias{affinity}
\alias{affinity,itemMatrix-method}
\alias{affinity,matrix-method}
\title{Computing Affinity Between Items}
\description{
  Provides the generic function \code{affinity} and the S4 methods
  to compute and return a similarity matrix with the affinities between items  
  for a set of \code{\linkS4class{transactions}}. 
}
\usage{
affinity(x)
%\S4method{affinity}{itemMatrix}(x)
%#\S4method{affinity}{matrix}(x)
}
\arguments{
  \item{x}{a matrix or an object of class \code{itemMatrix} 
  or \code{transactions}.} 
}
\details{
Affinity between the two items \eqn{i} and \eqn{j} is defined 
by Aggarwal et al. (2002) as
\deqn{A(i,j) = \frac{sup(\{i,j\})}{sup(\{i\}) + sup(\{j\}) - sup(\{i,j\})},}{A(i,j) = sup(\{i,j\})/(sup(\{i\}) + sup(\{j\}) - sup(\{i,j\})),}
where \eqn{sup(.)} is the support measure. This means that affinity is the 
\emph{Jaccard similarity} between items.
}
\value{
  returns an object of class \code{ar_similarity} which represents
  the affinities between items in \code{x}.
}
\seealso{
  \code{\link{dissimilarity}},
  \code{\link{ar_similarity-class}},
  \code{\link[arules]{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

## choose a sample, calculate affinities 
s <- sample(Adult, 500)
s

a <- affinity(s)
image(a)
}
\references{
   Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu (2002)
    Finding localized associations in market basket data,
    \emph{IEEE Trans. on Knowledge and Data Engineering,} 14(1):51--62.
}
\keyword{cluster}
\keyword{models}
