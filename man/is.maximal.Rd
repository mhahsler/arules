\name{is.maximal}
\alias{is.maximal}
\alias{is.maximal,itemsets-method}
\alias{is.maximal,itemMatrix-method}
%
%
\title{Find Maximal Itemsets}
\description{
Provides the generic function and the S4 method \code{is.maximal} for finding
maximal itemsets.  An itemset is maximal in a set if no proper superset of the
itemset is contained in the set (Zaki et al., 1997).
}
\usage{
is.maximal(x,\ldots)
%\S4method{is.maximal}{itemMatrix}(x, blocksize = 200)
\S4method{is.maximal}{itemMatrix}(x)
}
\arguments{
  \item{x}{ the set of itemsets or an itemMatrix object. }
  \item{\ldots}{ further arguments.}
%  \item{blocksize}{a numeric value indicating how much memory can is.maximal
%    use for large \code{x} (approx. in MB). This is only a
%    crude approximation for 32-bit machines (64-bit architectures need
%    double the blocksize in memory).}
}
%\details{
%Note that the method uses \code{is.superset} which can be extremely slow and 
%have a high memory usage for \code{x} 
%containing many elements.
%}
\value{a logical vector with the same length as \code{x} indicating
for each element in \code{x} if it is a maximal itemset.
}
\seealso{
\code{\link{is.superset}},
\code{\link{itemMatrix-class}},
\code{\link{itemsets-class}}
}
\references{
Mohammed J. Zaki, Srinivasan Parthasarathy, Mitsunori Ogihara, and Wei Li
(1997). \emph{New algorithms for fast discovery of association rules}.
Technical Report 651, Computer Science Department, University of Rochester,
Rochester, NY 14627.
}
\author{Michael Hahsler}
\keyword{models}
