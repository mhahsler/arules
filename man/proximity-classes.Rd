\name{proximity-classes}
\docType{class}
%
\alias{ar_similarity-class}
%
\alias{ar_cross_dissimilarity-class}
%
\alias{dist-class}

\title{Classes ``dist'', ``ar\_cross\_dissimilarity'' and ``ar\_similarity'' --- Proximity Matrices}
\description{
  Simple classes to represent proximity matrices.  For compatibility with
  clustering functions in \code{R}, we represent dissimilarities as the
  \code{S3} class \code{dist}.  For cross-dissimilarities and similarities, we
  provide the \code{S4} classes \code{ar_cross_dissimilarities} and
  \code{ar_similarities}.
}
\section{Objects from the Class}{
  \code{dist} objects are the result of calling the method 
  \code{\link{dissimilarity}} with one argument or any \code{R} function
  returning a \code{S3 dist} object.
  
  \code{ar_cross_dissimilarity} objects are the result of calling the method
  \code{\link{dissimilarity}} with two arguments, by  calls of the form
  \code{new("similarity", ...)}, or by coercion from matrix.

  \code{ar_similarity} objects are the result of calling the method
  \code{\link{affinity}}, by  calls of the form  \code{new("similarity", ...)},
  or by coercion from matrix.
}
\section{Slots}{
The \code{S4} classes have a \code{method} slot which contains the
type of measure used for calculation.
}
%\section{Methods}{
%  \describe{
%  }
%}
\seealso{
  \code{\link[stats]{dist}} (in package \code{stats}),
  \code{\link{dissimilarity}},
  \code{\link{affinity}}.
}
\author{Michael Hahsler}
%\examples{
%}
\keyword{classes}
