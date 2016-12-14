\name{LIST}
\alias{LIST}
\alias{LIST,itemMatrix-method}
\alias{LIST,transactions-method}
\alias{LIST,tidLists-method}
\alias{coerce,ngCMatrix,list-method}
\title{List Representation for Objects Based on Class itemMatrix}
\description{
  Provides the generic function \code{LIST} and the S4 methods to create a list
  representation from objects based on \code{\linkS4class{itemMatrix}} (e.g.,
  \code{\linkS4class{transactions}}, \code{\linkS4class{tidLists}}, or
  \code{\linkS4class{itemsets}}).  These methods can be used for the coercion
  to a list.
}
\usage{
LIST(from, \dots)

\S4method{LIST}{itemMatrix}(from, decode = TRUE)

\S4method{LIST}{transactions}(from, decode = TRUE)

\S4method{LIST}{tidLists}(from, decode = TRUE)
}
\arguments{
  \item{from}{the object to be converted into a list.}
  \item{\dots}{further arguments.}
  \item{decode}{a logical controlling whether the items/transactions are
    decoded from the column numbers internally used by
    \code{\linkS4class{itemMatrix}} to the names stored in
    the object \code{from}.  The default behavior is to decode.
  }
}
\details{
  Using \code{LIST} with \code{decode = TRUE} is equivalent to 
  the standard coercion \code{as(x, "list")}.
  \code{LIST} returns the object \code{from} as a list of vectors.
  Each vector represents one row of the
  \code{\linkS4class{itemMatrix}} (e.g., items in a
  transaction or itemset).
}
\value{
  a list primitive.
}
\seealso{
  \code{\link{decode}},
  \code{\link{coerce,itemMatrix,list-method}},
  \code{\link{itemMatrix-class}},
  \code{\link{DATAFRAME}}
}
\author{Michael Hahsler}
\examples{
data(Adult)

### default coercions (same as as(Adult[1:5], "list"))
LIST(Adult[1:5])

### coercion without item decoding
LIST(Adult[1:5], decode = FALSE)
}
\keyword{manip}
