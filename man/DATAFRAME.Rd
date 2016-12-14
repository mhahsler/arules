\name{DATAFRAME}
\alias{DATAFRAME}
\alias{DATAFRAME,rules-method}
\alias{DATAFRAME,itemsets-method}
\alias{DATAFRAME,itemMatrix-method}
\title{Data.frame Representation for arules Objects}
\description{
  Provides the generic function \code{DATAFRAME} and the S4 methods to 
  create a data.frame
  representation from some arules objects. 
  based on \code{\linkS4class{rules}}.
  These methods are used for the coercion
  to a data.frame, but offers more control over the coercion process 
  (item separators, etc.).
}
\usage{
DATAFRAME(from, \dots)
}
\arguments{
  \item{from}{the object to be converted into a data.frame (\code{\linkS4class{rules}}, \code{\linkS4class{itemsets}}, \code{\linkS4class{transactions}}).}
  \item{\dots}{further arguments.}
}
\details{
  Using \code{DATAFRAME} is equivalent to 
  the standard coercion \code{as(x, "data.frame")}.
  However, for rules, the argument \code{separate = TRUE} will prodiuce
  separate columns for the LHS and the RHS of the rule. Furthermore,
  the arguments 
  \code{itemSep}, 
  \code{setStart}, 
  \code{setEnd} (and \code{ruleSep} for \code{separate = FALSE}) 
  will be passed on to the \code{label} method.
}
\value{
  a data.frame.
}
\seealso{
  \code{\link{coerce,rules,data.frame-method}},
  \code{\link{coerce,itemsets,data.frame-method}},
  \code{\link{coerce,transactions,data.frame-method}},
  \code{\link{labels,itemMatrix-method}},
  \code{\link{LIST}}
}
\author{Michael Hahsler}
\examples{
data(Adult)
  
DATAFRAME(head(Adult))
DATAFRAME(head(Adult), setStart = '', itemSep = ' + ', setEnd = '')

rules <- apriori(Adult, 
  parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
rules <- head(rules, by = "conf")


### default coercions (same as as(rules, "data.frame"))
DATAFRAME(rules)

DATAFRAME(rules, separate = TRUE)
DATAFRAME(rules, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '')
}
\keyword{manip}
