\name{abbreviate}
\Rdversion{1.1}
\alias{abbreviate}
\alias{abbreviate.transactions}
\alias{abbreviate.rules}
\alias{abbreviate.itemsets}
\alias{abbreviate.itemMatrix}
\alias{abbreviate.tidLists}
\title{Abbreviate function for item labels in transactions, 
itemMatrix and associations}
\description{
Methods to abbreviate long item labels in transactions, associations (rules and itemsets) and transaction ID lists. Note that \code{abbreviate} is not a
  generic and this \pkg{arules} defines a generic with the R's abbreviate
  as the default.
}
\usage{
abbreviate(...)
\method{abbreviate}{transactions}(data, minlength = 4, ..., method = "both.sides")
\method{abbreviate}{rules}(data, minlength = 4, ..., method = "both.sides")
\method{abbreviate}{itemsets}(data, minlength = 4, ..., method = "both.sides")
\method{abbreviate}{itemMatrix}(data, minlength = 4, ..., method = "both.sides")
\method{abbreviate}{tidLists}(data, minlength = 4, ..., method = "both.sides")
}
\arguments{
  \item{data}{ an object of class "transactions", "itemMatrix", "itemsets",
    "rules" or "tidLists". }
  \item{minlength}{ number of characters allowed in abbreviation}
  \item{method}{ apply to level and value (both.sides) }
  \item{\dots}{ further arguments passed on to the default abbreviation function. }
}
                            
\author{
Sudheer Chelluboina and Michael Hahsler based on code by Martin Vodenicharov.
}
\seealso{
  \code{\link[base]{abbreviate}} in base.
}
\examples{
data(Adult)
inspect(head(Adult, 1))

Adult_abbr <- abbreviate(Adult, 15)
inspect(head(Adult_abbr, 1))
}
\keyword{ manip }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
