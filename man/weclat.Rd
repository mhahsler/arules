\name{weclat}
\alias{weclat}
\title{Mining Associations from Weighted Transaction Data with Eclat}
\description{
  Find frequent itemsets with the Eclat algorithm. This implementation
  uses optimized tidlist joins and transaction weights.
}
\usage{
weclat(data, parameter = NULL, control = NULL)
}
\arguments{
  \item{data}{an object that can be coerced into an object of class
  \code{\link{transactions}}.}
  \item{parameter}{an object of class
  \code{\link{ASparameter}} (default
    values: \code{support = 0.1}, \code{minlen = 1L}, and
    \code{maxlen = 5L}) or a named list with corresponding components.}
  \item{control}{an object of class
    \code{\link{AScontrol}} (default
    values: \code{verbose = TRUE}) or a named list with corresponding
    components.}
}
\details{
  Transaction weights are stored in the transaction as a column called 
  \code{weight} in \code{transactionInfo()}.
  
  The weighted support of an itemset is the sum of the weights of the 
  transactions that contain the itemset. An itemset is frequent if
  its weighted support is equal or greater than the threshold specified
  by \code{support} (assuming that the weights sum to one).
}
\value{
  Returns an object of class
  \code{\link{itemsets}}. Note that weighted support is returned in
  \code{quality} as column \code{support}. 
}
\author{Christian Buchta}
\note{
  The C code can be interrupted by CTRL-C. This is convenient
  but comes at the price that the code cannot clean up its internal
  memory. This is a known problem of the R API and the workaround
  is to defer the cleanup to the next function call.
}
\seealso{
  Class 
  \code{\link{transactions}},
  function 
  \code{\link{ruleInduction}},
  \code{\link{eclat}}
}
\examples{
data(SunBai)
SunBai

## weights are stored in transactionInfo
transactionInfo(SunBai)

## mine weighted support itemsets using transaction support in SunBai
s <- weclat(SunBai, parameter = list(support = 0.3),
		       control = list(verbose = TRUE))
inspect(sort(s))

## create rules using weighted support (satisfying a minimum 
## weighted confidence of 90%).
r <- ruleInduction(s, confidence = .9)
inspect(r)
}
\keyword{models}
