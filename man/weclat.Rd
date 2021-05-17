\name{weclat}
\alias{weclat}
\alias{WARM}
\alias{warm}
\title{Mining Associations from Weighted Transaction Data with Eclat (WARM)}
\description{
  Find frequent itemsets with the Eclat algorithm. This implementation
  uses optimized tidlist joins and transaction weights to implement 
  weighted association rule mining (WARM).
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
  \code{weight} in \code{\link{transactionInfo}}.
  
  The weighted support of an itemset is the sum of the weights of the 
  transactions that contain the itemset. An itemset is frequent if
  its weighted support is equal or greater than the threshold specified
  by \code{support} (assuming that the weights sum to one).

  Note that ECLAT only mines (weighted) frequent itemsets. Weighted
  association rules can be created using \code{\link{ruleInduction}}.
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
  memory. 
}
\seealso{
  Class 
  \code{\link{transactions}},
  function 
  \code{\link{ruleInduction}},
  \code{\link{eclat}}
}
\references{
G.D. Ramkumar, S. Ranka, and S. Tsur (1998). 
Weighted Association Rules: Model and Algorithm, 
\emph{Proceedings of ACM SIGKDD}
}
\examples{
## Example 1: SunBai data
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

## Example 2: Find association rules in weighted data
trans <-  list(
    c("A", "B", "C", "D", "E"),
    c("C", "F", "G"),
    c("A", "B"),
    c("A"),
    c("C", "F", "G", "H"),
    c("A", "G", "H")
)

## convert list to transactions
trans <-  transactions(trans)

## add weight information
transactionInfo(trans) <- data.frame(weights = c(5, 10, 6, 7, 5, 1))
inspect(trans)

## mine weightd support itemsets 
s <- weclat(trans, parameter = list(support = 0.3),
		       control = list(verbose = TRUE))
inspect(sort(s))

## create association rules
r <- ruleInduction(s, confidence = .5)
inspect(r)
}
\keyword{models}
