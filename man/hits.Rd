\name{hits}
\alias{hits}
\title{Computing Transaction Weights With HITS}
\description{
  Compute the hub weights of a collection of transactions using
  the HITS (hubs and authorities) algorithm.
}
\usage{
hits(data, iter = 16L, tol = NULL,
     type = c("normed", "relative", "absolute"), verbose = FALSE)
}
\arguments{
  \item{data}{an object of or coercible to class
    \code{\link{transactions}}.}
  \item{iter}{an integer value specifying the maximum number of iterations
    to use.}
  \item{tol}{convergence tolerance (default \code{FLT_EPSILON}).}
  \item{type}{a string value specifying the norming of the hub weights.
    For \code{"normed"} scale the weights to unit length (L2 norm), 
    and for \code{"relative"} to unit sum.}
  \item{verbose}{a logical specifying if progress and runtime
    information should be displayed.}
}
\details{
  Model a collection of transactions as a bipartite graph of hubs
  (transactions) and authorities (items) with unit arcs and free
  node weights. That is, a transaction weight is the sum of the
  (normalized) weights of the items and vice versa. The weights
  are estimated by iterating the model to a steady-state using
  a builtin convergence tolerance of \code{FLT_EPSILON} for
  (the change in) the norm of the vector of authorities.
}
\value{
  A \code{numeric} vector with transaction weights for \code{data}.
}
\references{
  K. Sun and F. Bai (2008). Mining Weighted Association Rules
  without Preassigned Weights. \emph{IEEE Transactions on Knowledge
  and Data Engineering}, 4 (30), 489--495.
}
\author{Christian Buchta}
\seealso{
  Class
  \code{\link{transactions}},
  function 
  \code{\link{weclat}}
}
\examples{
data(SunBai)

## calculate transaction weigths  
w <- hits(SunBai)
w

## add transaction weight to the dataset
transactionInfo(SunBai)[["weight"]] <- w
transactionInfo(SunBai)

## calulate regular item frequencies
itemFrequency(SunBai, weighted = FALSE)

## calulate weighted item frequencies
itemFrequency(SunBai, weighted = TRUE)
}
\keyword{models}
