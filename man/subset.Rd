\name{subset}
\alias{subset}
\alias{subset,itemMatrix-method}
\alias{subset,itemsets-method}
\alias{subset,itemMatrix-method}
\alias{subset,rules-method}
\title{Subsetting Itemsets, Rules and Transactions}
\description{
  Provides the generic function \code{subset} and S4 methods to subset
  associations or transactions (itemMatrix) which meet certain conditions
  (e.g., contains certain items or satisfies a minimum lift).
}
\usage{
subset(x, ...)

\S4method{subset}{itemMatrix}(x, subset, ...)

\S4method{subset}{itemsets}(x, subset, ...)

\S4method{subset}{rules}(x, subset, ...)

\S4method{subset}{itemMatrix}(x, subset, ...)
}
\arguments{
  \item{x}{object to be subsetted.}
  \item{subset}{logical expression indicating elements to keep.}
  \item{\dots}{further arguments to be passed to or from other methods.}
}
\details{
\code{subset} works on the rows/itemsets/rules of \code{x}. The 
expression given in \code{subset} will be evaluated using \code{x},
so the items (lhs/rhs/items) and the columns in the quality
data.frame can be directly referred to by their names.

Important operators to select itemsets containing items specified by their 
labels are \code{\%in\%} (select itemsets matching
\emph{any} given item),  \code{\%ain\%} (select only itemsets matching
\emph{all} given item) and  \code{\%pin\%} (\code{\%in\%} with partial
matching).
}
\value{
An object of the same class as \code{x} containing only the
elements which satisfy the conditions.
}
\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}},
  \code{\link{transactions-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")
rules <- apriori(Adult)

## select all rules with item "marital-status=Never-married" in 
## the right-hand-side and lift > 2
rules.sub <- subset(rules, subset = rhs \%in\% "marital-status=Never-married" 
    & lift > 2)

## use partial matching for all items corresponding to the variable
## "marital-status"
rules.sub <- subset(rules, subset = rhs \%pin\% "marital-status=")

## select only rules with items "age=Young" and "workclass=Private" in
## the left-hand-side
rules.sub <- subset(rules, subset = lhs \%ain\% 
    c("age=Young", "workclass=Private"))
}
\keyword{manip}
