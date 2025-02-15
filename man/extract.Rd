% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract}
\alias{extract}
\alias{"[-methods"}
\alias{[,itemMatrix,ANY,ANY,ANY-method}
\alias{[,transactions,ANY,ANY,ANY-method}
\alias{[,tidLists,ANY,ANY,ANY-method}
\alias{[,rules,ANY,ANY,ANY-method}
\alias{[,itemsets,ANY,ANY,ANY-method}
\title{Methods for "[": Extraction or Subsetting arules Objects}
\usage{
\S4method{[}{itemMatrix,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{transactions,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{tidLists,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{rules,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{itemsets,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)
}
\arguments{
\item{x}{an object of class \link{itemMatrix}, \link{transactions} or \link{associations}.}

\item{i}{select rows/sets using an integer vector containing row
numbers or a logical vector.}

\item{j}{select columns/items using an integer vector containing column
numbers (i.e., item IDs), a logical vector or a vector of strings containing parts of item labels.}

\item{...}{further arguments are ignored.}

\item{drop}{ignored.}
}
\description{
Methods for \code{"["}, i.e., extraction or subsetting for arules objects.
}
\examples{
data(Adult)
Adult

## select first 10 transactions
Adult[1:10]

## select first 10 items for first 100 transactions
Adult[1:100, 1:10]

## select the first 100 transactions for the items containing
## "income" or "age=Young" in their labels
Adult[1:100, c("income=small", "income=large", "age=Young")]
}
\seealso{
Other associations functions: 
\code{\link{abbreviate}()},
\code{\link{associations-class}},
\code{\link{c}()},
\code{\link{duplicated}()},
\code{\link{inspect}()},
\code{\link{is.closed}()},
\code{\link{is.generator}()},
\code{\link{is.maximal}()},
\code{\link{is.redundant}()},
\code{\link{is.significant}()},
\code{\link{is.superset}()},
\code{\link{itemsets-class}},
\code{\link{match}()},
\code{\link{rules-class}},
\code{\link{sample}()},
\code{\link{sets}},
\code{\link{size}()},
\code{\link{sort}()},
\code{\link{unique}()}

Other itemMatrix and transactions functions: 
\code{\link{abbreviate}()},
\code{\link{c}()},
\code{\link{crossTable}()},
\code{\link{duplicated}()},
\code{\link{hierarchy}},
\code{\link{image}()},
\code{\link{inspect}()},
\code{\link{is.superset}()},
\code{\link{itemFrequency}()},
\code{\link{itemFrequencyPlot}()},
\code{\link{itemMatrix-class}},
\code{\link{match}()},
\code{\link{merge}()},
\code{\link{random.transactions}()},
\code{\link{sample}()},
\code{\link{sets}},
\code{\link{size}()},
\code{\link{supportingTransactions}()},
\code{\link{tidLists-class}},
\code{\link{transactions-class}},
\code{\link{unique}()}
}
\author{
Michael Hahsler
}
\concept{associations functions}
\concept{itemMatrix and transactions functions}
\keyword{array}
