\name{[-methods}
\docType{methods}
\alias{[-methods}
%
\alias{[,rules,ANY,ANY,ANY-method}
\alias{[,itemsets,ANY,ANY,ANY-method}
\alias{[,transactions,ANY,ANY,ANY-method}
\alias{[,itemMatrix,ANY,ANY,ANY-method}
%\alias{[,ngCMatrix,ANY,ANY,ANY-method}  % internal method
\alias{[,tidLists,ANY,ANY,ANY-method}
\alias{[,rules-method}
\alias{[,itemsets-method}
\alias{[,transactions-method}
\alias{[,itemMatrix-method}
%\alias{[,ngCMatrix-method}  % internal method
\alias{[,tidLists-method}
%
\alias{[,Matrix,ANY,ANY,ANY-method} % Namespace import problem
\alias{[<-,Matrix,ANY,ANY,ANY-method} % Namespace import problem
\alias{[<-,Matrix,missing,missing,ANY-method} % Namespace import problem
\alias{[,Matrix,lMatrix,missing,ANY-method} % Namespace import problem
\alias{[,Matrix,logical,missing,ANY-method} % Namespace import problem
%
\title{Methods for "[": Extraction or Subsetting in Package 'arules'}
\description{
  Methods for \code{"["}, i.e., extraction or subsetting in package
  \pkg{arules}. Subsetting can be done by integers containing column/row
  numbers, vectors of logicals or strings containing parts of item labels.

}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "itemMatrix", i = "ANY", j = "ANY", drop= "ANY")};  extracts parts of an \code{itemMatrix}.  
	The first argument selects rows (e.g., transactions or rules) 
	and the second argument selects columns (items).
	Either argument can be omitted to select all rows or columns.}
    \item{[}{\code{signature(x = "itemsets", i = "ANY", j = "ANY", drop= "ANY")}; extracts a subset of itemsets and the associated quality measures.  
	\code{j} has to be missing. }
    \item{[}{\code{signature(x = "rules", i = "ANY", j = "ANY", drop= "ANY")}; extracts a subset of rules and the associated quality measures.
	 \code{j} has to be missing. }
    \item{[}{\code{signature(x = "transactions", i = "ANY", j = "ANY", drop= "ANY")}; extracts a subset of transactions/items from a transactions object
	(a binary incidence matrix). 
	\code{i} and \code{j}  can be numeric where
	\code{i} selects transactions and 
	\code{j} selects items. }
    \item{[}{\code{signature(x = "tidLists", i = "ANY", j = "ANY", drop= "ANY")}; extracts parts (transaction ID vectors) from \code{tidLists}.
          \code{i} selects the items or itemsets 
	   and \code{j} selects transactions in the lists.}
}}

\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}},
  \code{\link{transactions-class}},
  \code{\link{tidLists-class}}

}
\author{Michael Hahsler}
\examples{
data(Adult)
Adult

## select first 10 transactions
Adult[1:10]

## select first 10 items for first 100 transactions
Adult[1:100, 1:10]

## select the first 100 transactions for the items containing
## "income" or "age=Young" in their labels 
Adult[1:100, c("income=small", "income=large" ,"age=Young")]
}
%\keyword{methods}
\keyword{array}
