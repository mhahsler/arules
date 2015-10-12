\name{tidLists-class}
\docType{class}
\alias{tidLists-class}
\alias{tidLists}
\alias{tidlists}
\alias{tidLists_or_NULL-class}
%%%
\alias{initialize,tidLists-method}
\alias{coerce,tidLists,matrix-method}
\alias{coerce,tidLists,list-method}
\alias{coerce,list,tidLists-method}
\alias{coerce,tidLists,ngCMatrix-method}
\alias{coerce,tidLists,transactions-method}
\alias{coerce,transactions,tidLists-method}
\alias{coerce,tidLists,itemMatrix-method}
\alias{coerce,itemMatrix,tidLists-method}
\alias{dim,tidLists-method}
\alias{dimnames,tidLists-method}
\alias{dimnames<-,tidLists,list-method}
\alias{labels,tidLists-method}
\alias{itemLabels,tidLists-method}
\alias{itemInfo,tidLists-method}
\alias{itemInfo<-,tidLists-method}
\alias{inspect,tidLists-method}
\alias{transactionInfo,tidLists-method}
\alias{t,tidLists-method}
\alias{c,tidLists-method}
\alias{show,tidLists-method}
\alias{summary,tidLists-method}
%
\alias{summary.tidLists-class}
\alias{show,summary.tidLists-method}

\title{Class ``tidLists'' --- Transaction ID Lists for Items/Itemsets}
\description{
  Transaction ID lists contains a set of lists.  Each list is associated with
  an item/itemset and stores the IDs of the transactions which support the
  item/itemset.  \code{tidLists} uses the class
  \code{\link[Matrix:ngCMatrix-class]{ngCMatrix}} to efficiently store the
  transaction ID lists as a sparse matrix.  Each column in the matrix
  represents one transaction ID list.
  
  \code{tidLists} can be used for different purposes.  For some operations
  (e.g., support counting) it is efficient to coerce a
  \code{\linkS4class{transactions}} database into \code{tidLists} where each
  list contains the transaction IDs for an item (and the support is given by
  the length of the list).
  
  The implementation of the Eclat mining algorithm (which uses transaction ID
  list intersection) can also produce transaction ID lists for the found
  itemsets as part of the returned \code{\linkS4class{itemsets}} object.  These
  lists can then be used for further computation.
}
\section{Objects from the Class}{
  Objects are created by Eclat if the \code{\link{eclat}} function is called
  with \code{tidLists = TRUE} in the \code{\linkS4class{ECparameter}} object,
  and returned as part of the mined \code{\linkS4class{itemsets}}.  Objects can
  also be created by coercion from an object of class
  \code{\linkS4class{transactions}} or by calls of the form
  \code{new("tidLists", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{object of class
      \code{\link[Matrix:ngCMatrix-class]{ngCMatrix}}.}
     \item{\code{itemInfo}:}{a data.frame to store item/itemset labels (see
	\code{\linkS4class{itemMatrix}} class).}
     \item{\code{transactionInfo}:}{a data.frame
       with vectors of the same length as the number of transactions. 
       Each vector can hold additional information e.g., store
       transaction IDs or user IDs for each transaction.}
   }
}
\section{Methods}{
 \describe{
 % \item{[}{\code{signature(x = "tidLists")};
 %   extracts parts (transaction ID vectors) from the tidLists. 
 %   The argument selects the item or itemset.}
  \item{coerce}{\code{signature(from = "tidLists", to = "ngCMatrix")};
      access the sparse matrix representation. In the
      \code{ngCMatrix} each column represents the transaction IDs
      for one item/itemset.}
  \item{coerce}{\code{signature(from = "tidLists", to = "list")}}
  \item{coerce}{\code{signature(from = "list", to = "tidLists")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "matrix")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "itemMatrix")}}
  \item{coerce}{\code{signature(from = "tidLists", to = "transactions")}}
  \item{coerce}{\code{signature(from = "itemMatrix", to = "tidLists")}; this also coerces from \code{transactions}.}
  \item{coerce}{\code{signature(from = "transactions", to = "tidLists")}}
  \item{c}{\code{signature(x = "tidLists")}; combine.}
  \item{dim}{\code{signature(x = "tidLists")};
      returns the dimensions of the sparse Matrix representing the 
      \code{tidLists}.}
  \item{dimnames, rownames, colnames}{\code{signature(x = "transactions")};
     returns row (items/itemsets) and column 
    (transactionIDs if available) names.}
  \item{labels}{\code{signature(x = "transactions")};
     returns the labels for the itemsets in each transaction 
     (see \code{itemMatrix}).}  
  \item{inspect}{ inspect the transaction ID lists.}
  \item{itemInfo}{ returns the slot \code{itemInfo}.}
  \item{itemLabels}{\code{signature(object = "tidLists")};
    returns the item labels as a character vector.}
  \item{labels}{\code{signature(x = "transactions")};
    returns the labels (transaction IDs).}
  \item{show}{\code{signature(object = "tidLists")}}
  \item{summary}{\code{signature(object = "tidLists")}}
  \item{transactionInfo}{\code{signature(x = "transactions")}:
    returns the slot \code{transactionInfo}.}
}
}
\seealso{
  \code{\link{[-methods}},
  \code{\link{LIST}},
  \code{\link{eclat}},
  \code{\link{image}},
  \code{\link{length}},
  \code{\link{size}},
  \code{\link[Matrix:ngCMatrix-class]{ngCMatrix}}(in \pkg{Matrix}),
  \code{\link{itemMatrix-class}},
  \code{\link{itemsets-class}},
  \code{\link{transactions-class}}
}
\author{Michael Hahsler}
\examples{
## Create transaction data set.
data <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("b","e"),
  c("b","c","e"),
  c("a","d","e"),
  c("a","c"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e")
  )
data <- as(data, "transactions")
data

## convert transactions to transaction ID lists
tl <- as(data, "tidLists")
tl 

inspect(tl)
dim(tl)
dimnames(tl)

## inspect visually
image(tl)

## mine itemsets with transaction ID lists
f <- eclat(data, parameter = list(support = 0, tidLists = TRUE))
tl2 <- tidLists(f)
inspect(tl2)
}
\keyword{classes}
