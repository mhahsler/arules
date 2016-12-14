\name{itemMatrix-class}
\docType{class}
\alias{itemMatrix-class}
\alias{itemMatrix}
%%%
\alias{initialize,itemMatrix-method}
\alias{summary.itemMatrix-class}
\alias{show,summary.itemMatrix-method}
\alias{print,summary.itemMatrix-method}
\alias{coerce,matrix,itemMatrix-method}
\alias{coerce,list,itemMatrix-method}
\alias{coerce,itemMatrix,matrix-method}
\alias{coerce,itemMatrix,ngCMatrix-method}
\alias{coerce,ngCMatrix,itemMatrix-method}
\alias{coerce,itemMatrix,list-method}
\alias{dim,itemMatrix-method}
\alias{dimnames,itemMatrix-method}
\alias{dimnames<-,itemMatrix,list-method}
\alias{\%in\%,itemMatrix,character-method}
\alias{\%in\%,itemMatrix,itemMatrix-method}
\alias{\%pin\%}
\alias{\%pin\%,itemMatrix,character-method}
\alias{\%ain\%}
\alias{\%ain\%,itemMatrix,character-method}
\alias{itemLabels<-,itemMatrix-method}
\alias{itemLabels,itemMatrix-method}
\alias{itemLabels<-}
\alias{itemLabels}
\alias{itemInfo<-,itemMatrix-method}
\alias{itemInfo,itemMatrix-method}
\alias{itemInfo<-}
\alias{itemInfo}
\alias{itemsetInfo<-,itemMatrix-method}
\alias{itemsetInfo,itemMatrix-method}
\alias{itemsetInfo<-}
\alias{itemsetInfo}
\alias{labels,itemMatrix-method}
\alias{labels}
\alias{nitems}
\alias{nitems,itemMatrix-method}
\alias{show,itemMatrix-method}
\alias{summary,itemMatrix-method}
%compat
\alias{t,ngCMatrix-method}
\alias{plot.itemMatrix}

\title{Class itemMatrix --- Sparse Binary Incidence Matrix to Represent Sets
  of Items}
\description{The \code{itemMatrix} class is the basic building block
  for transactions, itemsets and rules in package \pkg{arules}. 
  The class contains a sparse Matrix representation of items (a set of
  itemsets or transactions) and the corresponding item labels.
}
\details{
Sets of itemsets are represented as sparse binary matrices.
If you work with several itemMatrices at the same time (e.g.,
several transaction sets, lhs and rhs of a rule, etc.),
then the encoding (itemLabes and order of the items in the binary matrix) 
in the different itemMatrices is important and needs to conform.
See \code{\link{itemCoding}} to learn how to encode and recode itemMatrix objects.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("itemMatrix",
    ...)}.
  However, most of the time objects will be created by coercion from a
  matrix, list or data.frame.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class
      \code{\link[Matrix:ngCMatrix-class]{ngCMatrix}} 
      (from package \pkg{Matrix}) which stores 
      item occurrences in sparse representation.  Note that the 
      \code{ngCMatrix} is column-oriented and \code{itemMatrix} is
      row-oriented with each row representing an element (an itemset, a
      transaction, etc.).  As a result, the \code{ngCMatrix} in this
      slot is always a transposed version of the binary incidence matrix
      in \code{itemMatrix}.
    }
    \item{\code{itemInfo}:}{a data.frame which
      contains named vectors of the length equal to the number of
      elements in the set.  If the slot is not empty (contains no item
      labels), the first element in the data.frame must have the
      name \code{"labels"} and contain a character vector with the item
      labels used for representing an item.  In addition to the item
      labels, the data.frame can contain arbitrary named vectors
      (of the same length) to represent, e.g., variable names and values
      which were used to create the binary items or hierarchical
      category information associated with each item label.  
      %This
      %information is used for the \code{\%in\%} method in this
      %class.  The subset method in associations can use this method to
      %select subsets of associations containing items with matching
      %variable or category names.
    }
    \item{\code{itemsetInfo}:}{a data.frame which may contain additional 
        information for the rows (mostly representing itemsets) in the matrix. 
        }
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "itemMatrix")};
        expects \code{from} to be a binary matrix only containing 0s and 1s.}
    
    \item{coerce}{\code{signature(from = "itemMatrix", to = "matrix")}; coerces
      to a dense 0-1 matrix of \code{storage.mode} \code{"integer"} instead of 
    \code{"double"} to save memory.}

    \item{coerce}{\code{signature(from = "list", to = "itemMatrix")};
      \code{from} is a list of vectors. Each vector contains one 
      set/transaction/\ldots.}
      
    \item{coerce}{\code{signature(from = "itemMatrix", to = "list")}; see also
        the methods for \code{LIST}.}
    
    \item{coerce}{\code{signature(from = "itemMatrix", to = "ngCMatrix")};
      access the sparse matrix representation. Note, the \code{ngCMatrix} 
      contains a transposed from of the itemMatrix.}
    
    \item{coerce}{\code{signature(from = "ngCMatrix", to = "itemMatrix")};
      Note, the \code{ngCMatrix} has to be transposed with items as rows!}
    \item{c}{\code{signature(object = "itemMatrix")}; combine. }
    
    \item{dim}{\code{signature(x = "itemMatrix")};
      returns the dimensions of the itemMatrix.}
    \item{dimnames, rownames, colnames}{\code{signature(x = "itemMatrix")};
        returns row (itemsetID) and column (item) names.}
    \item{dimnames}{\code{signature(x = "itemMatrix")};
      returns dimnames.}
    \item{dimnames<-}{\code{signature(x = "itemMatrix", value = "list")};
      replace dimnames.}
    \item{\%in\%}{\code{signature(x = "itemMatrix", table = "character")};
      matches the strings in \code{table} against the item labels 
      in \code{x} and returns a logical vector indicating if
      a row (itemset) in \code{x} contains \emph{any} of the
      items specified in \code{table}.
      Note that there is a \code{\%in\%} method with 
      \code{signature(x = "itemMatrix", table = "character")}. This method is
      described in together with \code{\link{match}}.}
    \item{\%ain\%}{\code{signature(x = "itemMatrix", table = "character")}; 
        matches the strings in \code{table} against the item labels 
        in \code{x} and returns a logical vector indicating if
        a row (itemset) in \code{x} contains \emph{all} of the
        items specified in \code{table}.}
    \item{\%pin\%}{\code{signature(x = "itemMatrix", table = "character")}; 
        matches the strings in \code{table} against the item labels 
        in \code{x} (using \emph{partial} matching) and returns a 
        logical vector indicating if
        a row (itemset) in \code{x} contains \emph{any} of the
        items specified in \code{table}.}
    \item{itemLabels}{\code{signature(object = "itemMatrix")};
      returns the item labels used for encoding as a character vector.}
    \item{itemLabels<-}{\code{signature(object = "itemMatrix")};
      replaces the item labels used for encoding.}
    \item{itemInfo}{\code{signature(object = "itemMatrix")};
      returns the whole item/column information data.frame including
      labels.}
    \item{itemInfo<-}{\code{signature(object = "itemMatrix")};
      replaces the item/column info by a data.frame.}
    \item{itemsetInfo}{\code{signature(object = "itemMatrix")};
      returns the item set/row information data.frame.}
    \item{itemsetInfo<-}{\code{signature(object = "itemMatrix")};
      replaces the item set/row info by a data.frame.}
    \item{labels}{\code{signature(x = "transactions")};
      returns labels for the itemsets. The following arguments can 
      be used to customize the representation of the labels: 
      \code{itemSep}, 
      \code{setStart} and 
      \code{setEnd}.}
    \item{nitems}{\code{signature(x = "itemMatrix")}; returns the number 
      of items (number in columns) in the itemMatrix.}
    \item{show}{\code{signature(object = "itemMatrix")}}
    \item{summary}{\code{signature(object = "itemMatrix")}}
  }
}
\seealso{
  \code{\link{LIST}},
  \code{\link{c}},
  \code{\link{duplicated}},
  \code{\link{inspect}},
  \code{\link{is.subset}},
  \code{\link{is.superset}},
  \code{\link{itemFrequency}},
  \code{\link{itemFrequencyPlot}},
  \code{\link{itemCoding}},
  \code{\link{match}},
  \code{\link{length}},
  \code{\link{sets}},
  \code{\link{subset}},
  \code{\link{unique}},
  \code{\link{[-methods}},
  \code{\link{image}},
  \code{\link[Matrix]{ngCMatrix-class}} 
  (from \pkg{Matrix}),
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}}
}
\author{Michael Hahsler}
\examples{
set.seed(1234)
  
## Generate random data and coerce data to itemMatrix.
m <- matrix(runif(100000)>0.8, ncol=20)
dimnames(m) <- list(NULL, paste("item", c(1:20), sep=""))
i <- as(m, "itemMatrix")

## Get the number of elements (rows) in the itemMatrix.
length(i)

## Get first 5 elements (rows) of the itemMatrix as list.
as(i[1:5], "list")

## Get first 5 elements (rows) of the itemMatrix as matrix.
as(i[1:5], "matrix")

## Get first 5 elements (rows) of the itemMatrix as sparse ngCMatrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed!
as(i[1:5], "ngCMatrix")

## Get labels for the first 5 itemsets (first default and then with 
## custom formating)
labels(i[1:5])
labels(i[1:5], itemSep = " + ", setStart = "", setEnd = "")

## create itemsets from itemMatrix  
is <- new("itemsets", items = i[1:3])
inspect(is)

## create rules (rhs and lhs cannot share items so I use 
## itemSetdiff here). Also assign (random) support.
rules <- new("rules", lhs=itemSetdiff(i[4:6],i[1:3]), rhs=i[1:3],
  quality = data.frame(support = runif(3)))
inspect(rules) 
}
\keyword{classes}
