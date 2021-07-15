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
\alias{\%pin\%,itemMatrix,character-method}
\alias{\%ain\%,itemMatrix,character-method}
\alias{\%oin\%,itemMatrix,character-method}
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
\alias{toLongFormat}
\alias{toLongFormat,itemMatrix-method}
\alias{plot.itemMatrix}

\title{Class itemMatrix --- Sparse Binary Incidence Matrix to Represent Sets
  of Items}
\description{The \code{itemMatrix} class is the basic building block
  for transactions, itemsets and rules in package \pkg{arules}. 
  The class contains a sparse Matrix representation of items (a set of
  itemsets or transactions) and the corresponding item labels.
}
\details{
Sets of itemsets (or transactions) are represented as a compressed sparse binary matrix.
Columns represent items and rows are the set/transactions. In the compressed form, each itemset is a vector of column indices (called item IDs) representing the items.

\bold{Note:} If you work with several itemMatrices at the same time (e.g.,
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
    \item{\%oin\%}{\code{signature(x = "itemMatrix", table = "character")}; 
        matches the strings in \code{table} against the item labels 
        in \code{x} and returns a logical vector indicating if
        a row (itemset) in \code{x} contains \emph{only}
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
    \item{toLongFormat}{\code{signature(object = "itemMatrix")}; convert the transactions to long format (a data.frame with two columns, tid and item). Column names can be specified as a character vector of length 2 called \code{cols}.}  
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
  
## Generate a logical matrix with 5000 random itemsets for 20 items
m <- matrix(runif(5000*20)>0.8, ncol=20, 
            dimnames = list(NULL, paste("item", c(1:20), sep="")))
head(m)

## Coerce the logical matrix into an itemMatrix object
imatrix <- as(m, "itemMatrix")
imatrix

## An itemMatrix contains a set of itemsets (each row is an itemset). 
## The length of the set is the number of rows. 
length(imatrix)

## The sparese matrix also has regular matrix  dimensions.
dim(imatrix)
nrow(imatrix)
ncol(imatrix)

## Subsetting: Get first 5 elements (rows) of the itemMatrix. This can be done in 
## several ways.
imatrix[1:5]            ### get elements 1:5
imatrix[1:5, ]          ### Matrix subsetting for rows 1:5
head(imatrix, n = 5)    ### head()

## Get first 5 elements (rows) of the itemMatrix as list.
as(imatrix[1:5], "list")

## Get first 5 elements (rows) of the itemMatrix as matrix.
as(imatrix[1:5], "matrix")

## Get first 5 elements (rows) of the itemMatrix as sparse ngCMatrix.
## Warning: For efficiency reasons, the ngCMatrix is transposed!
as(imatrix[1:5], "ngCMatrix")

## Get labels for the first 5 itemsets (first default and then with 
## custom formating)
labels(imatrix[1:5])
labels(imatrix[1:5], itemSep = " + ", setStart = "", setEnd = "")

## Create itemsets manually from an itemMatrix. Itemsets contain items in the form of 
## an itemMatrix and additional quality measures (not supplied in the example).
is <- new("itemsets", items = imatrix)
is
inspect(head(is, n = 3))


## Create rules manually. I use imatrix[4:6] for the lhs of the rules and 
## imatrix[1:3] for the rhs. Rhs and lhs cannot share items so I use 
## itemSetdiff here. I also assign missing values for the quality measures support
## and confidence.
rules <- new("rules", 
             lhs = itemSetdiff(imatrix[4:6], imatrix[1:3]), 
             rhs = imatrix[1:3],
             quality = data.frame(support = c(NA, NA, NA), 
                                  confidence =  c(NA, NA, NA)
          ))
rules
inspect(rules)

## Manually create a itemMatrix with an item encoding that matches imatrix (20 items in order
## item1, item2, ..., item20)
itemset_list <- list(c("item1","item2"),
                     c("item3"))

imatrix_new <- encode(itemset_list, itemLabels = imatrix)
imatrix_new
compatible(imatrix_new, imatrix)
}
\keyword{classes}
