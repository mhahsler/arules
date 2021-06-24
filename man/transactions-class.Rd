\name{transactions-class}
\docType{class}
\alias{transactions-class}
\alias{transactions}
%%%
\alias{initialize,transactions-method}
\alias{coerce,matrix,transactions-method}
\alias{coerce,list,transactions-method}
\alias{coerce,transactions,matrix-method}
\alias{coerce,transactions,list-method}
\alias{coerce,data.frame,transactions-method}
\alias{coerce,transactions,data.frame-method}
\alias{coerce,ngCMatrix,transactions-method}
%\alias{image,transactions-method}
\alias{labels,transactions-method}
\alias{items,transactions-method}
\alias{dimnames,transactions-method}
\alias{dimnames<-,transactions,list-method}
\alias{show,transactions-method}
\alias{transactionInfo<-,transactions-method}
\alias{transactionInfo,transactions-method}
\alias{transactionInfo<-}
\alias{transactionInfo}
\alias{t,transactions-method}
%
\alias{summary,transactions-method}
\alias{summary.transactions-class}
\alias{show,summary.transactions-method}

\title{Class transactions --- Binary Incidence Matrix for
  Transactions}
\description{
  The \code{transactions} class represents transaction data used for
  mining itemsets or rules.  
}
\section{Objects from the Class}{
  Objects are created by coercion from objects of other classes 
  (see Examples section) or by
  calls of the form 
  
  \code{new("transactions", ...)}
  
  or by using the constructor function 
  
  \code{transactions(x, itemLabels = NULL, transactionInfo = NULL)}.
  
  See Examples Section for creating transactions from data.
}
\section{Slots}{
  \describe{
    \item{\code{itemsetInfo}:}{a data.frame
      with one row per transaction (each transaction is considered an 
      itemset).  The data.frame
      can hold columns with additional information, e.g.,
      transaction IDs or user IDs for each transaction. \bold{Note:} this
      slot is inherited from class \code{\linkS4class{itemMatrix}}, but
      should be accessed in transactions with the 
      method \code{transactionInfo()}.}
    \item{\code{data}:}{object of class
      \code{\link[Matrix:ngCMatrix-class]{ngCMatrix}} to store the
      binary incidence matrix (see
      \code{\linkS4class{itemMatrix}} class)} 
    \item{\code{itemInfo}:}{a data.frame to store 
      item labels (see \code{\linkS4class{itemMatrix}} class)}
  }
}
\section{Extends}{
  Class \code{\linkS4class{itemMatrix}}, directly.
}

\details{
Transactions are a direct extension of class
\code{\linkS4class{itemMatrix}} to store a binary incidence
matrix, item labels, and optionally transaction IDs and user IDs.

Transactions can be created from a list
containing transactions or a matrix or data.frames
using 
\itemize{
\item the constructor function \code{transactions(x, itemLabels = NULL, transactionInfo = NULL)}, or 
\item S4 coercion with \code{as(x, "transactions")}).}

\code{itemLabels} and
\code{transactionInfo} are by default created from information in \code{x} (e.g., from row and column names). In the constructor function, the user can specify for itemLabels a vector of all possible item labels (character)
or another transactions object to copy the item coding (see \code{\link{itemCoding}} for details).

Note that you will need to prepare your data first (see coercion methods in the
Methods Section and the Example Section below for details on the needed format). 

\bold{Continuous variables:} Association rule mining can only use items and does not work with continuous variables. Continuous variables need to be discretized first. An item resulting from discretization might be \emph{age>18} and the column contains only \code{TRUE} or \code{FALSE}. Alternatively it can be a factor with levels \emph{age<=18}, \emph{50=>age>18} and \emph{age>50}. These will be automatically converted into 3 items, one for each level. Have a look at the function \code{\link{discretize}} for automatic discretization.

\bold{Logical variables:} A logical variable describing a person could be \emph{tall} indicating if the person is tall using the values \code{TRUE} and \code{FALSE}. The fact that the person is tall would be encoded in the transaction containing the item \emph{tall} while not tall persons would not have this item. Therefore, for logical variables, the \code{TRUE} value is converted into an item with the name of the variable and for the \code{FALSE} values no item is created. 

\bold{Factors:} The function also can convert columns with nominal values (i.e., factors) into a series of binary items (one for each level constructed as \code{`variable name`=`level`}). Note that nominal variables need to be encoded as factors (and not characters or numbers). This can be done with 

\code{data[,"a_nominal_var"] <- factor(data[,"a_nominal_var"])}.

Complete examples for how to prepare data can be found in the man pages for \code{\link{Income}} and
\code{\link{Adult}}.

Transactions are represented as sparse binary matrices of class
\code{\link{itemMatrix}}. If you work with several transaction sets at the 
same time, then the encoding (order of the items in the binary matrix) in the different sets is important.
See \code{\link{itemCoding}} to learn how to encode and recode transaction sets. 
}

\section{Methods}{
  \describe{
  %  \item{[}{\code{signature(x = "transactions")};
  %    extracts a subset from the incidence matrix.  The first argument
  %    extracts transactions and the second argument can be used to
  %    extract a subset of items (using item IDs)}
    \item{coerce}{\code{signature(from = "matrix", to = "transactions")};
      produces a transactions data set from a binary incidence matrix.
      The column names are used as item labels and the row names are
      stores as transaction IDs.}
    \item{coerce}{\code{signature(from = "transactions", to = "matrix")};
      coerces the transactions data set into a binary incidence matrix.
    }
    \item{coerce}{\code{signature(from = "list", to = "transactions")};
      produces a transactions data set from a list.  The names of the
      items in the list are used as item labels.
    }
    \item{coerce}{\code{signature(from = "transactions", to = "list")};
      coerces the transactions data set into a list of transactions. 
      Each transaction is a vector of character strings (names of the 
      contained items).
    }
    \item{coerce}{\code{signature(from = "data.frame", to = "transactions")}; 
      recodes the data frame containing only categorical variables (factors)
      or logicals all into a binary transaction data set. For binary variables
      only TRUE values are converted into items and the item label is the
      variable name. For factors, a dummy item for each level is 
      automatically generated. Item labels are generated by concatenating 
      variable names and levels with \samp{"="}.
      The original variable names and levels are stored in the itemInfo 
      data frame
      as the components \code{variables} and \code{levels}.
      Note that \code{NAs} are ignored (i.e., do not generate an item).}
    \item{coerce}{\code{signature(from = "transactions", to = "data.frame")}; 
      represents the set of transactions in a printable form 
      as a data.frame. 
      Note that this does not reverse coercion from data.frame 
      to \code{transactions}.}
    \item{coerce}{\code{signature(from = "ngCMatrix", to = "transactions")};              
      Note that the data is stored transposed in the ngCMatrix. Items are 
      stored as rows and transactions are columns!}
    \item{dimnames, rownames, colnames}{\code{signature(x = "transactions")};
      returns row (transactionID) and column (item) names.}
    \item{items}{\code{signature(x = "transactions")};
      returns the items in the transactions as an 
      \code{\linkS4class{itemMatrix}}.}
    \item{labels}{\code{signature(x = "transactions")};
      returns the labels for the itemsets in each transaction 
      (see \code{itemMatrix}).} 
    \item{transactionInfo<-}{\code{signature(x = "transactions")};
      replaces the transaction information with a new data.frame.}
    \item{transactionInfo}{\code{signature(x = "transactions")};
      returns the transaction information as a data.frame.}
    \item{show}{\code{signature(object = "transactions")}}
    \item{summary}{\code{signature(object = "transactions")}}
  }
}
\seealso{
  \code{\link{[-methods}},
  \code{\link{discretize}},
  \code{\link{LIST}},
  \code{\link{write}},
  \code{\link{c}},
  \code{\link{image}},
  \code{\link{inspect}},
  \code{\link{itemCoding}},
  \code{\link{read.transactions}},
  \code{\link{random.transactions}},
  \code{\link{sets}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
## Example 1: creating transactions form a list (each element is a transaction)
a_list <- list(
      c("a","b","c"),
      c("a","b"),
      c("a","b","d"),
      c("c","e"),
      c("a","b","d","e")
      )

## Set transaction names
names(a_list) <- paste("Tr",c(1:5), sep = "")
a_list

## Use the constructor to create transactions
## Note: S4 coercion does the same trans1 <- as(a_list, "transactions")
trans1 <- transactions(a_list)
trans1

## Analyze the transactions
summary(trans1)
image(trans1)

## Example 2: creating transactions from a 0-1 matrix with 5 transactions (rows) and 
##            5 items (columns)
a_matrix <- matrix(c(
  1, 1, 1, 0, 0,
	1, 1, 0, 0, 0,
	1, 1, 0, 1, 0,
	0, 0, 1, 0, 1,
	1, 1, 0, 1, 1
  ), ncol = 5)

## Set item names (columns) and transaction labels (rows)
colnames(a_matrix) <- c("a", "b", "c", "d", "e")
rownames(a_matrix) <- paste("Tr", c(1:5), sep = "")

a_matrix

## Create transactions
trans2 <- transactions(a_matrix)
trans2
inspect(trans2)

## Example 3: creating transactions from data.frame
a_df <- data.frame(
	age   = as.factor(c(6, 8, NA, 9, 16)), 
	grade = as.factor(c("A", "C", "F", NA, "C")),
  pass  = c(TRUE, TRUE, FALSE, TRUE, TRUE))  
## Note: factors are translated differently than logicals and NAs are ignored
a_df

## Create transactions
trans3 <- transactions(a_df) 
inspect(trans3)

## Note that coercing the transactions back to a data.frame does not recreate the 
## original data.frame, but represents the transactions as sets of items
as(trans3, "data.frame")

## Example 4: creating transactions from a data.frame with 
## transaction IDs and items (by converting it into a list of transactions first) 
a_df3 <- data.frame(
  TID = c(1, 1, 2, 2, 2, 3), 
  item=c("a", "b", "a", "b", "c", "b")
  )
a_df3
trans4 <- transactions(split(a_df3[, "item"], a_df3[, "TID"]))
trans4
inspect(trans4)

## Note: This is very slow for large datasets. It is much faster to 
## read transactions using read.transactions() with format = "single".
## This can be done using an anonymous file.
write.table(a_df3, file = tmp <- file(), row.names = FALSE)
trans4 <- read.transactions(tmp, format = "single",
  header = TRUE, cols = c("TID", "item"))
close(tmp)
inspect(trans4)

## Example 5: create transactions from a dataset with numeric variables
## using discretization.
data(iris)

irisDisc <- discretizeDF(iris)
head(irisDisc)

trans5 <- transactions(irisDisc)
trans5
inspect(head(trans5))

## Note, creating transactions without discretizing numeric variables will apply the 
## default discretization and also create a warning.


## Example 6: create transactions manually (with the same item coding as in trans5)
trans6 <- transactions(
  list(
    c("Sepal.Length=[4.3,5.4)", "Species=setosa"),
    c("Sepal.Length=[4.3,5.4)", "Species=setosa")
  ), itemLabels = trans5)
trans6  

inspect(trans6)
}
\keyword{classes}
