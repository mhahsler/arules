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
  calls of the form \code{new("transactions", ...)}.
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
Transactions are  a direct extension of class
\code{\linkS4class{itemMatrix}} to store a binary incidence
matrix, item labels, and optionally transaction IDs and user IDs.

Transactions can be created by coercion from lists
containing transactions, but also from matrix and data.frames.
However, you will need to prepare your data first (see coercion methods in the
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
## example 1: creating transactions form a list
a_list <- list(
      c("a","b","c"),
      c("a","b"),
      c("a","b","d"),
      c("c","e"),
      c("a","b","d","e")
      )

## set transaction names
names(a_list) <- paste("Tr",c(1:5), sep = "")
a_list

## coerce into transactions
trans1 <- as(a_list, "transactions")

## analyze transactions
summary(trans1)
image(trans1)

## example 2: creating transactions from a matrix
a_matrix <- matrix(c(
  1,1,1,0,0,
	1,1,0,0,0,
	1,1,0,1,0,
	0,0,1,0,1,
	1,1,0,1,1
  ), ncol = 5)

## set dim names
dimnames(a_matrix) <- list(c("a","b","c","d","e"),
	paste("Tr",c(1:5), sep = ""))

a_matrix

## coerce
trans2 <- as(a_matrix, "transactions")
trans2
inspect(trans2)

## example 3: creating transactions from data.frame
a_df <- data.frame(
	age   = as.factor(c(6, 8, NA, 9, 16)), 
	grade = as.factor(c("A", "C", "F", NA, "C")),
  pass  = c(TRUE, TRUE, FALSE, TRUE, TRUE))  
## note: factors are translated differently to logicals and NAs are ignored
a_df

## coerce
trans3 <- as(a_df, "transactions") 
inspect(trans3)
as(trans3, "data.frame")

## example 4: creating transactions from a data.frame with 
## transaction IDs and items (by converting it into a list of transactions first) 
a_df3 <- data.frame(
  TID = c(1,1,2,2,2,3), 
  item=c("a","b","a","b","c", "b")
  )
a_df3
trans4 <- as(split(a_df3[,"item"], a_df3[,"TID"]), "transactions")
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

## example 5: create transactions from a dataset with numeric variables
## using discretization.
data(iris)

irisDisc <- discretizeDF(iris)
head(irisDisc)
trans5 <- as(irisDisc, "transactions")
trans5
inspect(head(trans5))

## example 6: create transactions manually (with the same items as in trans5)
trans6 <- as(encode(list(
  c("Sepal.Length=[4.3,5.4)", "Species=setosa"),
  c("Sepal.Length=[4.3,5.4)", "Species=setosa")
  ), itemLabels = itemLabels(trans5)
), "transactions")

inspect(trans6)
}
\keyword{classes}
