\name{itemCoding}
\alias{itemCoding}
\alias{itemcoding}
\alias{decode}
\alias{decode,numeric-method}
\alias{decode,list-method}
\alias{encode}
\alias{encode,numeric-method}
\alias{encode,list-method}
\alias{encode,character-method}
\alias{recode}
\alias{recode,itemMatrix-method}
\title{Item Coding --- Conversion between Item Labels and Column IDs}
\description{
  Provides the generic functions and the S4 methods to translate between the binary representation in the itemMatrix format (used in transactions, rules and itemsets), item labels and numeric item IDs (i.e., the column numbers in the binary representation).
}
\usage{
encode(x, \ldots)
\S4method{encode}{list}(x, itemLabels, itemMatrix = TRUE)
\S4method{encode}{character}(x, itemLabels, itemMatrix = TRUE)
\S4method{encode}{numeric}(x, itemLabels, itemMatrix = TRUE)

recode(x, \ldots)
\S4method{recode}{itemMatrix}(x, itemLabels = NULL, match = NULL)

decode(x, \ldots)
\S4method{decode}{list}(x, itemLabels)
\S4method{decode}{numeric}(x, itemLabels)
}
\arguments{
\item{x}{ a vector or a list of vectors of character strings 
  (for \code{encode}) or of numeric (for \code{decode}), or
  an object of class \code{itemMatrix} (for \code{recode}).}
\item{itemLabels}{a vector of character strings used for coding where 
  the position of an item label in the vector gives the item's column ID.  
  The used \code{itemLabels} vector can be obtained from \code{itemMatrix},  
  \code{transactions} and \code{associations} by the 
  method \code{itemLabels}.}
\item{itemMatrix}{return an object of class \code{itemMatrix} otherwise
  an object of the same class as \code{x} is returned.}
\item{match}{an \code{itemMatrix} object whose item coding \code{x} 
	should match.}
  \item{\ldots}{further arguments.}
}
\value{
\code{recode} always returns an object
  of class \code{itemMatrix}.
   
For \code{encode} with \code{itemMatrix = TRUE} an object
  of class \code{itemMatrix} is returned.
  Otherwise the result is of the same type as \code{x}, e.g., a
  list or a vector.
}
\details{
\code{encode} converts from readable item labels to an itemMatrix using a
given coding. With this method it is possible to create several compatible
\code{itemMatrix} objects (i.e., use the same binary representation for
items) from data.

\code{recode} recodes an itemMatrix object so its coding is compatible
with another itemMatrix object specified in \code{match} (i.e., the colums are reordered to match).

\code{decode} converts from the colun IDs used in the temMatrix representation to
item labels. \code{decode} is used by \code{\link{LIST}}.
}

\seealso{
  \code{\link{LIST}},
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

## Example 1: Manual decoding
## get code
iLabels <- itemLabels(Adult)
head(iLabels)

## get undecoded list and decode in a second step
list <- LIST(Adult[1:5], decode = FALSE)
list

decode(list, itemLabels = iLabels)

## Example 2: Manually create an itemMatrix 
data <- list(
    c("income=small", "age=Young"),
    c("income=large", "age=Middle-aged")
    )

iM <- encode(data, iLabels)
iM

inspect(iM)

## use the itemMatrix to create transactions
as(iM, "transactions")


## Example 3: use recode
## select first 100 transactions and all education-related items
sub <- Adult[1:100, itemInfo(Adult)$variables ==  "education"]
itemLabels(sub)
image(sub)

## recode to match Adult again
sub.recoded <- recode(sub, match = Adult)
image(sub.recoded)

## Example 4: manually create 2 new transaction for the Adult data set
##            Note: check itemLabels(Adult) to see the available labels for items
twoTransactions <- as(encode(list(
    c("age=Young", "relationship=Unmarried"), 
    c("age=Senior")
  ), itemLabels = itemLabels(Adult)),
  "transactions")

inspect(twoTransactions)

## Example 5: manually create a rule and calculate interest measures
aRule <- new("rules", 
  lhs = encode(list(c("age=Young", "relationship=Unmarried")), 
    itemLabels = itemLabels(Adult)),
  rhs = encode(list(c("income=small")), 
    itemLabels = itemLabels(Adult))
)

quality(aRule) <- interestMeasure(aRule, 
  measure = c("support", "confidence", "lift"), transactions = Adult)

inspect(aRule)
}
\keyword{manip}
