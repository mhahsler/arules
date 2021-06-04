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
\alias{recode,itemsets-method}
\alias{recode,rules-method}
\alias{compatible}
\alias{compatible,itemMatrix-method}
\alias{compatible,associations-method}
\title{Item Coding --- Conversion between Item Labels and Column IDs}
\description{
  The order in which items are stored in an \code{itemMatrix} is called the \emph{item coding}. The following generic functions and S4 methods are used to translate between the binary representation in the itemMatrix format (used in transactions, rules and itemsets), item labels and numeric item IDs (i.e., the column numbers in the binary representation).
}
\usage{
encode(x, \ldots)
\S4method{encode}{list}(x, itemLabels, itemMatrix = TRUE)
\S4method{encode}{character}(x, itemLabels, itemMatrix = TRUE)
\S4method{encode}{numeric}(x, itemLabels, itemMatrix = TRUE)

compatible(x, y)

recode(x, \ldots)
\S4method{recode}{itemMatrix}(x, itemLabels = NULL, match = NULL)
\S4method{recode}{itemsets}(x, itemLabels = NULL, match = NULL)
\S4method{recode}{rules}(x, itemLabels = NULL, match = NULL)

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
  Alternatively, a \code{itemMatrix},  
  \code{transactions} or \code{associations} object can be specified and the item labels or these objects are used.}
\item{itemMatrix}{return an object of class \code{itemMatrix} otherwise
  an object of the same class as \code{x} is returned.}
\item{y}{ an object of class \code{itemMatrix}, \code{transactions} or \code{associations} to compare item coding to \code{x}. }
\item{match}{ deprecated: used \code{itemLabels} instead.}
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

\bold{Item compatibility:}
If you deal with several datasets or different subsets of the same dataset and want to combine or compate the found itemsets or rules, then you need to make sure that all transaction sets have a compatible item coding. That is, the sparse matrices representing the items have columns for the same items in exactly the same order. The coercion to transactions with \code{as(x, "transactions")} will create the item coding by adding items when they are encountered in the dataset. This can lead to different item codings (different order, missing items) for even only slightly different datasets. You can use the method \code{compatible} to check if two sets have the same item coding. 

If you work with many sets, then you should first define a common item coding by creating a vector with all possible item labels and then use either \code{encode} to create transactions or \code{recode} to make a different set compatible.

The following function help with creating and changing the item coding to make them compatible.

\code{encode} converts from readable item labels to an itemMatrix using a
given coding. With this method it is possible to create several compatible
\code{itemMatrix} objects (i.e., use the same binary representation for
items) from data.

\code{decode} converts from the column IDs used in the itemMatrix representation to
item labels. \code{decode} is used by \code{\link{LIST}}.

\code{recode} recodes an itemMatrix object so its coding is compatible
with another itemMatrix object specified in \code{itemLabels} (i.e., the columns are reordered to match).
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
## Extract the item coding as a vector of item labels.
iLabels <- itemLabels(Adult)
head(iLabels)

## get undecoded list (itemIDs)
list <- LIST(Adult[1:5], decode = FALSE)
list

## decode itemIDs by replacing them with the appropriate item label
decode(list, itemLabels = iLabels)


## Example 2: Manually create an itemMatrix using iLabels as the common item coding
data <- list(
    c("income=small", "age=Young"),
    c("income=large", "age=Middle-aged")
    )

# Option a: encode to match the item coding in Adult
iM <- encode(data, itemLabels = Adult)
iM
inspect(iM)
compatible(iM, Adult)

# Option b: coercion plus recode to make it compatible to Adult 
#           (note: the coding has 115 item columns after recode)
iM <- as(data, "itemMatrix")
iM
compatible(iM, Adult)

iM <- recode(iM, itemLabels = Adult)
iM
compatible(iM, Adult)


## Example 3: use recode to make itemMatrices compatible
## select first 100 transactions and all education-related items
sub <- Adult[1:100, itemInfo(Adult)$variables ==  "education"]
itemLabels(sub)
image(sub)

## After choosing only a subset of items (columns), the item coding is now 
## no longer compatible with the Adult dataset
compatible(sub, Adult)

## recode to match Adult again
sub.recoded <- recode(sub, itemLabels = Adult)
image(sub.recoded)


## Example 4: manually create 2 new transaction for the Adult data set
##            Note: check itemLabels(Adult) to see the available labels for items
twoTransactions <- as(
    encode(list(
        c("age=Young", "relationship=Unmarried"), 
        c("age=Senior")
      ), itemLabels = Adult),
    "transactions")

twoTransactions
inspect(twoTransactions)

## the same using the transactions constructor function instead
twoTransactions <- transactions(
    list(
        c("age=Young", "relationship=Unmarried"), 
        c("age=Senior")
    ), itemLabels = Adult)

twoTransactions
inspect(twoTransactions)

## Example 5: Use a common item coding

# Creation of transactions separately will produce different item codings
trans1 <- transactions(
    list(
        c("age=Young", "relationship=Unmarried"), 
        c("age=Senior")
    ))
trans1

trans2 <- transactions(
    list(
        c("age=Middle-aged", "relationship=Married"), 
        c("relationship=Unmarried", "age=Young")
    ))
trans2

compatible(trans1, trans2)

# produce common item coding (all item labels in the two sets)
commonItemLabels <- union(itemLabels(trans1), itemLabels(trans2))
commonItemLabels

trans1 <- recode(trans1, itemLabels = commonItemLabels)
trans1
trans2 <- recode(trans2, itemLabels = commonItemLabels)
trans2

compatible(trans1, trans2)


## Example 6: manually create a rule using the item coding in Adult 
## and calculate interest measures
aRule <- new("rules", 
  lhs = encode(list(c("age=Young", "relationship=Unmarried")), 
    itemLabels = Adult),
  rhs = encode(list(c("income=small")), 
    itemLabels = Adult)
)

## shorter version using the rules constructor
aRule <- rules( 
  lhs = list(c("age=Young", "relationship=Unmarried")),
  rhs = list(c("income=small")), 
  itemLabels = Adult
)

quality(aRule) <- interestMeasure(aRule, 
  measure = c("support", "confidence", "lift"), transactions = Adult)

inspect(aRule)
}
\keyword{manip}
