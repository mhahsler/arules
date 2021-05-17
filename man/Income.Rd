\name{Income}
\alias{Income}
\alias{IncomeESL}
\docType{data}
\title{Income Data Set}
\description{
  The \code{IncomeESL} data set originates from an example in the book
  \sQuote{The Elements of Statistical Learning} (see Section source).  The data
  set is an extract from this survey.  It consists of 8993 instances (obtained
  from the original data set with 9409 instances, by removing those
  observations with the annual income missing) with 14 demographic attributes.
  The data set is a good mixture of categorical and continuous variables with a
  lot of missing data.  This is characteristic of data mining applications.
  The \code{Income} data set contains the data already prepared and coerced to
  \code{\link{transactions}}.
}
\usage{
data("Income")
data("IncomeESL")
}
\format{
  \code{IncomeESL} is a data frame with 8993 observations on the 
  following 14 variables.
  \describe{
    \item{income}{an ordered factor with levels \code{[0,10)} < \code{[10,15)} < \code{[15,20)} < \code{[20,25)} < \code{[25,30)} < \code{[30,40)} < \code{[40,50)} < \code{[50,75)} < \code{75+}}
    \item{sex}{a factor with levels \code{male} \code{female}}
    \item{marital status}{a factor with levels \code{married} \code{cohabitation} \code{divorced} \code{widowed} \code{single}}
    \item{age}{an ordered factor with levels \code{14-17} < \code{18-24} < \code{25-34} < \code{35-44} < \code{45-54} < \code{55-64} < \code{65+}}
    \item{education}{an ordered factor with levels \code{grade <9} < \code{grades 9-11} < \code{high school graduate} < \code{college (1-3 years)} < \code{college graduate} < \code{graduate study}}
    \item{occupation}{a factor with levels \code{professional/managerial} \code{sales} \code{laborer} \code{clerical/service} \code{homemaker} \code{student} \code{military} \code{retired} \code{unemployed}}
    \item{years in bay area}{an ordered factor with levels \code{<1} < \code{1-3} < \code{4-6} < \code{7-10} < \code{>10}}
    \item{dual incomes}{a factor with levels \code{not married} \code{yes} \code{no}}
    \item{number in household}{an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7} < \code{8} < \code{9+}}
    \item{number of children}{an ordered factor with levels \code{0} < \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7} < \code{8} < \code{9+}}
    \item{householder status}{a factor with levels \code{own} \code{rent} \code{live with parents/family}}
    \item{type of home}{a factor with levels \code{house} \code{condominium} \code{apartment} \code{mobile Home} \code{other}}
    \item{ethnic classification}{a factor with levels \code{american indian} \code{asian} \code{black} \code{east indian} \code{hispanic} \code{pacific islander} \code{white} \code{other}}
    \item{language in home}{a factor with levels \code{english} \code{spanish} \code{other}}
  }
}
\details{
  To create \code{Income} (the transactions object), the original data frame in
  \code{IncomeESL} is prepared in a similar way as 
  described in \sQuote{The Elements
  of Statistical Learning.} We 
  removed cases with missing values and
  cut each ordinal variable (age, education,
  income, years in bay area, number in household, and number of children) 
  at its median into two values (see Section examples).
}
\source{
  Impact Resources, Inc., Columbus, OH (1987).

  Obtained from the web site of the book:
  Hastie, T., Tibshirani, R. \& Friedman, J. (2001)
  \emph{The Elements of Statistical Learning}.
  Springer-Verlag.
}
\author{Michael Hahsler}
\examples{
data("IncomeESL")
IncomeESL[1:3, ]

## remove incomplete cases
IncomeESL <- IncomeESL[complete.cases(IncomeESL), ]

## preparing the data set
IncomeESL[["income"]] <- factor((as.numeric(IncomeESL[["income"]]) > 6) +1,
  levels = 1 : 2 , labels = c("$0-$40,000", "$40,000+"))
	  
IncomeESL[["age"]] <- factor((as.numeric(IncomeESL[["age"]]) > 3) +1,
  levels = 1 : 2 , labels = c("14-34", "35+"))

IncomeESL[["education"]] <- factor((as.numeric(IncomeESL[["education"]]) > 4) +1,
  levels = 1 : 2 , labels = c("no college graduate", "college graduate"))

IncomeESL[["years in bay area"]] <- factor(
  (as.numeric(IncomeESL[["years in bay area"]]) > 4) +1,
  levels = 1 : 2 , labels = c("1-9", "10+"))

IncomeESL[["number in household"]] <- factor(
  (as.numeric(IncomeESL[["number in household"]]) > 3) +1,
  levels = 1 : 2 , labels = c("1", "2+"))

IncomeESL[["number of children"]] <- factor(
  (as.numeric(IncomeESL[["number of children"]]) > 1) +0,
  levels = 0 : 1 , labels = c("0", "1+"))
	
##  creating transactions
Income <- transactions(IncomeESL)
Income
}
\keyword{datasets}
