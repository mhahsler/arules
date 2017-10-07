\name{APappearance-class}
\docType{class}
\alias{APappearance-class}
\alias{APappearance}
\alias{template}
\alias{coerce,list,APappearance-method}
\alias{coerce,NULL,APappearance-method}
\title{Class APappearance --- Specifying the appearance Argument of Apriori to Implement Rule Templates}

\description{Specifies the restrictions on the associations mined by
  \code{\link{apriori}}. These restrictions can implement certain aspects
  of rule templates described by Klemettinen (1994).
  
  Note that appearance is only supported by the
  implementation of \code{\link{apriori}}.
}

\section{Objects from the Class}{
  If appearance restrictions are used, an appearance object will be
  created automatically within the \code{\link{apriori}} function using
  the information in the named list of the function's \code{appearance}
  argument.  In this case, the item labels used in the list will be
  automatically matched against the items in the used transaction database.
  The list can contain the following elements:

  \describe{
    \item{\code{lhs}, \code{rhs}, \code{both}, \code{items}, \code{none}:}{ 
      character vectors giving the labels of the items
	    which can appear in the specified place
        (rhs, lhs or both for rules and items for itemsets).
      The special place none specifies, that the items mentioned there cannot
      appear in the rule/itemset.}
    \item{\code{default}:}{one of \code{"both"}, \code{"lhs"},
      \code{"rhs"}, \code{"none"} (the default is \code{"both"}). 
      Specified the default appearance for all items not 
      explicitly mentioned in the other elements of the list.}
  }

  Objects can also be created by calls of the form 
  \code{new("APappearance", ...)}.  In this case, item IDs (column
  numbers of the transactions incidence matrix) have to be used instead
  of labels.
}
\section{Slots}{
  \describe{
    \item{\code{set}:}{an integer scalar
      indicating how many items are specified for each of lhs, rhs,
      items, both and none}
    \item{\code{items}:}{an integer
      vector of item IDs (column numbers)}
    \item{\code{labels}:}{a character
      vector of item labels}
    \item{\code{default}:}{a character scalar
      indicating the value for default appearance}
    }
}
\references{
  Christian Borgelt (2004)
  \emph{Apriori --- Finding Association Rules/Hyperedges with the Apriori
    Algorithm}.
  \url{www.borgelt.net/apriori.html}

  M. Klemettinen, H. Mannila, P. Ronkainen, H. Toivonen and A. I. Verkamo (1994).
  Finding Interesting Rules from Large Sets of Discovered Association Rules. 
  In \emph{Proceedings of the Third International Conference 
    on Information and Knowledge Management,} 401--407.

}
\author{Michael Hahsler and Bettina Gruen}
\examples{
data("Adult")

## find only frequent itemsets which do not contain  small or large income
is <- apriori(Adult, parameter = list(support= 0.1, target="frequent"), 
  appearance = list(none = c("income=small", "income=large"),
  default="both"))
itemFrequency(items(is))["income=small"]
itemFrequency(items(is))["income=large"]

## find itemsets that only contain small or large income and young age
is <- apriori(Adult, parameter = list(support= 0.1, target="frequent"), 
  appearance = list(items = c("income=small", "income=large", "age=Young"),
  default="none"))
inspect(head(is))
  
## find only rules with income-related variables in the right-hand-side.
incomeItems <- grep("^income=", itemLabels(Adult), value = TRUE)
incomeItems
rules <- apriori(Adult, parameter = list(support=0.2, confidence = 0.5), 
  appearance = list(rhs = incomeItems, default="lhs"))
inspect(head(rules))

## for more complicated restrictions you have to mine all rules/itemsets and
## then filter the results afterwards
}
\seealso{
  \code{\link{apriori}}
}
\keyword{classes}
