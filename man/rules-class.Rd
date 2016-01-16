\name{rules-class}
\docType{class}
\alias{rules-class}
\alias{rules}
\alias{summary.rules-class}
\alias{show,summary.rules-method}
%\alias{[,rules-method}
\alias{coerce,rules,data.frame-method}
\alias{itemInfo,rules-method}
\alias{itemLabels,rules-method}
\alias{items,rules-method}
\alias{generatingItemsets}
\alias{generatingItemsets,rules-method}
\alias{labels,rules-method}
\alias{itemLabels,rules-method}
%\alias{length,rules-method}
\alias{lhs<-,rules-method}
\alias{lhs,rules-method}
\alias{rhs<-,rules-method}
\alias{rhs,rules-method}
\alias{lhs<-}
\alias{lhs}
\alias{rhs<-}
\alias{rhs}
%\alias{subset,rules-method}
\alias{summary,rules-method}
\alias{initialize,rules-method}

\title{Class ``rules'' --- A Set of Rules}
\description{
The \code{rules} class represents a set of rules.

Note that the class can also represent a multiset of rules with duplicated
elements. Duplicated elements can be removed with \code{\link{unique}}.

}
\section{Objects from the Class}{
  Objects are the result of calling the function \code{\link{apriori}}.
  Objects can also be created by calls of the form
  \code{new("rules", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{lhs}:}{Object of class
      \code{\linkS4class{itemMatrix}}; 
      the left-hand-sides of the rules (antecedents)}
    \item{\code{rhs}:}{Object of class
      \code{\linkS4class{itemMatrix}};
      the right-hand-sides of the rules (consequents)} 
    \item{\code{quality}:}{a data.frame}
  }
}
\section{Extends}{
  Class \code{\linkS4class{associations}}, directly.
}
\section{Methods}{
  \describe{
   % \item{[}{\code{signature(x = "rules")};
   %   extracts a subset of rules and the associated quality measures}
    \item{coerce}{\code{signature(from = "rules", to = "data.frame")}; 
      represents the set of rules as a \code{data.frame}}
    \item{itemInfo}{\code{signature(object = "rules")};  
      returns the whole item information data frame including item
      labels}
    \item{itemLabels}{\code{signature(object = "rules")};
           returns the item labels used to encode the rules}
    \item{items}{\code{signature(x = "rules")};
      returns for each rule the union of the items in the 
      lhs and rhs (i.e., the itemsets
      which generated the rule) as an 
      \code{\linkS4class{itemMatrix}}}
    \item{generatingItemsets}{\code{signature(x = "rules")};
      returns a collection of the itemsets which generated the rules (one 
      itemset for each rule). Note that the collection can be a multiset and 
      contain duplicated
      elements. Use \code{unique} to remove duplicates and obtain a 
      proper set.}  
    \item{labels}{\code{signature(object = "rules")};
      returns labels for the rules ("{lhs} => {rhs}") as a
      \code{character} vector. The representation can be customized using 
      the additional parameter \code{ruleSep} and parameters for \code{label}
      defined in \code{\linkS4class{itemMatrix}}} 
   \item{itemLabels}{\code{signature(object = "rules")};
      returns the item labels as a character vector.
      The index for each label is the column index of the item in the
      binary matrix.}
%   \item{length}{\code{signature(x = "rules")};
%      returns the number of rules stored in the the set}
    \item{lhs}{\code{signature(x = "rules")};
      returns the \code{\linkS4class{itemMatrix}}
      representing the left-hand-side of the rules (antecedents)}
    \item{lhs<-}{\code{signature(x = "rules")};
      replaces the \code{\linkS4class{itemMatrix}}
      representing the left-hand-side of the rules (antecedents)}
    \item{rhs}{\code{signature(x = "rules")};
      returns the \code{\linkS4class{itemMatrix}}
      representing the right-hand-side of the rules (consequents)}
    \item{rhs<-}{\code{signature(x = "rules")};
      replaces the \code{\linkS4class{itemMatrix}}
      representing the right-hand-side of the rules (consequents)}
    %\item{subset}{\code{signature(x = "rules")};
    %  selects a subset using restrictions on the quality measures or on
    %  the items present in the rules (see examples).}
    \item{summary}{\code{signature(object = "rules")}}
  }
}
\seealso{
\code{\link{[-methods}},
\code{\link{apriori}},
\code{\link{c}},
\code{\link{duplicated}},
\code{\link{inspect}},
\code{\link{length}},
\code{\link{match}},
\code{\link{sets}},
\code{\link{size}},
\code{\link{subset}},
\code{\link{associations-class}},
\code{\link{itemMatrix-class}},
}
\author{Michael Hahsler}
\examples{
data("Adult")

## Mine rules.
rules <- apriori(Adult, parameter = list(support = 0.4))

## Select a subset of rules using partial matching on the items 
## in the right-hand-side and a quality measure
rules.sub <- subset(rules, subset = rhs \%pin\% "sex" & lift > 1.3)

## Display the top 3 support rules
inspect(head(rules.sub, n = 3, by = "support"))

## Display the first 3 rules
inspect(rules.sub[1:3])

## Get labels for the first 3 rules
labels(rules.sub[1:3])
labels(rules.sub[1:3], itemSep = " + ", setStart = "", setEnd="", 
  ruleSep = " ---> ")
}
\keyword{classes}
