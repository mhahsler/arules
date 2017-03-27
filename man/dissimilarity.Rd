\name{dissimilarity}
\alias{dissimilarity}
\alias{dist}
\alias{dissimilarity,matrix-method}
\alias{dissimilarity,itemMatrix-method}
\alias{dissimilarity,associations-method}
\title{Dissimilarity Computation}
\description{
  Provides the generic function \code{dissimilarity} and the S4 methods to
  compute and returns distances for binary data in a \code{matrix},
  \code{\linkS4class{transactions}} or \code{\linkS4class{associations}} which 
  can be used for grouping and clustering. See Hahsler (2016)
  for an introduction to distance-based
  clustering of association rules.
}
\usage{
dissimilarity(x, y = NULL, method = NULL, args = NULL, \ldots)

\S4method{dissimilarity}{itemMatrix}(x, y = NULL, method = NULL, args = NULL,
	which = "transactions")

\S4method{dissimilarity}{associations}(x, y = NULL, method = NULL, args = NULL,
	which = "associations")

\S4method{dissimilarity}{matrix}(x, y = NULL, method = NULL, args = NULL)
}
\arguments{
  \item{x}{ the set of elements (e.g., \code{matrix, itemMatrix, transactions, 
    itemsets, rules}). }
  \item{y}{ \code{NULL} or a second set to calculate cross dissimilarities. }
  \item{method}{ the distance measure to be used. Implemented measures
  are (defaults to \code{"jaccard"}):
   \describe{
       \item{\code{"affinity"}:}{ measure based on 
         the \code{\link{affinity}},
         a similarity measure between items. It is defined as the
         average \emph{affinity} between the items in two transactions 
         (see Aggarwal et al. (2002)). If \code{x} is not the
	 full transaction set \code{args} needs to contain
	 either precalculated affinities as element \code{"affinities"} or
	 the transaction set as \code{"transactions"}.}
       \item{\code{"cosine"}:}{ the \emph{cosine} distance.} 
       \item{\code{"dice"}:}{ the \emph{Dice's coefficient} defined
         by Dice (1945). Similar to \emph{Jaccard} but gives double the
	     weight to agreeing items.}
       \item{\code{"euclidean"}:}{ the \emph{euclidean} distance.} 
       \item{\code{"jaccard"}:}{ the number of items which occur in both 
         elements divided by the total number of items in the 
         elements (Sneath, 1957). 
         This measure is often 
         also called: \emph{binary, asymmetric binary,} etc. }
       \item{\code{"matching"}:}{ the \emph{Matching coefficient} defined
         by Sokal and Michener (1958). This coefficient gives the same
	     weight to presents and absence of items.}
       \item{\code{"pearson"}:}{ \eqn{1 - r} if \eqn{r>1} and 
	\eqn{1} otherwise. \eqn{r} is \emph{Pearson's correlation 
        coefficient}.} 
       \item{\code{"phi"}:}{ same as pearson. Pearson's correlation
	coefficient reduces to the phi coefficient for the 2x2 contingency 
	tables used here.} 
    }
    For associations the following additional measures are available:
    \describe{
	\item{\code{"toivonen"}:}{Method described in Toivonen et al. (1995).
	    For rules this measure is only defined between rules with
	    the same consequent.
	    The distance between two rules is defined as the number of 
	    transactions which is 
	    covered by only one of the two rules.
	    The transactions used to mine the associations has to be passed on
		via \code{args} as element \code{"transactions"}.}
	\item{\code{"gupta"}:}{Method described in Gupta et al. (1999).
	    The distance between two rules is defined as 1 minus the
	    proportion of transactions which are covered by both
	    rules in the transactions covered by each rule individually.
	    The transactions used to mine the associations has to be passed on
		via \code{args} as element \code{"transactions"}.}
    }

 }
  \item{args}{ a list of additional arguments for the methods.}  
  \item{which}{a character string indicating if the dissimilarity should be 
     calculated between transactions/associations (default) 
     or items (use \code{"items"}). }
  \item{\ldots}{further arguments.}
}
\value{
  returns an object of class \code{dist}.
}
\seealso{
  \code{\link{affinity}},
  \code{\link{dist-class}},
  \code{\link{itemMatrix-class}},
  \code{\link{associations-class}}.
}
\examples{
## cluster items in Groceries with support > 5\%
data("Groceries")

s <- Groceries[,itemFrequency(Groceries)>0.05]
d_jaccard <- dissimilarity(s, which = "items")
plot(hclust(d_jaccard, method = "ward.D2"), main = "Dendrogram for items")

## cluster transactions for a sample of Adult
data("Adult")
s <- sample(Adult, 500) 

##  calculate Jaccard distances and do hclust
d_jaccard <- dissimilarity(s)
hc <- hclust(d_jaccard, method = "ward.D2")
plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Jaccard)")

## get 20 clusters and look at the difference of the item frequencies (bars) 
## for the top 20 items) in cluster 1 compared to the data (line) 
assign <- cutree(hc, 20)
itemFrequencyPlot(s[assign==1], population=s, topN=20)

## calculate affinity-based distances between transactions and do hclust
d_affinity <- dissimilarity(s, method = "affinity")
hc <- hclust(d_affinity, method = "ward.D2")
plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Affinity)")

## cluster association rules
rules <- apriori(Adult, parameter=list(support=0.3))
rules <- subset(rules, subset = lift > 2)

## use affinity to cluster rules
## Note: we need to supply the transactions (or affinities) from the 
## dataset (sample).
d_affinity <- dissimilarity(rules, method = "affinity", 
  args = list(transactions = s))
hc <- hclust(d_affinity, method = "ward.D2")
plot(hc, main = "Dendrogram for Rules (Affinity)") 

## create 4 groups and inspect the rules in the first group.
assign <- cutree(hc, k = 3)
inspect(rules[assign == 1])
}
\references{
  Aggarwal, C.C., Cecilia Procopiuc, and Philip S. Yu. (2002)
  Finding localized associations in market basket data.
  \emph{IEEE Trans. on Knowledge and Data Engineering} 14(1):51--62.

  Dice, L. R. (1945) Measures of the amount of ecologic association 
  between species. \emph{Ecology} 26, pages 297--302.
  
  Gupta, G., Strehl, A., and Ghosh, J. (1999) Distance based clustering 
  of association rules. \emph{In Intelligent Engineering 
  Systems Through Artificial Neural Networks (Proceedings 
  of ANNIE 1999)}, pages 759-764. ASME Press.
  
  Hahsler, M. (2016) Grouping association rules using lift. In C. 
  Iyigun, R. Moghaddess, and A. Oztekin, editors, 11th INFORMS Workshop 
  on Data Mining and Decision Analytics (DM-DA 2016).

  Sneath, P. H. A. (1957) Some thoughts on bacterial classification. 
  \emph{Journal of General Microbiology} 17, pages 184--200.
  
  Sokal, R. R. and Michener, C. D. (1958) A statistical method for evaluating 
  systematic relationships. \emph{University of Kansas Science Bulletin} 38, 
  pages 1409--1438.
 
  Toivonen, H., Klemettinen, M., Ronkainen, P., 
  Hatonen, K. and Mannila H. (1995) Pruning and grouping discovered
  association rules. \emph{In Proceedings of KDD'95}.
}
\author{Michael Hahsler}
\keyword{cluster}
\keyword{models}
