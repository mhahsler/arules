\name{interestMeasure}
\alias{interestMeasure}
\alias{interestMeasure,rules-method}
\alias{interestMeasure,itemsets-method}
\title{Calculating various additional interest measures}
\description{
Provides the generic function \code{interestMeasure} and the needed S4 method 
to calculate various additional interest measures for existing sets of
itemsets or rules.
}
\usage{
interestMeasure(x, measure, transactions = NULL, reuse = TRUE, ...)
}
\arguments{
  \item{x}{ a set of itemsets or rules. }
  \item{measure}{ name or vector of names of the desired interest measures 
      (see details for available measures). If measure is missing then all 
      available measures are calculated.} 
  \item{transactions}{ the transaction data set used to mine 
    the associations or a set of different transactions to calculate
    interest measures from (Note: you need to set \code{reuse=FALSE} in the 
    later case). }
  \item{reuse}{ logical indicating if information in quality slot should
      be reuse for calculating the measures. This speeds up the process
      significantly since only very little (or no) transaction counting 
      is necessary if support, confidence and lift are already available.
      Use \code{reuse=FALSE} to force counting (might be very slow but
      is necessary if you use a differnt set of transactions than was used 
      for mining).}
  \item{\dots}{ further arguments for the measure calculation. }
}
\details{
For itemsets \eqn{X} the following measures are implemented:  
\describe{
\item{"allConfidence" (Omiencinski, 2003)}{
  
  Is defined on itemsets as the
  minimum confidence of all possible rule generated from the itemset. 

  Range: \eqn{[0, 1]}}

\item{"crossSupportRatio", cross-support ratio (Xiong et al., 2003)}{ 
  
  Defined on itemsets as the ratio of the support of the least frequent item 
  to the support of the most frequent item, i.e., 
  \eqn{\frac{min(supp(x \in X))}{max(supp(x \in X))}}{min(supp(x in X)) / max(supp(x in X))}. Cross-support patterns have 
  a ratio smaller than a set threshold. Normally many found patterns are 
  cross-support patterns which contain frequent as well as rare items. Such 
  patterns often tend to be spurious. 

  Range: \eqn{[0, 1]}}

\item{"lift"}{
  
  Probability (support) of the itemset over the product of the probabilities 
  of all items in the itemset, i.e., 
  \eqn{\frac{supp(X)}{\prod_{x \in X} supp(X)}}{supp(X)/(supp(x_1) supp(x_2) ... supp(x_n))}. This is a measure 
  of dependence similar to lift for rules. 
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (1 indicated independence)}

\item{"support", supp (Agrawal et al., 1996)}{
  
  Support is an estimate of \eqn{P(X)}
  a measure of generality of the itemset. 

Range: \eqn{[0, 1]}}
}


For rules \eqn{X \Rightarrow Y}{X -> Y} the following measures are implemented.
In the following we use the notation 
\eqn{supp(X \Rightarrow Y) = supp(X \cup Y)}{supp(X -> Y) = supp(X & Y)} to indicate the support
of the union of the itemsets \eqn{X} and \eqn{Y}, i.e., the proportion of 
the transactions that contain both itemsets. 
We also use \eqn{\overline{X}}{!X} as the complement itemset to \eqn{X}
with \eqn{supp(\overline{X}) = 1 - supp(X)}{supp(!X) = 1 - supp(X)}, i.e.,
the proportion of transactions that do not contain \eqn{X}.

\describe{

\item{"addedValue", added Value, AV, Pavillon index, 
  centered confidence (Tan et al., 2002)}{ 
  
  Defined as \eqn{conf(X \Rightarrow Y) - supp(Y)}{conf(X -> Y) - supp(Y)}

  Range: \eqn{[-.5, 1]}}

\item{"chiSquared", \eqn{\chi^2}{X^2} (Liu et al., 1999)}{ 

  The chi-squared statistic 
  to test for independence between the lhs and rhs of the rule.
  The critical value of the chi-squared distribution with \eqn{1} degree of 
  freedom (2x2 contingency table) at \eqn{\alpha=0.05}{alpha=0.05} 
  is \eqn{3.84}; higher chi-squared
  values indicate that the lhs and the rhs are not independent.  
  %Note that if
  %any cell in the contingency table has an expected value below \eqn{5}, the
  %chi-square value is set to \code{NA} 
  %since the approximation used in the chi-square test breaks down.
  Note that the contingency table is likely to have cells with
  low expected values and that thus
  Fisher's Exact Test might be more appropriate 
  (see below).
  
  Called with \code{significance=TRUE}, the p-value of the test for independence is returned instead of the chi-squared statistic.  
  
  Range: \eqn{[0, \infty]}{[0, Inf]}}

\item{"certainty", certainty factor, CF, Loevinger (Berzal et al., 2002)}{
  The certainty factor is a measure of variation of the probability that 
  Y is in a transaction when only considering transactions with X. An inreasing CF   
  means a decrease of the probability that Y is not in a transaction that X 
  is in. Negative CFs have a similar interpretation.

  Range: \eqn{[-1, 1]} (0 indicates independence)}

\item{"collectiveStrength"}{Collective strength (S). 

  Range: \eqn{[0, \infty]}{[0, Inf]}}

\item{"confidence", conf (Agrawal et al., 1996)}{ 
  Rule confidence is an estimate of \eqn{P(Y|X)} calculated as 
    \eqn{\frac{supp(X \Rightarrow Y)}{supp(X)}}{supp(X -> Y)/supp(X)}. Confidence is a measure of validity. 
  
  Range \eqn{[0, 1]}}

\item{"conviction" (Brin et al. 1997)}{ 
  
  Defined as 
  \eqn{\frac{supp(X)supp(\overline{Y})}{supp(X \cup \overline{Y})}}{supp(X)supp(!Y)/supp(X & !Y)}. 
  
  Range: \eqn{[.5, \infty]}{[.5, Inf]} (\eqn{1} indicates unrelated items)}

\item{"cosine" (Tan et al., 2004)}{
  Defined as \eqn{\frac{supp(X \cup Y)}{\sqrt{(supp(X)supp(Y))}}}{supp(X & Y)/sqrt(supp(X)supp(Y))}

  Range: \eqn{[0, 1]}}
  
\item{"coverage", cover, LHS-support}{ Support of the left-hand-side of the rule, 
    i.e., \eqn{supp(X)}.
    A meassure of to how often the rule can be applied.
    
    Range: \eqn{[0, 1]}}


\item{"confirmedConfidence", descriptive confirmed confidence (Kodratoff, 1999)}{
    Confidence confirmed by its negative as \eqn{conf(X \Rightarrow Y) - conf(X \Rightarrow \overline{Y})}{conf(X -> Y) - conf(X -> !Y)}.
    
    Range: \eqn{[-1, 1]}}

\item{"casualConfidence", casual confidence (Kodratoff, 1999)}{
    Confidence reinforced by negatives given by 
    \eqn{\frac{1}{2}(conf(X \Rightarrow Y) + conf(\overline{Y} \Rightarrow \overline{X}))}{1/2 (conf(X -> Y) + conf(!Y -> !X))}.
    
    Range: \eqn{[0, 1]}}

\item{"casualSupport", casual support (Kodratoff, 1999)}{
    Support improved by negatives given by \eqn{supp(X \cup Y) - supp(\overline{X} \cup \overline{Y})}{supp(X & Y) - supp(!X & !Y)}.
    
    Range: \eqn{[-1, 1]}}

\item{"counterexample", example and counterexample rate}{
    \eqn{\frac{supp(X \cup Y) - supp(X \cup \overline{Y})}{supp(X \cup Y)}}{(supp(X & Y) - supp(X & !Y)) / supp(X & Y)}
    
    Range: \eqn{[0, 1]}}


\item{"descriptiveConfirm", descriptive-confirm (Kodratoff, 1999)}{
    Defined by \eqn{supp(X \cup Y) - supp(X \cup \overline{Y})}{supp(X & Y) - supp(X & !Y)}.   
      
    Range: \eqn{[0, 1]}}

\item{"doc", difference of confidence (Hofmann and Wilhelm, 2001)}{
  Defined as 
  \eqn{conf(X \Rightarrow Y) - conf(\overline{X} \Rightarrow Y)}{conf(X -> Y)-conf(!X -> Y)}.
    
    Range: \eqn{[-1, 1]}}

\item{"fishersExactTest", Fisher's exact test (Hahsler and Hornik, 2007)}{
  Statistical significance test used in the analysis of contingency tables 
  where sample sizes are small. Returns the p-value.
  Note that it is equal to hyper-confidence with \code{significance=TRUE}.
  
  Range: \eqn{[0, 1]} (p-value scale)}

\item{"gini", Gini index (Tan et al., 2004)}{ 
  Measures quadratic entropy.
  
  Range: \eqn{[0, 1]} (0 for independence)}

\item{"hyperLift" (Hahsler and Hornik, 2007)}{ 
  Adaptation of the lift measure which is more robust for low counts. It is 
  based on the idea that under
  independence the count \eqn{c_{XY}} of the transactions which contain all items
  in a rule \eqn{X \Rightarrow Y}{X -> Y} follows a hypergeometric distribution 
  (represented by the random variable \eqn{C_{XY}}) with
  the parameters given by the counts  \eqn{c_X} and  \eqn{c_Y}.
  
  %Lift is defined for the rule \eqn{X \Rightarrow Y}{X -> Y} as:
  %\deqn{\mathrm{lift}(X \Rightarrow Y) = \frac{P(X \cup Y)}{P(X)P(Y)} 
  %= \frac{c_{XY}}{E[C_{XY}]},}{lift(X -> Y) = P(X+Y)/(P(X)*P(Y)) = c_XY / E[C_XY],}
%  
%  where \eqn{E[C_{XY}] = c_X c_Y / m} with \eqn{m} being the number
%  of transactions in the database.

  Hyper-lift is defined as:
  \deqn{\mathrm{hyperlift}(X \Rightarrow Y) = \frac{c_{XY}}{Q_{\delta}[C_{XY}]},}{hyperlift(X -> Y) = c_XY / Q_d[C_XY],}

  where  \eqn{Q_{\delta}[C_{XY}]}{Q_d[C_XY]} is the
  quantile of the hypergeometric distribution given by \eqn{\delta}{d}.
  The quantile can be given
  as parameter \code{d} (default: \code{d=0.99}).

  Range: \eqn{[0, \infty]}{[0, Inf]} (1 indicates independence)}
  
\item{"hyperConfidence" (Hahsler and Hornik, 2007)}{
  Confidence level for observation of too high/low counts 
  for rules \eqn{X \Rightarrow Y}{X -> Y} using the hypergeometric model.
  Since the counts are drawn from a hypergeometric distribution 
  (represented by the random variable \eqn{C_{XY}}) with
  known parameters given by the counts  \eqn{c_X} and  \eqn{c_Y},
  we can calculate a confidence interval for the observed counts 
  \eqn{c_{XY}} stemming from the distribution. Hyper-confidence
  reports the confidence level 
  (significance level if \code{significance=TRUE} is used) for
  \describe{
  \item{complements -}{
    \eqn{1 - P[C_{XY} >= c_{XY} | c_X, c_Y]}
  }
  \item{substitutes -}{
    \eqn{1 - P[C_{XY} < c_{XY} | c_X, c_Y]}.
  }
  }
  A confidence level of, e.g., \eqn{> 0.95} indicates that
  there is only a  5\% chance that the count for the rule was generated
  randomly.
  
  Per default complementary effects are mined, substitutes can be found
  by using the parameter \code{complements = FALSE}. 
  
  Range: \eqn{[0, 1]}}
  
\item{"imbalance", imbalance ratio, IR (Wu, Chen and Han, 2010)}{
  IR is defined as
  \eqn{\frac{|supp(X) - supp(Y)|}{supp(X) + supp(Y) - supp(X \Rightarrow Y))}}{|supp(X) - supp(Y)|/(supp(X) + supp(Y) - supp(X -> Y))}
  gauges the degree of imbalance between two events 
  that the lhs and the rhs are contained in a transaction.
  The ratio is close to 0 if the conditional probabilities are similar (i.e., 
  very balanced) and
  close to 1 if they are very different. 
  
  Range: \eqn{[0, 1]} (0 indicates a balanced rule)}

\item{"implicationIndex", implication index (Gras, 1996)}{
  Defined as \eqn{\sqrt{N} \frac{supp(X \cup \overline{Y}) - supp(X)supp(\overline{Y})}{\sqrt{supp(X)supp(\overline{Y})}}}{sqrt(N) (supp(X & !Y) - supp(X)supp(!Y))/ sqrt(supp(X)supp(!Y))}.
  Represents a variation of the Lerman similarity.

  Range: \eqn{[0, 1]} (0 means independence)}

\item{"improvement" (Bayardo et al., 2000)}{
  The  improvement of a rule is 
  the minimum difference between its confidence and the confidence of any
  proper sub-rule with the same consequent. 
  
  Range: \eqn{[0, 1]}}

\item{"jaccard", Jaccard coefficient (Tan and Kumar, 2000)}{
  Defined as \eqn{\frac{supp(X \cup Y)}{supp(X) + supp(Y) - supp(X \cup Y)}}{supp(X & Y) / (supp(X) + supp(Y) - supp(X & Y))}

  Range: \eqn{[-1, 1]} (0 for independence)}

\item{"jMeasure", J-measure, J (Smyth and Goodman, 1991)}{ 
  Measures cross entrophy.

  Range: \eqn{[0, 1]} (0 for independence)}

\item{"kappa" (Tan and Kumar, 2000)}{
  Defined as \eqn{\frac{supp(X \cup Y) + supp(\overline{X} \cup \overline{Y}) - supp(X)supp(Y) - supp(\overline{X})supp(\overline{Y})}{1- supp(X)supp(Y) - supp(\overline{X})supp(\overline{Y})}}{(supp(X & Y) + supp(!X & !Y) - supp(X)supp(Y) - supp(!X)supp(!Y))/(1- supp(X)supp(Y) - supp(!X)supp(!Y))}
  
  Range: \eqn{[-1,1]} (0 means independence)}


\item{"klosgen", Klosgen (Tan and Kumar, 2000)}{
  Defined as \eqn{\sqrt{supp(X \cup Y)} conf(X \Rightarrow Y) - supp(Y)}{sqrt(supp(X & Y)) conf(X -> Y) - supp(Y)}

  Range: \eqn{[-1, 1]} (0 for independence)}

\item{"kulczynski" (Wu, Chen and Han, 2007; Kulczynski, 1927)}{

  Calculate the null-invariant Kulczynski measure with a preference for 
  skewed patterns. 
  
  Range: \eqn{[0, 1]}}

\item{"lambda", Goodman-Kruskal \eqn{\lambda}{lambda}, predictive association 
  (Tan and Kumar, 2000)}{
  
  Range: \eqn{[0, 1]}}

\item{"laplace", L (Tan and Kumar 2000)}{
  Estimates confidence (decreases with lower support). 
  
  Range: \eqn{[0, 1]}}

\item{"leastContradiction", least contradiction (Aze and Kodratoff, 2004}{
    \eqn{\frac{supp(X \cup Y) - supp(X \cup \overline{Y})}{supp(Y)}}{(supp(X \ Y) - supp(X & !Y)) / supp(Y)}.
    
    Range: \eqn{[-1, 1]}}

\item{"lerman", Lerman similarity (Lerman, 1981)}{
  Defined as \eqn{\sqrt{N} \frac{supp(X \cup Y) - supp(X)supp(Y)}{\sqrt{supp(X)supp(Y)}}}{sqrt(N) (supp(X & Y) - supp(X)supp(Y))/ sqrt(supp(X)supp(Y))}

  Range: \eqn{[0, 1]}}


\item{"leverage", PS (Piatetsky-Shapiro 1991)}{
   PS is defined as 
   \eqn{supp(X \Rightarrow Y) - supp(X)supp(Y)}{supp(X->Y) - (supp(X) supp(Y))}.
  It measures the difference of X and Y appearing together in the data set 
  and what would be expected if X and Y where statistically dependent. It can
  be interpreted as the gap to independence.
  
  Range: \eqn{[-1, 1]} (0 indicates intependence)}

\item{"lift", interest factor (Brin et al. 1997)}{
  Lift quantifies dependence between X and Y by \eqn{\frac{supp(X \cup Y)}{supp(X)supp(Y)}}{supp(X&Y)/(supp(X)supp(Y))}.
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (1 means independence)}

\item{"mutualInformation", uncertainty, M (Tan et al., 2002)}{
  Measures the information gain for Y provided by X.
  
  Range: \eqn{[0, 1]} (0 for independence)}

\item{"oddsRatio", odds ratio \eqn{\alpha}{alpha} (Tan et al., 2004)}{

  The odds of finding X in transactions which contain Y divided by
  the odds of finding X in transactions which do not contain Y.
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (\eqn{1} indicates that Y is not associated to X)}
  
\item{"phi", correlation coefficient \eqn{\phi}{phi} (Tan et al. 2004}{ 
  Equivalent to Pearsons Product Moment Correlation Coefficient \eqn{\rho}{rho}.
    
  Range: \eqn{[-1, 1]} (0 when X and Y are independent)}

\item{"ralambrodrainy", Ralambrodrainy Measure (Ralambrodrainy, 1991)}{
    
    Range: \eqn{[0, 1]}}

\item{"RLD", relative linkage disequilibrium (Kenett and Salini, 2008)}{

    RLD evaluates the deviation
    of the support of the whole rule from the support expected under 
    independence given the supports of the LHS and the RHS. The code was
    contributed by Silvia Salini. 
    
    Range: \eqn{[0, 1]}}


\item{"sebag", Sebag measure (Sebag and Schoenauer, 1988)}{
    Defined as \eqn{\frac{supp(X \cup Y)}{supp(X \cup \overline{Y})}}{supp(X & Y)/supp(X & !Y)}
    
    Range: \eqn{[0, 1]}}
    
\item{"support", supp (Agrawal et al., 1996)}{ 
  Support is an estimate of \eqn{P(X \cup Y)}{P(X & Y)} and 
  measures the generality of the rule. 

  Range: \eqn{[0, 1]}}

\item{"varyingLiaison", varying rates liaison (Bernard and Charron, 1996)}{ 
  Defined as \eqn{\frac{supp(X \cup Y)}{supp(X)supp(Y)}-1}{(supp(X & Y) / (supp(X)supp(Y))) - 1}. 
  Is equivalent to \eqn{lift(X \Rightarrow Y) -1}{lift(X -> Y) -1} 
  
  Range: \eqn{[-1, 1]} (0 for independence)}

\item{"yuleQ", Yule's Q (Tan and Kumar, 2000)}{
  Defined as \eqn{\frac{\alpha-1}{\alpha+1}}{(alpha-1)/(alpha+1)} where
  \eqn{\alpha}{alpha} is the odds ratio.
  
  Range: \eqn{[-1, 1]}}

\item{"yuleY", Yule's Y (Tan and Kumar, 2000)}{
  Defined as \eqn{\frac{\sqrt{\alpha}-1}{\sqrt{\alpha}+1}}{(sqrt(alpha)-1)/(sqrt(alpha)+1)} where
  \eqn{\alpha}{alpha} is the odds ratio.
  
  Range: \eqn{[-1, 1]}}

}}

\value{
   If only one measure is used, the function returns a numeric vector 
   containing the values of the interest measure for each association
   in the set of associations \code{x}. 

   If more than one measures are specified, the result is a data.frame 
   containing the different measures for each association.
   
   \code{NA} is returned for rules/itemsets for which a certain measure is not 
   defined.
}
\references{
Agrawal, R., H Mannila, R Srikant, H Toivonen, AI Verkamo (1996).
Fast Discovery of Association Rules.
\emph{Advances in Knowledge Discovery and Data Mining} 12 (1), 307--328.

Aze, J. and Y. Kodratoff (2004). 
Extraction de pepites de connaissances dans les donnees: Une nouvelle 
approche et une etude de sensibilite au bruit. In 
\emph{Mesures de Qualite pour la fouille de donnees. 
Revue des Nouvelles Technologies de l'Information, RNTI.}

Bayardo, R. , R. Agrawal, and D. Gunopulos (G). Constraint-based rule mining
in large, dense databases.  \emph{Data Mining and Knowledge Discovery},
4(2/3):217--240, 2000.

Berzal, Fernando, Ignacio Blanco, Daniel Sanchez and Maria-Amparo Vila (2002).
Measuring the accuracy and interest of
association rules: A new framework.
\emph{Intelligent Data Analysis} 6, 221--235.

Brin, Sergey, Rajeev Motwani, Jeffrey D. Ullman, and Shalom Tsur (1997). Dynamic
itemset counting and implication rules for market basket data. In \emph{SIGMOD
1997, Proceedings ACM SIGMOD International Conference on Management of Data},
pages 255--264, Tucson, Arizona, USA.

Hahsler, Michael and Kurt Hornik (2007). New probabilistic interest measures for association rules. \emph{Intelligent Data Analysis}, 11(5):437--455.

Hofmann, Heike and Adalbert Wilhelm (2001). Visual comparison of association rules. 
\emph{Computational Statistics}, 16(3):399--415.

Kenett, Ron and Silvia Salini (2008). Relative Linkage Disequilibrium: A New
measure for association rules. In \emph{8th Industrial Conference on 
Data Mining ICDM 2008,} July 16--18, 2008, Leipzig/Germany.

Kodratoff, Y. (1999). Comparing Machine Learning and Knowledge Discovery in 
Databases: An Application to Knowledge Discovery in Texts. 
\emph{Lecture Notes on AI (LNAI) - Tutorial series}.

Kulczynski, S. (1927). Die Pflanzenassoziationen der Pieninen. 
\emph{Bulletin International de l'Academie Polonaise des Sciences et des Lettres, Classe des Sciences Mathematiques et Naturelles B,} 57--203. 

Lerman, I.C. (1981). Classification et analyse ordinale des donnees. Paris. 

Liu, Bing, Wynne Hsu, and Yiming Ma (1999). Pruning and summarizing the
discovered associations. In \emph{KDD '99: Proceedings of the fifth ACM SIGKDD
international conference on Knowledge discovery and data mining}, pages
125--134.  ACM Press, 1999.

Omiecinski, Edward R. (2003). Alternative interest measures for mining
associations in databases. \emph{IEEE Transactions on Knowledge and Data
Engineering}, 15(1):57--69, Jan/Feb 2003.

Piatetsky-Shapiro, G. (1991). Discovery, analysis, and presentation of strong
rules. In: \emph{Knowledge Discovery in Databases}, pages 229--248.

Sebag, M. and M. Schoenauer (1988). 
Generation of rules with certainty and confidence factors from incomplete 
and incoherent learning bases. In \emph{Proceedings of the European 
Knowledge Acquisition Workshop (EKAW'88)}, 
Gesellschaft fuer Mathematik und Datenverarbeitung mbH, 28.1--28.20.

Smyth, Padhraic and Rodney M. Goodman (1991).
Rule Induction Using Information Theory.
\emph{Knowledge Discovery in Databases}, 159--176.

Tan, Pang-Ning and Vipin Kumar (2000). Interestingness Measures for Association Patterns: A Perspective. TR 00-036, Department of Computer Science
and Engineering University of Minnesota.  

Tan, Pang-Ning, Vipin Kumar, and Jaideep Srivastava (2002). Selecting the right interestingness measure for association patterns. In \emph{Proceedings of the eighth ACM SIGKDD international conference on Knowledge discovery and data mining (KDD '02)}, ACM, 32--41.

Tan, Pang-Ning, Vipin Kumar, and Jaideep Srivastava (2004). Selecting the right
objective measure for association analysis. \emph{Information Systems},
29(4):293--313.

Wu, Tianyi, Yuguo Chen, and Jiawei Han (2007). Association Mining in Large Databases: A Re-examination of Its Measures. In \emph{Proceedings of the 11th European conference on Principles and Practice of Knowledge Discovery in Databases (PKDD 2007)}, Springer-Verlag, Berlin, Heidelberg, 621--628.

Wu, T., Y. Chen, and J. Han (2010). Re-examination of interestingness measures in pattern mining: A unified framework. \emph{Data Mining and Knowledge Discovery},
2010.

Xiong, Hui, Pang-Ning Tan, and Vipin Kumar (2003). Mining strong affinity
association patterns in data sets with skewed support distribution. In Bart
Goethals and Mohammed J. Zaki, editors, \emph{Proceedings of the IEEE
International Conference on Data Mining}, November 19--22, 2003, Melbourne,
Florida, pages 387--394.
}

\seealso{ \code{\link{itemsets-class}}, \code{\link{rules-class}} }
\author{Michael Hahsler}
\examples{
data("Income")
rules <- apriori(Income)

## calculate a single measure and add it to the quality slot
quality(rules) <- cbind(quality(rules), 
	hyperConfidence = interestMeasure(rules, measure = "hyperConfidence", 
	transactions = Income))

inspect(head(sort(rules, by = "hyperConfidence")))

## calculate several measures
m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"), 
	transactions = Income)
inspect(head(rules))
head(m)

## calculate all available measures for the first 5 rules and show them as a 
## table with the measures as rows
t(interestMeasure(head(rules, 5), transactions = Income))

## calculate measures on a differnt set of transactions (I use a sample here)
## Note: reuse = TRUE (default) would just retun the stored support on the
##	data set used for mining
newTrans <- sample(Income, 100)
m2 <- interestMeasure(rules, "support", transactions = newTrans, reuse = FALSE) 
head(m2)
}
\keyword{models}
