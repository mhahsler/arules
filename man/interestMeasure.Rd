\name{interestMeasure}
\alias{interestMeasure}
\alias{interestMeasure,rules-method}
\alias{interestMeasure,itemsets-method}
\title{Calculate Additional Interest Measures}
\description{
Provides the generic function \code{interestMeasure} and the needed S4 method 
to calculate various additional interest measures for existing sets of
itemsets or rules. A searchable list of definitions, equations and references for all available interest measures can be found here: 
\href{http://michael.hahsler.net/research/association_rules/measures.html}{A Probabilistic Comparison of Commonly Used Interest Measures for Association Rules} (Hahsler, 2015).
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
      is necessary if you use a different set of transactions than was used 
      for mining).}
  \item{\dots}{ further arguments for the measure calculation. }
}
\details{
\bold{The following measures are implemented for itemsets \eqn{X}:}  
\describe{
\item{"allConfidence" (Omiencinski, 2003)}{
 
  Is defined on itemsets as the
  minimum confidence of all possible rule generated from the itemset. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#all-confidence}{All-Confidence}
  
  Range: \eqn{[0, 1]}}

\item{"crossSupportRatio", cross-support ratio (Xiong et al., 2003)}{ 
  
  Defined on itemsets as the ratio of the support of the least frequent item 
  to the support of the most frequent item.
  Cross-support patterns have 
  a ratio smaller than a set threshold. Normally many found patterns are 
  cross-support patterns which contain frequent as well as rare items. Such 
  patterns often tend to be spurious. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#crossSupport}{Cross-Support Ratio}

  Range: \eqn{[0, 1]}}

\item{"lift"}{
  
  Lift is typically only defined for rules. In a similar way, we use the
  probability (support) of the itemset over the product of the probabilities 
  of all items in the itemset, i.e., 
  \eqn{\frac{supp(X)}{\prod_{x \in X} supp(X)}}{supp(X)/(supp(x_1) supp(x_2) ... supp(x_n))}. 
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (1 indicated independence)}

\item{"support", supp (Agrawal et al., 1996)}{
  
  Support is an estimate of \eqn{P(X)}, a measure of generality of the itemset. It is estimated by 
  the number of transactions that contain the itemset over the total number of transactions in the data set.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#support}{Support}

  Range: \eqn{[0, 1]}}

\item{"count"}{
  
  Absolute support count of the itemset, i.e., the number of transactions that contain the itemset. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html}{Support Count}

  Range: \eqn{[0, \infty]}}
}


\bold{The following measures are implemented for rules of the form \eqn{X \Rightarrow Y}{X -> Y}:}

\describe{

\item{"addedValue", added Value, AV, Pavillon index, centered confidence (Tan et al., 2002)}{ 
  
  Defined as the rule confidence minus the rules support. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#addedValue}{Added Value}

  Range: \eqn{[-.5, 1]}}

\item{"chiSquared", \eqn{\chi^2}{X^2} statistic (Liu et al., 1999)}{ 

  The chi-squared statistic 
  to test for independence between the lhs and rhs of the rule.
  The critical value of the chi-squared distribution with \eqn{1} degree of 
  freedom (2x2 contingency table) at \eqn{\alpha=0.05}{alpha=0.05} 
  is \eqn{3.84}; higher chi-squared
  values indicate that the lhs and the rhs are not independent.  
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#chisquared}{Chi-Squared statistic}
  
  %Note that if
  %any cell in the contingency table has an expected value below \eqn{5}, the
  %chi-square value is set to \code{NA} 
  %since the approximation used in the chi-square test breaks down.
  Note that the contingency table is likely to have cells with
  low expected values and that thus
  Fisher's Exact Test might be more appropriate 
  (see below).
  
  Called with \code{significance=TRUE}, the p-value of the test for 
  independence is returned instead of the chi-squared statistic.  
  For p-values, substitutes effects can be tested using 
  the parameter \code{complements = FALSE}.  
  
  Range: \eqn{[0, \infty]}{[0, Inf]}} or p-value scale

\item{"certainty", certainty factor, CF, Loevinger (Berzal et al., 2002)}{
  The certainty factor is a measure of variation of the probability that 
  Y is in a transaction when only considering transactions with X. An increasing CF   
  means a decrease of the probability that Y is not in a transaction that X 
  is in. Negative CFs have a similar interpretation.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#certainty}{Certainty Factor}

  Range: \eqn{[-1, 1]} (0 indicates independence)}

\item{"collectiveStrength"}{Collective strength (S). 

  Collective strength gives 0 for perfectly negative correlated items, infinity for perfectly positive correlated    items, and 1 if the items co-occur as expected under independence.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#collective}{Collective Strength}

  Range: \eqn{[0, \infty]}{[0, Inf]}}

\item{"confidence", Strength, conf (Agrawal et al., 1996)}{ 
   Confidence is a measure of rule validity. Rule confidence is an estimate of \eqn{P(Y|X)}.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#confidence}{Confidence}
  
  Range \eqn{[0, 1]}}

\item{"conviction" (Brin et al. 1997)}{ 
  
  Conviction was developed as an alternative to lift that also incorporates the direction of the rule. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#conviction}{Conviction}
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (\eqn{1} indicates unrelated items)}

\item{"cosine" (Tan et al., 2004)}{
  
  A measure if correlation between the items in X and Y.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#cosine}{Cosine}

  Range: \eqn{[0, 1]}(\eqn{.5} indicates no correlation)}

\item{"count"}{
  
  Absolute support count of the rule, i.e., the number of transactions that contain all items in the rule. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html}{Support Count}
  
  Range: \eqn{[0, \infty]}}
  
\item{"coverage", cover, LHS-support}{ 
    
    It measures the probability that a rule applies to a randomly selected transaction. It is estimated by the proportion of transactions that contain the antecedent (LHS) of the rule. Therefore, coverage is sometimes called antecedent support or LHS support.
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#coverage}{Coverage}
    
    Range: \eqn{[0, 1]}}


\item{"confirmedConfidence", descriptive confirmed confidence (Kodratoff, 1999)}{

    How much higher is the confidence of a rule compared to the confidence of the rule \eqn{X \Rightarrow \overline{Y}}{X -> !Y}. 
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#confirmedConfidence}{Descriptive Confirmed Confidence}
    
    Range: \eqn{[-1, 1]}}

\item{"casualConfidence", casual confidence (Kodratoff, 1999)}{
    Confidence reinforced by the confidence of the rule \eqn{\overline{X} \Rightarrow \overline{Y}}{!X -> !Y}.
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#casualConfidence}{Casual Confidence}
    
    Range: \eqn{[0, 1]}}

\item{"casualSupport", casual support (Kodratoff, 1999)}{
    Support reinforced by the support of the rule \eqn{\overline{X} \Rightarrow \overline{Y}}{!X -> !Y}.
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#casualSupport}{Casual Support}
    
    Range: \eqn{[-1, 1]}}

\item{"counterexample", example and counter-example rate}{

    Rate of the examples minus the rate of counter examples (i.e., \eqn{X \Rightarrow \overline{Y}}{X -> !Y}). 
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#counterexample}{Example and Counter-example Rate}
    
    Range: \eqn{[0, 1]}}

\item{"doc", difference of confidence (Hofmann and Wilhelm, 2001)}{
  Defined as the difference in confidence of the rule and the rule \eqn{\overline{X} \Rightarrow Y}{!X -> Y}
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#doc}{Difference of Confidence}
    
    Range: \eqn{[-1, 1]}}

\item{"fishersExactTest", Fisher's exact test (Hahsler and Hornik, 2007)}{
  p-value of Fisher's exact test used in the analysis of contingency tables 
  where sample sizes are small.
  By default complementary effects are mined, substitutes can be found
  by using the parameter \code{complements = FALSE}. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#fishersExactTest}{Fisher's Exact Test}
  
  Note that it is equal to hyper-confidence with \code{significance=TRUE}.
  
  Range: \eqn{[0, 1]} (p-value scale)}

\item{"gini", Gini index (Tan et al., 2004)}{ 
  Measures quadratic entropy of a rule.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#gini}{Gini index}
  
  Range: \eqn{[0, 1]} (0 means the rule provides no information for the data set)}

\item{"hyperConfidence" (Hahsler and Hornik, 2007)}{
  Confidence level that the observed co-occurrence count of the LHS and RHS is too high 
  given the expected count using the hypergeometric model.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#hyperConf}{Hyper-Confidence}
  
  Hyper-confidence reports the confidence level by default and the 
  significance level if \code{significance=TRUE} is used.
  
  By default complementary effects are mined, substitutes (too low co-occurrence counts) can be found
  by using the parameter \code{complements = FALSE}. 
  
  
  Range: \eqn{[0, 1]}}
  

\item{"hyperLift" (Hahsler and Hornik, 2007)}{ 
  Adaptation of the lift measure which evaluates the deviation from independence
  using a quantile of the hypergeometric distribution defined by the counts of the LHS and RHS. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#hyperLift}{Hyper-Lift}
  
  The used quantile can be given
  as parameter \code{d} (default: \code{d=0.99}).

  Range: \eqn{[0, \infty]}{[0, Inf]} (1 indicates independence)}
  
\item{"imbalance", imbalance ratio, IR (Wu, Chen and Han, 2010)}{
  IR measures the degree of imbalance between the two events 
  that the lhs and the rhs are contained in a transaction.
  The ratio is close to 0 if the conditional probabilities are similar (i.e., 
  very balanced) and
  close to 1 if they are very different. 
  See also: \href{https://michael.hahsler.net/research/association_rules/measures.html#imbalance}{Imbalance ratio}
  
  Range: \eqn{[0, 1]} (0 indicates a balanced rule)}

\item{"implicationIndex", implication index (Gras, 1996)}{
  
  A variation of the Lerman similarity.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#implicationIndex}{Implication Index}

  Range: \eqn{[0, 1]} (0 means independence)}

\item{"importance" (MS Analysis Services)}{
  Log likelihood of the right-hand side of the rule, 
  given the left-hand side of the rule using Laplace corrected confidence.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#importance}{Importance}
  
  Range: \eqn{[-Inf, Inf]}}

\item{"improvement" (Bayardo et al., 2000)}{
  The  improvement of a rule is 
  the minimum difference between its confidence and the confidence of any
  more general rule (i.e., a rule with the same consequent but one or 
  more items removed in the LHS). 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#improvement}{Improvement}
  
  Range: \eqn{[0, 1]}}

\item{"jaccard", Jaccard coefficient (Tan and Kumar, 2000) sometimes also called Coherence (Wu et al., 2010)}{
  Null-invariant measure of dependence defined as the Jaccard similarity between the two sets of transactions
  that contain the items in X and Y, respectively.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#jaccard}{Jaccard coefficient}

  Range: \eqn{[-1, 1]} (0 for independence)}

\item{"jMeasure", J-measure, J (Smyth and Goodman, 1991)}{ 
  A scaled measures of cross entropy to measure the information content of a rule.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#jMeasure}{J-Measure}

  Range: \eqn{[0, 1]} (0 indicates X does not provide information for Y)}

\item{"kappa" Cohen's Kappa (Tan and Kumar, 2000)}{
  Cohen's Kappa of the rule (seen as a classifier) given as the rules observed rule accuracy (i.e., confidence)      corrected by the expected accuracy (of a random classifier).
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#kappa}{Cohen's Kappa}
  
  Range: \eqn{[-1,1]} (0 means the rule is not better than a random classifier)}


\item{"klosgen", Klosgen (Tan and Kumar, 2000)}{
  
  Defined as \eqn{\sqrt{supp(X \cup Y)} conf(X \Rightarrow Y) - supp(Y)}{sqrt(supp(X & Y)) conf(X -> Y) - supp(Y)}
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#klosgen}{Klosgen measure}

  Range: \eqn{[-1, 1]} (0 for independence)}

\item{"kulczynski" (Wu, Chen and Han, 2010; Kulczynski, 1927)}{

  Calculate the null-invariant Kulczynski measure with a preference for 
  skewed patterns. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#kulczynski}{Kulczynski measure}
  
  Range: \eqn{[0, 1]}}  

\item{"lambda", Goodman-Kruskal's \eqn{\lambda}{lambda}, predictive association 
  (Tan and Kumar, 2000)}{
  
  Goodman and Kruskal's lambda to assess the association between the LHS and RHS of the rule.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#lambda}{Goodman-Kruskal's Lambda}
  
  Range: \eqn{[0, 1]}}

\item{"laplace", Laplace corrected confidence/accuracy, L (Tan and Kumar 2000)}{
  Estimates confidence by increasing each count by 1. Parameter \code{k} can be used
  to specify the number of classes (default is 2).
  Prevents counts of 0 and L decreases with lower support. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#laplace}{Laplace corrected confidence/accuracy}
  
  Range: \eqn{[0, 1]}}

\item{"leastContradiction", least contradiction (Aze and Kodratoff, 2004}{

    Probability of finding a matching transaction minus the probability of finding a contradicting transaction normalized by the probability of finding a transaction containing Y. 
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#leastContradiction}{Least Contradiction}
    
    Range: \eqn{[-1, 1]}}

\item{"lerman", Lerman similarity (Lerman, 1981)}{
  
  Defined as \eqn{\sqrt{N} \frac{supp(X \cup Y) - supp(X)supp(Y)}{\sqrt{supp(X)supp(Y)}}}{sqrt(N) (supp(X & Y) - supp(X)supp(Y))/ sqrt(supp(X)supp(Y))}
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#lerman}{Lerman similarity}

  Range: \eqn{[0, 1]}}


\item{"leverage", Piatetsky-Shapiro Measure, PS (Piatetsky-Shapiro 1991)}{
   
  PS measures the difference of X and Y appearing together in the data set 
  and what would be expected if X and Y where statistically dependent. It can
  be interpreted as the gap to independence.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#leverage}{Leverage}
  
  Range: \eqn{[-1, 1]} (0 indicates independence)}

\item{"lift", interest factor (Brin et al. 1997)}{
  
  Lift quantifies dependence between X and Y by comparing the probability that X and Y are contained in a transaction to the expected probability under independence (i.e., the product of the probabilities that X is contained in a transaction times the probability that Y is contained in a transaction).
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#lift}{Lift}
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (1 means independence between LHS and RHS)}

\item{"maxConfidence" (Wu et al. 2010)}{
  Null-invariant symmetric measure defined as the larger of the confidence of the rule or the rule
  with X and Y exchanged.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#maxConf}{MaxConfidence}
  
  Range: \eqn{[0, 1]}
}

\item{"mutualInformation", uncertainty, M (Tan et al., 2002)}{
  Measures the information gain for Y provided by X.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#mutualInformation}{Mutual Information}
  
  Range: \eqn{[0, 1]} (0 means that X does not provide information for Y)}

\item{"oddsRatio", odds ratio \eqn{\alpha}{alpha} (Tan et al., 2004)}{

  The odds of finding X in transactions which contain Y divided by
  the odds of finding X in transactions which do not contain Y.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#oddsRatio}{Odds Ratio}
  
  Range: \eqn{[0, \infty]}{[0, Inf]} (\eqn{1} indicates that Y is not associated to X)}
  
\item{"phi", correlation coefficient \eqn{\phi}{phi} (Tan et al., 2004}{ 
  Correlation coefficient between the transactions containing X and Y  represented as two binary vectors. Phi correlation is equivalent to Pearson's Product Moment Correlation Coefficient \eqn{\rho}{rho} with 0-1 values.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#phi}{Phi Correlation Coefficient}
    
  Range: \eqn{[-1, 1]} (0 when X and Y are independent)}

\item{"ralambondrainy", Ralambondrainy Measure (Diatta et al., 2007)}{
    
    The measure is defined as the probability that a transaction contains X but not Y. A smaller value is better.
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#ralambondrainy}{Ralambondrainy Measure}
    
    Range: \eqn{[0, 1]}}

\item{"rhsSupport", Support of the rule consequent}{
    
    Range: \eqn{[0, 1]}}

\item{"RLD", relative linkage disequilibrium (Kenett and Salini, 2008)}{

    RLD is an association measure motivated by
indices used in population genetics. It evaluates the deviation
    of the support of the whole rule from the support expected under 
    independence given the supports of the LHS and the RHS. 
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#rld}{Relative linkage disequilibrium}
    
    The code was
    contributed by Silvia Salini. 
    
    Range: \eqn{[0, 1]}}

\item{"rulePowerFactor", rule power factor (Ochin et al., 2016)}{

    Product of support and confidence. Can be seen as rule confidence weighted
    by support. 
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#rpf}{Rule Power Factor}
    
    Range: \eqn{[0, 1]}}

\item{"sebag", Sebag-Schoenauer measure (Sebag and Schoenauer, 1988)}{
    
    Confidence of a rule divided by the confidence of the rule  \eqn{X \Rightarrow \overline{Y}}{X -> !Y}.
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#sebag}{Sebag-Schoenauer measure}
    
    Range: \eqn{[0, 1]}}

\item{"stdLift", Standardized Lift (McNicholas et al, 2008)}{
    
    Standardized lift uses the minimum and maximum lift can reach for each rule to standardize lift between 0 and 1. By default, the measure is corrected for minimum support and minimum confidence. Correction can be disabled by using the argument \code{correct = FALSE}.
    
    See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#stdLift}{Standardized Lift}
    
    Range: \eqn{[0, 1]}}
    
\item{"support", supp (Agrawal et al., 1996)}{ 
  Support is an estimate of \eqn{P(X \cup Y)}{P(X & Y)} and 
  measures the generality of the rule. 
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#support}{Support}

  Range: \eqn{[0, 1]}}

\item{"varyingLiaison", varying rates liaison (Bernard and Charron, 1996)}{ 

  Defined as the lift of a rule minus 1 so 0 represents independence.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#varyingLiaison}{Varying Rates Liaison}
  
  Range: \eqn{[-1, \infty]} (0 for independence)}

\item{"yuleQ", Yule's Q (Tan and Kumar, 2000)}{
  Defined as \eqn{\frac{\alpha-1}{\alpha+1}}{(alpha-1)/(alpha+1)} where
  \eqn{\alpha}{alpha} is the odds ratio.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#yule}{Yule's Q}
  
  Range: \eqn{[-1, 1]}}

\item{"yuleY", Yule's Y (Tan and Kumar, 2000)}{
  Defined as \eqn{\frac{\sqrt{\alpha}-1}{\sqrt{\alpha}+1}}{(sqrt(alpha)-1)/(sqrt(alpha)+1)} where
  \eqn{\alpha}{alpha} is the odds ratio.
  See details: \href{https://michael.hahsler.net/research/association_rules/measures.html#yule}{Yule's Y}
  
  Range: \eqn{[-1, 1]}}

}}

\value{
   If only one measure is used, the function returns a numeric vector 
   containing the values of the interest measure for each association
   in the set of associations \code{x}. 

   If more than one measures are specified, the result is a data.frame 
   containing the different measures for each association as columns.
   
   \code{NA} is returned for rules/itemsets for which a certain measure is not 
   defined.
}
\references{
Hahsler, Michael (2015). 
A Probabilistic Comparison of Commonly Used Interest Measures for Association Rules, 2015, URL: \url{http://michael.hahsler.net/research/association_rules/measures.html}.

Agrawal, R., H Mannila, R Srikant, H Toivonen, AI Verkamo (1996).
Fast Discovery of Association Rules.
\emph{Advances in Knowledge Discovery and Data Mining} 12(1):307--328.

Aze, J. and Y. Kodratoff (2004). 
Extraction de pepites de connaissances dans les donnees: Une nouvelle 
approche et une etude de sensibilite au bruit. In 
\emph{Mesures de Qualite pour la fouille de donnees. 
Revue des Nouvelles Technologies de l'Information, RNTI.}

Bernard, Jean-Marc and Charron, Camilo (1996).
L'analyse implicative bayesienne, une methode pour l'etude des dependances orientees. II : modele logique sur un tableau de contingence
Mathematiques et Sciences Humaines, Volume 135 (1996), p. 5--18. 

Bayardo, R. , R. Agrawal, and D. Gunopulos (2000). 
Constraint-based rule mining
in large, dense databases.  \emph{Data Mining and Knowledge Discovery,}
4(2/3):217--240.

Berzal, Fernando, Ignacio Blanco, Daniel Sanchez and Maria-Amparo Vila (2002).
Measuring the accuracy and interest of
association rules: A new framework.
\emph{Intelligent Data Analysis} 6, 221--235.

Bing, Liu, Wynne Hsu, and Yiming Ma (1999). Pruning and summarizing the
discovered associations. In \emph{KDD '99: Proceedings of the fifth ACM SIGKDD
international conference on Knowledge discovery and data mining,} pages
125--134.  ACM Press, 1999.

Brin, Sergey, Rajeev Motwani, Jeffrey D. Ullman, and Shalom Tsur (1997). Dynamic
itemset counting and implication rules for market basket data. In \emph{SIGMOD
1997, Proceedings ACM SIGMOD International Conference on Management of Data,}
pages 255--264, Tucson, Arizona, USA.

Diatta, J., H. Ralambondrainy, and A. Totohasina (2007). Towards a unifying probabilistic implicative normalized quality measure for association rules. In \emph{Quality Measures in Data Mining,} 237--250, 2007.

Gras R (1996). L'implication statistique. Nouvelle methode exploratoire de donnees. La Pensee Sauvage, Grenoble.

Hahsler, Michael and Kurt Hornik (2007). New probabilistic interest measures for association rules. \emph{Intelligent Data Analysis,} 11(5):437--455.

Hofmann, Heike and Adalbert Wilhelm (2001). Visual comparison of association rules. 
\emph{Computational Statistics,} 16(3):399--415.

Kenett, Ron and Silvia Salini (2008). Relative Linkage Disequilibrium: A New
measure for association rules. In \emph{8th Industrial Conference on 
Data Mining ICDM 2008,} July 16--18, 2008, Leipzig/Germany. 

Kodratoff, Y. (1999). Comparing Machine Learning and Knowledge Discovery in 
Databases: An Application to Knowledge Discovery in Texts. 
\emph{Lecture Notes on AI (LNAI) - Tutorial series}.

Kulczynski, S. (1927). Die Pflanzenassoziationen der Pieninen. 
\emph{Bulletin International de l'Academie Polonaise des Sciences et des Lettres, Classe des Sciences Mathematiques et Naturelles B,} 57--203. 

Lerman, I.C. (1981). Classification et analyse ordinale des donnees. Paris. 

McNicholas, P.D.,  T.B. Murphy, M. O'Regan (2008). Standardising the lift of an association rule, \emph{Computational Statistics & Data Analysis,} 52(10):4712--4721, ISSN 0167-9473,
\doi{10.1016/j.csda.2008.03.013}.

Ochin, Suresh Kumar, and Nisheeth Joshi (2016). Rule Power Factor: A New Interest Measure in Associative Classification. \emph{6th International Conference On Advances In Computing and Communications, ICACC 2016,} 6-8 September 2016, Cochin, India.

Omiecinski, Edward R. (2003). Alternative interest measures for mining
associations in databases. \emph{IEEE Transactions on Knowledge and Data
Engineering,} 15(1):57--69, Jan/Feb 2003.

Piatetsky-Shapiro, G. (1991). Discovery, analysis, and presentation of strong
rules. In: \emph{Knowledge Discovery in Databases,} pages 229--248.

Sebag, M. and M. Schoenauer (1988). 
Generation of rules with certainty and confidence factors from incomplete 
and incoherent learning bases. In \emph{Proceedings of the European 
Knowledge Acquisition Workshop (EKAW'88)}, 
Gesellschaft fuer Mathematik und Datenverarbeitung mbH, 28.1--28.20.

Smyth, Padhraic and Rodney M. Goodman (1991).
Rule Induction Using Information Theory.
\emph{Knowledge Discovery in Databases,} 159--176.

Tan, Pang-Ning and Vipin Kumar (2000). Interestingness Measures for Association Patterns: A Perspective. TR 00-036, Department of Computer Science
and Engineering University of Minnesota.  

Tan, Pang-Ning, Vipin Kumar, and Jaideep Srivastava (2002). Selecting the right interestingness measure for association patterns. In \emph{Proceedings of the eighth ACM SIGKDD international conference on Knowledge discovery and data mining (KDD '02),} ACM, 32--41.

Tan, Pang-Ning, Vipin Kumar, and Jaideep Srivastava (2004). Selecting the right
objective measure for association analysis. \emph{Information Systems,}
29(4):293--313.

Wu, T., Y. Chen, and J. Han (2010). Re-examination of interestingness measures in pattern mining: A unified framework. \emph{Data Mining and Knowledge Discovery,} 21(3):371-397, 2010.

Xiong, Hui, Pang-Ning Tan, and Vipin Kumar (2003). Mining strong affinity
association patterns in data sets with skewed support distribution. In Bart
Goethals and Mohammed J. Zaki, editors, \emph{Proceedings of the IEEE
International Conference on Data Mining,} November 19--22, 2003, Melbourne,
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

inspect(head(rules, by = "hyperConfidence"))

## calculate several measures
m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"), 
	transactions = Income)
inspect(head(rules))
head(m)

## calculate all available measures for the first 5 rules and show them as a 
## table with the measures as rows
t(interestMeasure(head(rules, 5), transactions = Income))

## calculate measures on a different set of transactions (I use a sample here)
## Note: reuse = TRUE (default) would just return the stored support on the
##	data set used for mining
newTrans <- sample(Income, 100)
m2 <- interestMeasure(rules, "support", transactions = newTrans, reuse = FALSE) 
head(m2)

## calculate all available measures for the 5 frequent itemsets with highest support
its <- apriori(Income, parameter = list(target = "frequent itemsets"))
its <- head(its, 5, by = "support")
inspect(its)

interestMeasure(its, transactions = Income)
}
\keyword{models}
