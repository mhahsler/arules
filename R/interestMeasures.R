#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' Calculate Additional Interest Measures
#'
#' Provides the generic function `interestMeasure()` and the needed S4
#' method to calculate various additional interest measures for existing sets
#' of itemsets or rules. A searchable list of definitions, equations and
#' references for all available interest measures can be found here:
#' \url{https://mhahsler.github.io/arules/docs/measures}
#'
#' **The following measures are implemented for itemsets \eqn{X}:**
#' \describe{ \item{"allConfidence"}{
#'
#' Is defined on itemsets as the minimum confidence of all possible rule
#' generated from the itemset.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#all-confidence}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"crossSupportRatio", cross-support ratio}{
#'
#' Defined on itemsets as the ratio of the support of the least frequent item
#' to the support of the most frequent item.  Cross-support patterns have a
#' ratio smaller than a set threshold. Normally many found patterns are
#' cross-support patterns which contain frequent as well as rare items. Such
#' patterns often tend to be spurious.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#cross-support-ratio}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"lift"}{
#'
#' Lift is typically only defined for rules. In a similar way, we use the
#' probability (support) of the itemset over the product of the probabilities
#' of all items in the itemset, i.e., \eqn{\frac{supp(X)}{\prod_{x \in X}
#' supp(X)}}{supp(X)/(supp(x_1) supp(x_2) ... supp(x_n))}.
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} (1 indicated independence)}
#'
#' \item{"support", supp}{
#'
#' Support is an estimate of \eqn{P(X)}, a measure of generality of the
#' itemset. It is estimated by the number of transactions that contain the
#' itemset over the total number of transactions in the data set.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#support}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"count"}{
#'
#' Absolute support count of the itemset, i.e., the number of transactions that
#' contain the itemset.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#support}
#'
#' Range: \eqn{[0, \infty]}} }
#'
#' \bold{The following measures are implemented for rules of the form \eqn{X
#' \Rightarrow Y}{X => Y}:}
#'
#' \describe{
#'
#' \item{"addedValue", added Value, AV, Pavillon index, centered confidence}{
#'
#' Defined as the rule confidence minus the rule's support.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#added-value}
#'
#' Range: \eqn{[-.5, 1]}}
#'
#' \item{"boost", confidence boost}{
#'
#' Confidence boost is the ratio of the confidence of a rule to the confidence
#' of any more general rule (i.e., a rule with the same consequent but one or
#' more items removed in the LHS).  Values larger than 1 mean the new rule
#' boosts the confidence compared to the best, more general rule. The measure
#' is related to the improvement measure.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#confidence-boost}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]}}
#'
#' \item{"chiSquared", }{
#'
#' The chi-squared statistic to test for independence between the lhs and rhs
#' of the rule.  The critical value of the chi-squared distribution with
#' \eqn{1} degree of freedom (2x2 contingency table) at
#' \eqn{\alpha=0.05}{alpha=0.05} is \eqn{3.84}; higher chi-squared values
#' indicate that the lhs and the rhs are not independent.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#chi-squared}
#'
#' %Note that if %any cell in the contingency table has an expected value below
#' \eqn{5}, the %chi-square value is set to \code{NA} %since the approximation
#' used in the chi-square test breaks down.  Note that the contingency table is
#' likely to have cells with low expected values and that thus Fisher's Exact
#' Test might be more appropriate (see below).
#'
#' Called with \code{significance = TRUE}, the p-value of the test for
#' independence is returned instead of the chi-squared statistic.  For
#' p-values, substitution effects (the ocurrence of one item makes the
#' ocurrance of another item less likely) can be tested using the parameter
#' \code{complements = FALSE}.  Correction for multiple comparisons can be done
#' using \code{\link{p.adjust}}.
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} or p-value
#' scale}\item{list(list("\\chi^2"), list("X^2"))}{
#'
#' The chi-squared statistic to test for independence between the lhs and rhs
#' of the rule.  The critical value of the chi-squared distribution with
#' \eqn{1} degree of freedom (2x2 contingency table) at
#' \eqn{\alpha=0.05}{alpha=0.05} is \eqn{3.84}; higher chi-squared values
#' indicate that the lhs and the rhs are not independent.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#chi-squared}
#'
#' %Note that if %any cell in the contingency table has an expected value below
#' \eqn{5}, the %chi-square value is set to \code{NA} %since the approximation
#' used in the chi-square test breaks down.  Note that the contingency table is
#' likely to have cells with low expected values and that thus Fisher's Exact
#' Test might be more appropriate (see below).
#'
#' Called with \code{significance = TRUE}, the p-value of the test for
#' independence is returned instead of the chi-squared statistic.  For
#' p-values, substitution effects (the ocurrence of one item makes the
#' ocurrance of another item less likely) can be tested using the parameter
#' \code{complements = FALSE}.  Correction for multiple comparisons can be done
#' using \code{\link{p.adjust}}.
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} or p-value scale}\item{ statistic}{
#'
#' The chi-squared statistic to test for independence between the lhs and rhs
#' of the rule.  The critical value of the chi-squared distribution with
#' \eqn{1} degree of freedom (2x2 contingency table) at
#' \eqn{\alpha=0.05}{alpha=0.05} is \eqn{3.84}; higher chi-squared values
#' indicate that the lhs and the rhs are not independent.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#chi-squared}
#'
#' %Note that if %any cell in the contingency table has an expected value below
#' \eqn{5}, the %chi-square value is set to \code{NA} %since the approximation
#' used in the chi-square test breaks down.  Note that the contingency table is
#' likely to have cells with low expected values and that thus Fisher's Exact
#' Test might be more appropriate (see below).
#'
#' Called with \code{significance = TRUE}, the p-value of the test for
#' independence is returned instead of the chi-squared statistic.  For
#' p-values, substitution effects (the ocurrence of one item makes the
#' ocurrance of another item less likely) can be tested using the parameter
#' \code{complements = FALSE}.  Correction for multiple comparisons can be done
#' using \code{\link{p.adjust}}.
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} or p-value scale}
#'
#' \item{"certainty", certainty factor, CF, Loevinger}{ The certainty factor is
#' a measure of variation of the probability that Y is in a transaction when
#' only considering transactions with X. An increasing CF means a decrease of
#' the probability that Y is not in a transaction that X is in. Negative CFs
#' have a similar interpretation.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#certainty-factor}
#'
#' Range: \eqn{[-1, 1]} (0 indicates independence) }
#'
#' \item{"collectiveStrength", Collective strength, S}{
#'
#' Collective strength gives 0 for perfectly negative correlated items,
#' infinity for perfectly positive correlated items, and 1 if the items
#' co-occur as expected under independence.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#collective-strength}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]}}
#'
#' \item{"confidence", Strength, conf}{ Confidence is a measure of rule
#' validity. Rule confidence is an estimate of \eqn{P(Y|X)}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#confidence}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"conviction"}{
#'
#' Conviction was developed as an alternative to lift that also incorporates
#' the direction of the rule.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#conviction}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} (\eqn{1} indicates unrelated items)}
#'
#' \item{"cosine"}{
#'
#' A measure if correlation between the items in X and Y.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#cosine}
#'
#' Range: \eqn{[0, 1]}(\eqn{.5} indicates no correlation)}
#'
#' \item{"count"}{
#'
#' Absolute support count of the rule, i.e., the number of transactions that
#' contain all items in the rule.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#support}
#'
#' Range: \eqn{[0, \infty]}}
#'
#' \item{"coverage", cover, LHS-support}{
#'
#' It measures the probability that a rule applies to a randomly selected
#' transaction. It is estimated by the proportion of transactions that contain
#' the antecedent (LHS) of the rule. Therefore, coverage is sometimes called
#' antecedent support or LHS support.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#coverage}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"confirmedConfidence", descriptive confirmed confidence}{
#'
#' How much higher is the confidence of a rule compared to the confidence of
#' the rule \eqn{X \Rightarrow \overline{Y}}{X -> !Y}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#descriptive-confirmed-confidence}
#'
#' Range: \eqn{[-1, 1]}}
#'
#' \item{"casualConfidence", casual confidence}{ Confidence reinforced by the
#' confidence of the rule \eqn{\overline{X} \Rightarrow \overline{Y}}{!X ->
#' !Y}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#casual-confidence}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"casualSupport", casual support}{ Support reinforced by the support of
#' the rule \eqn{\overline{X} \Rightarrow \overline{Y}}{!X -> !Y}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#casual-support} Range:
#' \eqn{[-1, 1]}}
#'
#' \item{"counterexample", example and counter-example rate}{
#'
#' Rate of the examples minus the rate of counter examples (i.e., \eqn{X
#' \Rightarrow \overline{Y}}{X -> !Y}).
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#example-and-counter-example-rate}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"doc", difference of confidence}{ Defined as the difference in
#' confidence of the rule and the rule \eqn{\overline{X} \Rightarrow Y}{!X ->
#' Y}
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#difference-of-confidence}
#' Range: \eqn{[-1, 1]}}
#'
#' \item{"fishersExactTest", Fisher's exact test}{ p-value of Fisher's exact
#' test used in the analysis of contingency tables where sample sizes are
#' small.  By default complementary effects are mined, substitutes can be found
#' by using the parameter \code{complements = FALSE}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#fishers-exact-test}
#'
#' Note that it is equal to hyper-confidence with \code{significance=TRUE}.
#' Correction for multiple comparisons can be done using
#' \code{\link{p.adjust}}.
#'
#' Range: \eqn{[0, 1]} (p-value scale)}
#'
#' \item{"gini", Gini index}{ Measures quadratic entropy of a rule.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#gini-index}
#'
#' Range: \eqn{[0, 1]} (0 means the rule provides no information for the data
#' set)}
#'
#' \item{"hyperConfidence"}{ Confidence level that the observed co-occurrence
#' count of the LHS and RHS is too high given the expected count using the
#' hypergeometric model.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#hyper-confidence}
#'
#' Hyper-confidence reports the confidence level by default and the
#' significance level if \code{significance=TRUE} is used.
#'
#' By default complementary effects are mined, substitutes (too low
#' co-occurrence counts) can be found by using the parameter \code{complements
#' = FALSE}.
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"hyperLift"}{ Adaptation of the lift measure which evaluates the
#' deviation from independence using a quantile of the hypergeometric
#' distribution defined by the counts of the LHS and RHS. HyperLift can be used
#' to calculate confidence intervals for the lift measure.
#'
#' The used quantile can be given as parameter \code{level} (default:
#' \code{level = 0.99}).
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#hyper-lift}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} (1 indicates independence)}
#'
#' \item{"imbalance", imbalance ratio, IR}{ IR measures the degree of imbalance
#' between the two events that the lhs and the rhs are contained in a
#' transaction.  The ratio is close to 0 if the conditional probabilities are
#' similar (i.e., very balanced) and close to 1 if they are very different.
#' See also:
#' \url{https://mhahsler.github.io/arules/docs/measures#imbalance-ratio}
#'
#' Range: \eqn{[0, 1]} (0 indicates a balanced rule)}
#'
#' \item{"implicationIndex", implication index}{
#'
#' A variation of the Lerman similarity.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#implication-index}
#'
#' Range: \eqn{[0, 1]} (0 means independence)}
#'
#' \item{"importance"}{ Log likelihood of the right-hand side of the rule,
#' given the left-hand side of the rule using Laplace corrected confidence.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#importance}
#'
#' Range: \eqn{[-Inf, Inf]}}
#'
#' \item{"improvement"}{ The improvement of a rule is the minimum difference
#' between its confidence and the confidence of any more general rule (i.e., a
#' rule with the same consequent but one or more items removed in the LHS).
#'
#' Special case: We define improvement for a rules with an empty LHS as its
#' confidence.
#'
#' The idea of improvement can be generalized to other measures than
#' confidence. Other measures like lift can be specified with the extra
#' parameter \code{improvementMeasure}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#improvement}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"jaccard", Jaccard coefficient, sometimes also called Coherence}{
#' Null-invariant measure of dependence defined as the Jaccard similarity
#' between the two sets of transactions that contain the items in X and Y,
#' respectively.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#jaccard-coefficient}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"jMeasure", J-measure, J}{ A scaled measures of cross entropy to
#' measure the information content of a rule.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#j-measure}
#'
#' Range: \eqn{[0, 1]} (0 indicates X does not provide information for Y)}
#'
#' \item{"kappa" Cohen's Kappa (Tan and Kumar, 2000)}{ Cohen's Kappa of the
#' rule (seen as a classifier) given as the rule's observed rule accuracy
#' (i.e., confidence) corrected by the expected accuracy (of a random
#' classifier).
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#kappa}
#'
#' Range: \eqn{[-1,1]} (0 means the rule is not better than a random
#' classifier)}
#'
#' \item{"klosgen"}{
#'
#' Defined as \eqn{\sqrt{supp(X \cup Y)} conf(X \Rightarrow Y) -
#' supp(Y)}{sqrt(supp(X & Y)) conf(X -> Y) - supp(Y)}
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#klosgen}
#'
#' Range: \eqn{[-1, 1]} (0 for independence)}
#'
#' \item{"kulczynski", kulc}{
#'
#' Calculate the null-invariant Kulczynski measure with a preference for skewed
#' patterns.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#kulczynski}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"lambda", Goodman-Kruskal's }{
#'
#' Goodman and Kruskal's lambda to assess the association between the LHS and
#' RHS of the rule.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#lambda}
#'
#' Range: \eqn{[0, 1]}}\item{list(list("\\lambda"), list("lambda"))}{
#'
#' Goodman and Kruskal's lambda to assess the association between the LHS and
#' RHS of the rule.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#lambda}
#'
#' Range: \eqn{[0, 1]}}\item{, predictive association}{
#'
#' Goodman and Kruskal's lambda to assess the association between the LHS and
#' RHS of the rule.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#lambda}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"laplace", Laplace corrected confidence/accuracy, L}{ Estimates
#' confidence by increasing each count by 1. Parameter \code{k} can be used to
#' specify the number of classes (default is 2).  Prevents counts of 0 and L
#' decreases with lower support.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#laplace-corrected-confidence}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"leastContradiction", least contradiction}{
#'
#' Probability of finding a matching transaction minus the probability of
#' finding a contradicting transaction normalized by the probability of finding
#' a transaction containing Y.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#least-contradiction}
#'
#' Range: \eqn{[-1, 1]}}
#'
#' \item{"lerman", Lerman similarity}{
#'
#' Defined as \eqn{\sqrt{N} \frac{supp(X \cup Y) -
#' supp(X)supp(Y)}{\sqrt{supp(X)supp(Y)}}}{sqrt(N) (supp(X & Y) -
#' supp(X)supp(Y))/ sqrt(supp(X)supp(Y))}
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#lerman-similarity}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"leverage", Piatetsky-Shapiro Measure, PS}{
#'
#' PS measures the difference of X and Y appearing together in the data set and
#' what would be expected if X and Y where statistically dependent. It can be
#' interpreted as the gap to independence.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#leverage}
#'
#' Range: \eqn{[-1, 1]} (0 indicates independence)}
#'
#' \item{"lift", interest factor}{
#'
#' Lift quantifies dependence between X and Y by comparing the probability that
#' X and Y are contained in a transaction to the expected probability under
#' independence (i.e., the product of the probabilities that X is contained in
#' a transaction times the probability that Y is contained in a transaction).
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#lift}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} (1 means independence between LHS and
#' RHS)}
#'
#' \item{"maxConfidence"}{ Null-invariant symmetric measure defined as the
#' larger of the confidence of the rule or the rule with X and Y exchanged.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#maxconfidence} Range:
#' \eqn{[0, 1]} }
#'
#' \item{"mutualInformation", uncertainty, M}{ Measures the information gain
#' for Y provided by X.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#mutual-information}
#'
#' Range: \eqn{[0, 1]} (0 means that X does not provide information for Y)}
#'
#' \item{"oddsRatio", odds ratio}{
#'
#' The odds of finding X in transactions which contain Y divided by the odds of
#' finding X in transactions which do not contain Y.  For zero counts,
#' Haldane-Anscombe correction (adding .5 to all zells) is applied.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#odds_ratio}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]} (\eqn{1} indicates that Y is not
#' associated to X)}
#'
#' \item{"oddsRatioCI", odds ratio confidence interval}{
#'
#' Calculates the lower and upper bounds of the confidence interval around the
#' odds ratio (using a normal approximation).  The used confidence level
#' defaults to 0.95, but can be adjusted with the additional parameter
#' \code{confidenceLevel}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#odds-ratio}
#'
#' Range: \eqn{[0, \infty]}{[0, Inf]}}
#'
#' \item{"phi", correlation coefficient }{ Correlation coefficient between the
#' transactions containing X and Y represented as two binary vectors. Phi
#' correlation is equivalent to Pearson's Product Moment Correlation
#' Coefficient \eqn{\rho}{rho} with 0-1 values.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#phi-correlation-coefficient}
#' Range: \eqn{[-1, 1]} (0 when X and Y are
#' independent)}\item{list(list("\\phi"), list("phi"))}{ Correlation
#' coefficient between the transactions containing X and Y represented as two
#' binary vectors. Phi correlation is equivalent to Pearson's Product Moment
#' Correlation Coefficient \eqn{\rho}{rho} with 0-1 values.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#phi-correlation-coefficient}
#' Range: \eqn{[-1, 1]} (0 when X and Y are independent)}
#'
#' \item{"ralambondrainy", Ralambondrainy Measure}{
#'
#' The measure is defined as the probability that a transaction contains X but
#' not Y. A smaller value is better.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#ralambondrainy}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"rhsSupport", Support of the rule consequent}{
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"RLD", relative linkage disequilibrium}{
#'
#' RLD is an association measure motivated by indices used in population
#' genetics. It evaluates the deviation of the support of the whole rule from
#' the support expected under independence given the supports of the LHS and
#' the RHS.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#relative-linkage-disequilibrium}
#'
#' The code was contributed by Silvia Salini.
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"rulePowerFactor", rule power factor}{
#'
#' Product of support and confidence. Can be seen as rule confidence weighted
#' by support.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#rule-power-factor}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"sebag", Sebag-Schoenauer measure}{
#'
#' Confidence of a rule divided by the confidence of the rule \eqn{X
#' \Rightarrow \overline{Y}}{X -> !Y}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#sebag-schoenauer}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"stdLift", Standardized Lift}{
#'
#' Standardized lift uses the minimum and maximum lift can reach for each rule
#' to standardize lift between 0 and 1. By default, the measure is corrected
#' for minimum support and minimum confidence. Correction can be disabled by
#' using the argument \code{correct = FALSE}.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#standardized-lift}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"support", supp}{ Support is an estimate of \eqn{P(X \cup Y)}{P(X &
#' Y)} and measures the generality of the rule.
#'
#' See details: \url{https://mhahsler.github.io/arules/docs/measures#support}
#'
#' Range: \eqn{[0, 1]}}
#'
#' \item{"table"}{ Returns the counts for the contingency table. The values are
#' labeled \eqn{n_{XY}}{n_XY} where \eqn{X} and \eqn{Y} represent the presence
#' (1) or absence (0) of the LHS and RHS of the rule, respectively.  If several
#' measures are specified, then the counts have the prefix \code{table.}
#'
#' Range: counts}
#'
#' \item{"varyingLiaison", varying rates liaison}{
#'
#' Defined as the lift of a rule minus 1 so 0 represents independence.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#Varying-Rates-Liaison}
#'
#' Range: \eqn{[-1, \infty]} (0 for independence)}
#'
#' \item{"yuleQ", Yule's Q}{ Defined as
#' \eqn{\frac{\alpha-1}{\alpha+1}}{(alpha-1)/(alpha+1)} where
#' \eqn{\alpha}{alpha} is the odds ratio.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#yules-q-and-yules-y}
#'
#' Range: \eqn{[-1, 1]}}
#'
#' \item{"yuleY", Yule's Y}{ Defined as
#' \eqn{\frac{\sqrt{\alpha}-1}{\sqrt{\alpha}+1}}{(sqrt(alpha)-1)/(sqrt(alpha)+1)}
#' where \eqn{\alpha}{alpha} is the odds ratio.
#'
#' See details:
#' \url{https://mhahsler.github.io/arules/docs/measures#yules-q-and-yules-y}
#'
#' Range: \eqn{[-1, 1]}}
#'
#' }
#'
#' @aliases interestMeasure
#' @family interest measures
#' 
#' @param x a set of itemsets or rules.
#' @param measure name or vector of names of the desired interest measures (see
#' details for available measures). If measure is missing then all available
#' measures are calculated.
#' @param transactions the transaction data set used to mine the associations
#' or a set of different transactions to calculate interest measures from
#' (Note: you need to set \code{reuse=FALSE} in the later case).
#' @param reuse logical indicating if information in quality slot should be
#' reuse for calculating the measures. This speeds up the process significantly
#' since only very little (or no) transaction counting is necessary if support,
#' confidence and lift are already available.  Use \code{reuse=FALSE} to force
#' counting (might be very slow but is necessary if you use a different set of
#' transactions than was used for mining).
#' @param \dots further arguments for the measure calculation. Many measures
#' are based on contingency table counts and zero counts can produce NaN values
#' (division by zero). This issue can be resolved by using the additional
#' parameter \code{smoothCounts} which performs additive smoothing by adds a
#' "pseudo count" of \code{smoothCounts} to each count in the contingency
#' table. Use \code{smoothCounts = 1} or larger values for Laplace smoothing.
#' Use \code{smoothCounts = .5} for Haldane-Anscombe correction often used for
#' chi-squared, phi correlation and related measures.
#' @return If only one measure is used, the function returns a numeric vector
#' containing the values of the interest measure for each association in the
#' set of associations \code{x}.
#'
#' If more than one measures are specified, the result is a data.frame
#' containing the different measures for each association as columns.
#'
#' \code{NA} is returned for rules/itemsets for which a certain measure is not
#' defined.
#' @author Michael Hahsler
#' @seealso \code{\link{itemsets-class}}, \code{\link{rules-class}}
#' @references A complete list of references for each individual measure is
#' available in the following document:
#'
#' Hahsler, Michael (2015).  A Probabilistic Comparison of Commonly Used
#' Interest Measures for Association Rules, 2015, URL:
#' \url{https://mhahsler.github.io/arules/docs/measures}.
#' @keywords models
#' @examples
#'
#' data("Income")
#' rules <- apriori(Income)
#'
#' ## calculate a single measure and add it to the quality slot
#' quality(rules) <- cbind(quality(rules),
#' 	hyperConfidence = interestMeasure(rules, measure = "hyperConfidence",
#' 	transactions = Income))
#'
#' inspect(head(rules, by = "hyperConfidence"))
#'
#' ## calculate several measures
#' m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"),
#' 	transactions = Income)
#' inspect(head(rules))
#' head(m)
#'
#' ## calculate all available measures for the first 5 rules and show them as a
#' ## table with the measures as rows
#' t(interestMeasure(head(rules, 5), transactions = Income))
#'
#' ## calculate measures on a different set of transactions (I use a sample here)
#' ## Note: reuse = TRUE (default) would just return the stored support on the
#' ##	data set used for mining
#' newTrans <- sample(Income, 100)
#' m2 <- interestMeasure(rules, "support", transactions = newTrans, reuse = FALSE)
#' head(m2)
#'
#' ## calculate all available measures for the 5 frequent itemsets with highest support
#' its <- apriori(Income, parameter = list(target = "frequent itemsets"))
#' its <- head(its, 5, by = "support")
#' inspect(its)
#'
#' interestMeasure(its, transactions = Income)
setGeneric("interestMeasure",
  function(x,
    measure,
    transactions = NULL,
    reuse = TRUE,
    ...)
    standardGeneric("interestMeasure"))

measuresItemsets <- c("support",
  "count",
  "allConfidence",
  "crossSupportRatio",
  "lift")

#' @rdname interestMeasure
setMethod("interestMeasure",  signature(x = "itemsets"),
  function(x,
    measure,
    transactions = NULL,
    reuse = TRUE,
    ...) {
    ## backward comp.
    add <- list(...)
    if (!is.null(add$method)) {
      warning("interestMeasure: parameter method is now deprecated! Use measure instead!")
      measure <- add$method
    }
    
    if (missing(measure))
      measure <- measuresItemsets
    
    ## check and expand measure
    if (any(is.na(ind <- pmatch(
      tolower(measure),
      tolower(measuresItemsets)
    ))))
      stop(
        gettextf(
          "Invalid measure(s) for rules: %s",
          paste(measure[is.na(ind)], collapse = ", ")
        ),
        gettextf(
          "\n\n  Available measures: %s",
          paste(measuresItemsets, collapse = ", ")
        ),
        domain = NA
      )
    
    measure <- measuresItemsets[ind]
    
    ## remove quality information if we do not want to reuse! Then we can start reusing
    if (!reuse)
      quality(x) <- data.frame(seq_len(length(x)))[, 0]
    
    if (is.null(quality(x)[["support"]]))
      quality(x)[["support"]] <-
      support(x, transactions = transactions)
    
    ## deal with multiple measures
    if (length(measure) > 1)
      return(as.data.frame(
        sapply(
          measure,
          FUN =
            function(m)
              interestMeasure(x, m, transactions, reuse = TRUE, ...),
          USE.NAMES = TRUE,
          simplify = FALSE
        )
      ))
    
    ## first see if we already have it:
    if (!is.null(quality(x)[[measure]]))
      return(quality(x)[[measure]])
    
    ## calculate measures
    if (measure == "count")
      return(round(quality(x)[["support"]] * .getN(x, transactions)))
    ## all other measures are basic measures
    return(.basicItemsetMeasures(x, measure, transactions, ...))
  })

.getN <- function(x, transactions) {
  ## find N from associations or if not available then from transactions
  n <- info(x)$ntransactions
  if (is.null(n)) {
    if (is.null(transactions))
      stop("transaction data needed. Please specify the transactions used to mine the itemsets!")
    n <- length(transactions)
  }
  n
}

.basicItemsetMeasures <- function(x,
  measure,
  transactions = NULL,
  ...) {
  if (is.null(transactions))
    stop("transaction data needed. Please specify the transactions used to mine the itemsets!")
  
  itemSupport <- itemFrequency(transactions)
  if (length(itemSupport) != nitems(items(x)))
    stop("number of items in itemsets and transactions do not match.")
  
  ## create an itemset list
  itemset_list <- LIST(items(x), decode = FALSE)
  
  ## catch empty itemset
  if (length(itemset_list) < 1)
    return(numeric(0))
  
  ## calculate all-confidence for existing (frequent) itemsets.
  ##
  ## Edward R. Omiecinski. Alternative interest measures for mining
  ## associations in databases. IEEE Transactions on Knowledge and
  ## Data Engineering, 15(1):57-69, Jan/Feb 2003.
  ##
  ## calculate all-confidence using itemsets support and the
  ## singleton support of the most frequent item in the itemset
  ## all-confidence(Z) = supp(Z) / max(supp(i elem Z))
  
  if (measure == "allConfidence") {
    m <-
      quality(x)[["support"]] / sapply(itemset_list, function(i)
        max(itemSupport[i]))
    
    ### deal with 1-itemsets
    is1 <- size(x) == 1
    m[is1] <- quality(x)[["support"]][is1]
  }
  
  ## calculate the cross-support ratio
  ## used to eliminate cross support patterns which contain item with
  ## extremely different support. These patterns tend to be spurious
  ## (i.e., one item which occurs in virtually all transactions and some very
  ##  rare items)
  ##
  ## Hui Xiong, Pang-Ning Tan, Vipin Kumar. Mining Strong Affinity Association
  ## Patterns in Data Sets with Skewed Support. Third IEEE International
  ## Conference on Data Mining, Melbourne, Florida, November 19 - 22, 2003.
  ##
  
  if (measure == "crossSupportRatio")
    m <- sapply(itemset_list, function(i)
      min(itemSupport[i])) /
    sapply(itemset_list, function(i)
      max(itemSupport[i]))
  
  if (measure == "lift")
    m <-
    quality(x)[["support"]] / sapply(itemset_list, function(i)
      prod(itemSupport[i]))
  
  m[!is.finite(m)] <- NA
  
  return(m)
}

## measures for rules

# sort measures (except 1-4)
# dput(c(measuresRules[1:4], sort(measuresRules[-(1:4)])))
measuresRules <-
  c(
    "support",
    "confidence",
    "lift",
    "count",
    "addedValue",
    "boost",
    "casualConfidence",
    "casualSupport",
    "centeredConfidence",
    "certainty",
    "chiSquared",
    "collectiveStrength",
    "confirmedConfidence",
    "conviction",
    "cosine",
    "counterexample",
    "coverage",
    "doc",
    "fishersExactTest",
    "gini",
    "hyperConfidence",
    "hyperLift",
    "imbalance",
    "implicationIndex",
    "importance",
    "improvement",
    "jaccard",
    "jMeasure",
    "kappa",
    "kulczynski",
    "lambda",
    "laplace",
    "leastContradiction",
    "lerman",
    "leverage",
    "maxconfidence",
    "mutualInformation",
    "oddsRatio",
    "phi",
    "ralambondrainy",
    "relativeRisk",
    "rhsSupport",
    "RLD",
    "rulePowerFactor",
    "sebag",
    "stdLift",
    "table",
    "varyingLiaison",
    "yuleQ",
    "yuleY"
  )


#' @rdname interestMeasure
setMethod("interestMeasure",  signature(x = "rules"),
  function(x,
    measure,
    transactions = NULL,
    reuse = TRUE,
    ...) {
    ## backward comp.
    add <- list(...)
    if (!is.null(add$method)) {
      warning("interestMeasure: parameter method is now deprecated! Use measure instead!")
      measure <- add$method
    }
    
    if (missing(measure))
      measure <- measuresRules
    
    ## check and expand measure
    if (any(is.na(ind <- pmatch(
      tolower(measure),
      tolower(measuresRules)
    ))))
      stop(
        gettextf(
          "Invalid measure(s) for rules: %s",
          paste(measure[is.na(ind)], collapse = ", ")
        ),
        gettextf(
          "\n\n  Available measures: %s",
          paste(measuresRules, collapse = ", ")
        ),
        domain = NA
      )
    
    measure <- measuresRules[ind]
    
    ## remove quality information if we do not want to reuse! Then we can start reusing
    if (!reuse)
      quality(x) <- data.frame(seq_len(length(x)))[, 0]
    
    ## precalculate some measures once (most measures can be calculated using support, confidence, and lift)
    ## if we haive no support then we probably have nothing! Count it with a single p-tree
    if (is.null(quality(x)[["support"]])) {
      s <-
        support(c(items(x), lhs(x), rhs(x)), transactions = transactions)
      quality(x)[["support"]] <- s[seq(length(x))]
      quality(x)[["coverage"]] <- s[length(x) + seq(length(x))]
      quality(x)[["confidence"]] <-
        quality(x)[["support"]] / quality(x)[["coverage"]]
      quality(x)[["rhsSupport"]] <-
        s[2 * length(x) + seq(length(x))]
      quality(x)[["lift"]] <-
        quality(x)[["confidence"]] / quality(x)[["rhsSupport"]]
    }
    
    if (is.null(quality(x)[["coverage"]]))
      quality(x)[["coverage"]] <-
      coverage(x, transactions = transactions)
    if (is.null(quality(x)[["confidence"]]))
      quality(x)[["confidence"]] <-
      quality(x)[["support"]] / quality(x)[["coverage"]]
    if (is.null(quality(x)[["lift"]]))
      quality(x)[["lift"]] <-
      quality(x)[["confidence"]] / .rhsSupport(x, transactions = transactions)
    
    if (length(measure) > 1L)
      return(as.data.frame(
        sapply(
          measure,
          FUN =
            function(m)
              interestMeasure(x, m, transactions = transactions, reuse = TRUE, ...),
          USE.NAMES = TRUE,
          simplify = FALSE
        )
      ))
    
    ## catch empty ruleset
    if (length(x) < 1)
      return(numeric(0))
    
    ## first see if we already have a basic measure. All others we recalculate.
    if (measure %in% c("support", "confidence", "coverage", "lift")
      && !is.null(quality(x)[[measure]]))
      return(quality(x)[[measure]])
    
    ## calculate measure (support, confidence, lift and coverage are already handled)
    if (measure == "boost")
      return(.conf_boost(x, transactions = transactions, ...))
    if (measure == "count")
      return(round(quality(x)[["support"]] * .getN(x, transactions)))
    if (measure == "rhsSupport")
      return(.rhsSupport(x, transactions))
    if (measure == "improvement")
      return(.improvement(x, transactions = transactions, ...))
    if (measure == "hyperLift")
      return(.hyperLift(x, transactions = transactions, ...))
    if (measure == "hyperConfidence")
      return(.hyperConfidence(x, transactions = transactions, ...))
    if (measure == "fishersExactTest")
      return(.hyperConfidence(x,
        transactions = transactions,
        significance = TRUE,
        ...))
    if (measure == "RLD")
      return(.RLD(x, transactions = transactions, ...))
    if (measure == "stdLift")
      return(.stdLift(x, transactions = transactions, ...))
    
    ## all other measures are implemented here (i is in ...)
    ret <-
      .basicRuleMeasure(x, measure, transactions = transactions, ...)
    
    ## make all bad values NA (does not work for measures that return data.frames)
    #if (is.vector(ret)) ret[!is.finite(ret)] <- NA
    
    return(ret)
    
    stop("Specified measure not implemented.")
  })

## calculate hyperlift for existing rules.
##
## Michael Hahsler, Kurt Hornik, and Thomas Reutterer.
## Implications of probabilistic data modeling for rule mining.
## Report 14, Research Report Series, Department of Statistics and
## Mathematics, Wirtschaftsuniversitaet Wien, Augasse 2-6, 1090 Wien,
## Austria, March 2005.

## hyperlift(X => Y) = c_X,Y / Q_d[C_X,Y]
##
## where Q_d[C_X,Y] = qhyper(d, m = c_Y, n = length(trans.) - c_Y, k = c_X)
##
## c_X,Y = count(X => Y)
## c_X = count(X)
## c_Y = count(Y)
##
## this implements only hyperlift for rules with a single item in the consequent

.hyperLift <- function(x, level = 0.99, ...) {
  counts <- .getCounts(x, ...)
  
  with(counts, {
    Q <-
      stats::qhyper(
        level,
        m = nx1,
        n = n - nx1,
        k = n1x,
        lower.tail = TRUE
      )
    n11 / Q
  })
}


## calculate hyperconfidence for existing rules.
## (confidence level that we observe too high/low counts)
##
## uses the model from:
## Hahsler, Michael and Kurt Hornik (2007). New probabilistic
## interest measures for association rules.
## Intelligent Data Analysis, 11(5):437--455.

.hyperConfidence <-
  function(x,
    complements = TRUE,
    significance = FALSE,
    ...) {
    ## significance: return significance levels instead of
    ##   confidence levels
    
    counts <- .getCounts(x, ...)
    
    
    if (complements == TRUE)
      ## c_XY - 1 so we get P[C_XY < c_XY] instead of P[C_XY <= c_XY]
      res <- with(counts, {
        stats::phyper(
          n11 - 1,
          m = nx1,
          n = n - nx1,
          k = n1x,
          lower.tail = !significance
        )
      })
    
    else
      ## substitutes; Pr[C_XY > c_XY]
      ## empty LHS causes a div by zero -> NAN
      suppressWarnings(res <- with(counts, {
        stats::phyper(
          n11,
          m = nx1,
          n = n - n1x,
          k = n1x,
          lower.tail = significance
        )
      }))
    
    res[is.nan(res)] <- NA
    res
  }

## Minimum Improvement (Bayardo et al. 1999)
## Let the improvement of a rule be defined as the minimum
## difference between its confidence and the confidence of any
## proper sub-rule with the same consequent.

.improvement <- function(x,
  improvementMeasure = "confidence", ...) {
  ## Note: improvement is defined for confidence, but could also used with
  ## other measures
  q <- interestMeasure(x, measure = improvementMeasure, ...)
  imp <- numeric(length(x))
  
  ### do it by unique rhs
  rr <- .Call(R_pnindex, rhs(x)@data, NULL, FALSE)
  
  for (r in unique(rr)) {
    pos <- which(rr == r)
    
    q2 <- q[pos]
    ### FALSE is for verbose
    qsubmax <- .Call(R_pnmax, lhs(x[pos])@data, q2, FALSE)
    
    imp[pos] <- q2 - qsubmax
  }
  
  imp
}

.conf_boost <- function(x, ...) {
  conf <- interestMeasure(x, "confidence", ...)
  imp <- .improvement(x, ...)
  
  conf / (conf - imp)
}

.getCounts <-
  function(x,
    transactions = NULL,
    reuse = TRUE,
    smoothCounts = 0) {
    if (smoothCounts < 0)
      stop("smoothCount needs to be >= 0!")
    
    q <-
      interestMeasure(
        x,
        c("support", "coverage", "rhsSupport"),
        transactions = transactions,
        reuse = reuse
      )
    
    n <- .getN(x, transactions)
    n11 <- round(q$support * n)
    n1x <- round(q$coverage * n)
    nx1 <- round(q$rhsSupport * n)
    n0x <- n - n1x
    nx0 <- n - nx1
    n10 <- n1x - n11
    n01 <- nx1 - n11
    n00 <- n0x - n01
    
    if (smoothCounts > 0) {
      n <- n + 4 * smoothCounts
      n11 <- n11 + smoothCounts
      n10 <- n10 + smoothCounts
      n01 <- n01 + smoothCounts
      n00 <- n00 + smoothCounts
      
      n0x <- n0x + 2 * smoothCounts
      nx0 <- nx0 + 2 * smoothCounts
      n1x <- n1x + 2 * smoothCounts
      nx1 <- nx1 + 2 * smoothCounts
    }
    
    
    list(
      n11 = n11,
      n01 = n01,
      n10 = n10,
      n00 = n00,
      n1x = n1x,
      nx1 = nx1,
      n0x = n0x,
      nx0 = nx0,
      n = n
    )
  }

.rhsSupport <- function(x, transactions) {
  q <- quality(x)
  
  if (!is.null(q$confidence) && !is.null(q$lift)) {
    rhsSupport <- q$confidence / q$lift
    ### in case lift was NaN (0/0)
    rhsSupport[is.na(rhsSupport)] <- 0
  } else {
    if (is.null(transactions))
      stop(
        "transactions missing. Please specify the data used to mine the rules as transactions!"
      )
    if (all(diff(rhs(x)@data@p) == 1))
      ### this is a lot faster for single items in the RHS
      rhsSupport <-
        unname(itemFrequency(transactions)[rhs(x)@data@i + 1L])
    else
      rhsSupport <-
        support(rhs(x), transactions) ### multiple items in the RHS
  }
  
  return(rhsSupport)
}


## More measures (see Tan et al. Introduction to Data Mining, 2006)
# x can be a set of rules or list with counts (at least n11, n10, n01, n11 and n)

.basicRuleMeasure <- function(x,
  measure,
  transactions = NULL,
  ### adds smoothCounts to the count in each cell to avoid counts of 0
  smoothCounts = 0,
  ### k is the number of classes used by laplace
  significance = FALSE,
  ### used by chi-squared
  compliment = TRUE,
  ### k is the number of classes used by laplace
  k = 2) {
  if (is(x, "rules"))
    counts <-
      .getCounts(x, transactions, smoothCounts = smoothCounts)
  else {
    # is counts a matrix?
    if (is.matrix(x)) {
      counts <- lapply(seq_len(ncol(x)), function(i)
        x[, i])
      names(counts) <- colnames(x)
    } else
      counts <- x
    
    # complete missing counts if x has counts
    if (is.null(counts$n))
      counts$n <- counts$n11 + counts$n10 + counts$n01 + counts$n00
    if (is.null(counts$n1x))
      counts$n1x <- counts$n11 + counts$n10
    if (is.null(counts$nx1))
      counts$nx1 <- counts$n11 + counts$n01
    if (is.null(counts$n0x))
      counts$n0x <- counts$n - counts$n1x
    if (is.null(counts$nx0))
      counts$nx0 <- counts$n - counts$nx1
  }
  
  
  # note return in with just assigns to m
  m <- with(counts,
    switch(
      measure,
      table = data.frame(
        n11 = n11,
        n01 = n01,
        n10 = n10,
        n00 = n00
      ),
      support = n11 / n,
      confidence = n11 / n1x,
      lift = n * n11 / (n1x * nx1),
      coverage =  n1x / n,
      rhsSupport = nx1 / n,
      
      cosine = n11 / sqrt(n1x * nx1),
      conviction = n1x * nx0 / (n * n10),
      gini = n1x / n * ((n11 / n1x) ^ 2 + (n10 / n1x) ^ 2) - (nx1 / n) ^ 2 +
        n0x / n * ((n01 / n0x) ^ 2 + (n00 / n0x) ^ 2) - (nx0 / n) ^ 2,
      rulePowerFactor = n11 * n11 / n1x / n,
      oddsRatio = n11 * n00 / (n10 * n01),
      relativeRisk = (n11 / n1x) / (n01 / n0x),
      phi = (n * n11 - n1x * nx1) / sqrt(n1x * nx1 * n0x * nx0),
      leverage = n11 / n - (n1x * nx1 / n ^ 2),
      collectiveStrength = n11 * n00 / (n1x * nx1 + n0x + nx0) *
        (n ^ 2 - n1x * nx1 - n0x * nx0) / (n - n11 - n00),
      importance = log(((n11 + 1) * (n0x + 2)) / ((n01 + 1) * (n1x + 2)), base = 10),
      imbalance = abs(n1x - nx1) / (n1x + nx1 - n11),
      jaccard = n11 / (n1x + nx1 - n11),
      kappa = (n * n11 + n * n00 - n1x * nx1 - n0x * nx0) / (n ^ 2 - n1x * nx1 -
          n0x * nx0),
      
      lambda = {
        max_x0x1 <- apply(cbind(nx1, nx0), 1, max)
        (apply(cbind(n11, n10), 1, max) + apply(cbind(n01, n00), 1, max) -
            max_x0x1) / (n - max_x0x1)
      },
      
      mutualInformation = (
        n00 / n * log(n * n00 / (n0x * nx0)) +
          n01 / n * log(n * n01 / (n0x * nx1)) +
          n10 / n * log(n * n10 / (n1x * nx0)) +
          n11 / n * log(n * n11 / (n1x * nx1))
      ) /
        pmin(-1 * (n0x / n * log(n0x / n) + n1x / n * log(n1x / n)), -1 *
            (nx0 / n * log(nx0 / n) + nx1 / n * log(nx1 / n))),
      
      maxconfidence = pmax(n11 / n1x, n11 / nx1),
      jMeasure = n11 / n * log(n * n11 / (n1x * nx1)) +
        n10 / n * log(n * n10 / (n1x * nx0)),
      kulczynski =  (n11 / n1x + n11 / nx1) / 2,
      laplace = (n11 + 1) / (n1x + k),
      certainty = (n11 / n1x - nx1 / n) / (1 - nx1 / n),
      addedValue = n11 / n1x - nx1 / n,
      ralambondrainy = n10 / n,
      sebag = (n1x - n10) / n10,
      counterexample = (n11 - n10) / n11,
      # needs alpha
      #if(measure == "wang") return(1/n * (1-alpha) * n1x - n10)
      confirmedConfidence = (n11 - n10) / n1x,
      casualSupport = (n1x + nx1 - 2 * n10) / n,
      casualConfidence = 1 - n10 / n * (1 / n1x + 1 / nx1),
      leastContradiction = (n1x - n10) / nx1,
      centeredConfidence = nx0 / n - n10 / n1x,
      varyingLiaison = (n1x - n10) / (n1x * nx1 / n) - 1,
      yuleQ = {
        OR <- n11 * n00 / (n10 * n01)
        (OR - 1) / (OR + 1)
      },
      yuleY = {
        OR <- n11 * n00 / (n10 * n01)
        (sqrt(OR) - 1) / (sqrt(OR) + 1)
      },
      lerman = (n11 - n1x * nx1 / n) / sqrt(n1x * nx1 / n),
      implicationIndex = (n10 - n1x * nx0 / n) / sqrt(n1x * nx0 / n),
      doc = (n11 / n1x) - (n01 / n0x),
      
      chiSquared = {
        chi2 <- numeric(length(x))
        
        for (i in seq_len(length(x))) {
          fo <- matrix(c(n00[i], n01[i], n10[i], n11[i]), ncol = 2)
          #fe <- tcrossprod(c(nx0[i], nx1[i]), ic(n0x[i], n1x[i])) / n
          ## check if approximation is ok
          ## we don't do this now
          ##if(any(fe < 5)) chi2[i] <- nA
          ##else
          #chi2[i] <- sum((fo - fe) ^ 2 / fe)
          
          # warning about approximation
          suppressWarnings(chi2[i] <-
              stats::chisq.test(fo, correct = FALSE)$statistic)
        }
        
        ## the chi square test has 1 df for a 2x2 contingency table.
        ## The critical value at alpha=0.05 is:
        ## qchisq(0.05, df =1, lower.tail=FALSE)
        ## [1] 3.841459
        if (!significance)
          chi2
        else
          stats::pchisq(q = chi2,
            df = 1,
            lower.tail = !compliment)
      }
    ))
  
  if (is.null(m))
    stop("Specified measure not implemented.")
  
  m
}


## RLD see Kenett and Salini 2008
## RLD code contributed by Silvia Salini

.RLD <- function(x, ...) {
  counts <- .getCounts(x, ...)
  RLD <- with(counts, {
    RLD <- numeric(length(x))
    for (i in seq_len(length(x))) {
      D <- (n11[i] * n00[i] - n10[i] * n01[i]) / n
      if (D > 0)
        if (n01[i] < n10[i])
          RLD[i] <- D / (D + n01[i])
      else
        RLD[i] <- D / (D + n10[i])
      else
        if (n11[i] < n00[i])
          RLD[i] <- D / (D - n11[i])
        else
          RLD[i] <- D / (D - n00[i])
    }
    RLD
  })
  
  RLD[!is.finite(RLD)] <- NA
  
  RLD
}

## Standardising the Lift of an Association Rule
# by McNicholas, 2008, DOI: 10.1016/j.csda.2008.03.013

.stdLift <-
  function(rules,
    transactions = NULL,
    correct = TRUE) {
    measures <- interestMeasure(rules,
      c("support", "confidence", "lift", "coverage", "rhsSupport"),
      transactions = transactions)
    
    n <- info(rules)$ntransactions
    if (is.null(n)) {
      if (is.null(transactions))
        stop("rules do not contain info from transactions. transactions are needed.")
      n <- length(transactions)
    }
    
    supp_A <- measures$coverage
    supp_B <- measures$rhsSupport
    
    # upper bound of lift
    lambda <- pmax(supp_A + supp_B - 1, 1 / n) / (supp_A * supp_B)
    
    # correct lambda for min. confidence and min. support
    if (correct) {
      c <- info(rules)$confidence
      s <- info(rules)$support
      
      if (!is.null(c) && !is.null(s))
        lambda <-
        pmax(lambda, 4 * s / (1 + s) ^ 2, s / (supp_A * supp_B), c / supp_B)
      else
        warning(
          "minimum support or confidence not available in info(x). Using uncorrected stdLift instead."
        )
    }
    
    # lower bound of lift
    upsilon <- 1 / pmax(supp_A, supp_B)
    
    stdLift <- (measures$lift - lambda) / (upsilon - lambda)
    stdLift[is.nan(stdLift)] <- 1
    stdLift
  }
