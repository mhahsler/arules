---
title: A Probabilistic Comparison of Commonly Used Interest Measures for Association
  Rules
author: "Michael Hahsler"
abstract: "This document contains a comprehensive collection of commonly used measures of
  significance and interestingness (sometimes also called strength) 
  for association rules and itemsets. 
  Interest measures are usually defined in terms of itemset support and counts. 
  Here, we also present their relationship with estimating probabilities and 
  conditional probabilities."
output:
  html_document:
    toc: yes
    toc_depth: 2
    toc_float:
      collapsed: no
      smooth_scroll: no
  pdf_document:
    toc: yes
    toc_depth: '2'
bibliography: association_rules.bib
link-citations: yes
editor_options: 
  markdown: 
    wrap: 72
---

# About this Document

**Latest update:** September 6, 2026

Please cite this document as **Michael Hahsler, A Probabilistic Comparison of
Commonly Used Interest Measures for Association Rules, 2015, URL:
<https://mhahsler.github.io/arules/docs/measures>**

A PDF version of this document is available at
<https://mhahsler.github.io/arules/docs/measures.pdf>. An annotated
bibliography of association rules can be found at
<https://mhahsler.github.io/arules/docs/association_rules.html>.

<img src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" alt="CC BY-SA 4.0" style="float:left; margin:4px;"/>
This work is licensed under the
<a href="http://creativecommons.org/licenses/by-sa/4.0/">Creative
Commons Attribution Share Alike 4.0 International License.</a> 



## Code and Implementation

All measures discussed on this page are implemented in function
[interestMeasure()](https://search.r-project.org/CRAN/refmans/arules/html/interestMeasure.html)
in the freely available software:

* R package: [arules](https://github.com/mhahsler/arules)
* Python module: [arulespy](https://pypi.org/project/arulespy/)

## Corrections and Feedback

For corrections and missing measures on this page or in the
implementation in the package arules, please [open an issue on
GitHub](https://github.com/mhahsler/arules/issues) or contact
[me](http://michael.hahsler.net) directly.

# Definitions

@arules:Agrawal:1993 define [association rule
mining](https://en.wikipedia.org/wiki/Association_rule_learning) in the
following way:

Let $I=\{i_1, i_2,\ldots,i_m\}$ be a set of $m$ binary attributes called
**items.** Let $D = \{t_1, t_2, \ldots, t_n\}$ be a set of transactions
called the **database**. Each transaction $t \in D$ has a unique
transaction ID and contains a subset of the items in $I$, i.e.,
$t \subseteq I$. A **rule** is defined as an implication of the form
$X \Rightarrow Y$ where $X, Y \subseteq I$ and $X \cap Y = \emptyset$.
The sets of items (or **itemsets**) $X$ and $Y$ are called the antecedent
(left-hand side or LHS) and consequent (right-hand side or RHS) of the
rule, respectively. Measures of importance (interest) can be
defined for itemsets and rules. The support-confidence framework defines
the measures [support](#support) and [confidence](#confidence). Rules
that satisfy user-specified minimum thresholds on support and
confidence are called **association rules.**

Interest measures are usually defined in terms of itemset
[support](#support). Here, we also present them using probabilities and,
where appropriate, counts. The probability $P(E_X)$ of the event that
all items in itemset $X$ are contained in an arbitrarily chosen
transaction can be estimated from a database $D$ using maximum
likelihood estimation (MLE) by

$$\hat{P}(E_X) = \frac{|\{t \in D; X \subseteq t\}|}{n}$$

where $n_X = |\{t \in D; X \subseteq t\}|$ is the number of transactions
that contain the itemset $X$, and $n = |D|$ is the size
(number of transactions) of the database. For conciseness of notation,
we will drop the hat and the $E$ from the notation for probabilities. We
will use in the following $P(X)$ to mean $\hat{P}(E_X)$ and
$P(X \cap Y)$ to mean $\hat{P}(E_X \cap E_Y) = \hat{P}(E_{X \cup Y})$,
the probability of the intersection of the events $E_X$ and $E_Y$—that
is, the probability that a transaction contains
all items in the union of the itemsets $X$ and $Y$. The event notation
should not be confused with the set notation used in measures like
support, where $supp(X \cup Y)$ means the support of the union of the
itemsets $X$ and $Y$.

**Note on probability estimation:** These probability estimates can be
very poor for itemsets with low observed frequencies. This limitation
affects most measures discussed below and should always be considered.

**Note on null transactions:** Transaction data sets typically contain a
large number of transactions that do not contain either $X$ or $Y$.
These transactions are called null transactions, and it is desirable
that measures of rule strength are not influenced by a change in the
number of null transactions. However, most measures are affected by the
number of null transactions since the total number of transactions is
used for probability estimation. Measures that are not influenced by a
change in the number of null transactions are called null-invariant
[@arules:Tan:2004; @arules:Wu:2010].

Good overview articles about different association rule measures are

-   @arules:Tan:2004 Selecting the right objective measure for
    association analysis. *Information Systems,* 29(4):293-313, 2004

-   @arules:Geng:2006 Interestingness measures for data mining: A
    survey. *ACM Computing Surveys,* 38(3):9, 2006.

-   @arules:Lenca:2007 Association Rule Interestingness Measures:
    Experimental and Theoretical Studies. *Studies in Computational
    Intelligence (SCI)* 43, 51--76, 2007.

# Measures Defined on Itemsets

## Support {#support}

**Reference:** @arules:Agrawal:1993

$$
supp(X) 
= \frac{n_X}{n}
= P(X)
$$

Support is defined on itemsets and gives the proportion of transactions
that contain $X$. It is used as a measure of significance (importance)
of an itemset. Since it uses the count of transactions, it is often
called a **frequency constraint.** An itemset with support at least as
large as a specified minimum support threshold, $supp(X) \ge \sigma$,
is called a
**frequent or large itemset.**

For a rule, support is defined as the support of all items in the rule,
i.e., $supp(X \Rightarrow Y) = supp(X \cup Y) = P(X \cap Y)$.

Support has the **downward-closure property (anti-monotonicity),** which
means that all subsets of a
frequent set are also frequent. This property (actually, the fact that
no superset of an infrequent set can be frequent) is used to prune the
search space (usually thought of as a lattice or tree of itemsets with
increasing size) in level-wise algorithms (e.g., the Apriori algorithm).

The disadvantage of support is the **rare item problem.** Items that
occur very infrequently in the data set are pruned, although they would
still produce interesting and potentially valuable rules. The rare item
problem is important for transaction data, which usually have a very
uneven distribution of support across items (typically a power-law
distribution in which a few items occur frequently and most occur
rarely).

**Range:** $[0, 1]$

## Support Count {#count}

**Alias:** Absolute Support Count

**Range:** $\{0,1,\ldots,n\}$, where $n$ is the number of transactions.

## All-Confidence {#allconfidence}

**Reference:** @arules:Omiecinski:2003

All-confidence is defined on itemsets (not rules) as

$$\textrm{all-confidence}(X)
= \frac{supp(X)}{\max_{x \in X} supp(\{x\})}
= \frac{P(X)}{\max_{x \in X} P(\{x\})}$$

where $\max_{x \in X} supp(\{x\})$ is the support of the item with the
highest support in $X$. All-confidence means that all rules that can be
generated from itemset $X$ have at least a confidence of
$\textrm{all-confidence}(X)$. All-confidence possesses the
downward-closure property and thus can be effectively used inside
mining algorithms. All-confidence is null-invariant.

**Range:** $[0, 1]$

## Cross-Support Ratio {#crosssupportratio}

**Reference:** @arules:Xiong:2003

Defined on itemsets as the ratio of the support of the least frequent
item to the support of the most frequent item, i.e.,

$$\textrm{cross-support}(X) =
\frac{\min_{x \in X} supp(\{x\})}{\max_{x \in X} supp(\{x\})}$$

An itemset is a cross-support pattern if this ratio is smaller than a
specified threshold. Such patterns combine frequent and rare items and
are often spurious.

**Range:** $[0, 1]$

# Measures Defined on Rules

## Contingency Table {#table}

A $2 \times 2$ [contingency
table](https://en.wikipedia.org/wiki/Contingency_table) with counts for
rule $X \Rightarrow Y$ in the transaction dataset. The counts are:

|                |         $Y$         |         $\overline{Y}$         |
|:--------------:|:-------------------:|:------------------------------:|
|      $X$       |      $n_{XY}$       |      $n_{X\overline{Y}}$       |
| $\overline{X}$ | $n_{\overline{X}Y}$ | $n_{\overline{X}\overline{Y}}$ |

$n_{XY}$ is the number of transactions that contain all items in $X$ and
$Y$. All other measures for rules can be calculated using these counts.

## Confidence {#confidence}

**Alias:** Strength

**Reference:** @arules:Agrawal:1993

$$conf(X \Rightarrow Y) = \frac{supp(X \Rightarrow Y)}{supp(X)} = 
\frac{supp(X \cup Y)}{supp(X)} =
\frac{n_{XY}}{n_X} =
\frac{P(X \cap Y)}{P(X)} = P(Y | X)$$

Confidence is defined as the proportion of transactions that contain $Y$
among those that contain $X$. This proportion estimates the probability
of observing the rule's consequent given that a transaction contains the
antecedent.

Confidence is directed and gives different values for the rules
$X \Rightarrow Y$ and $Y \Rightarrow X$. Association rules must
satisfy a minimum confidence constraint,
$conf(X \Rightarrow Y) \ge \gamma$.

Confidence is not downward closed and was developed together with
support by Agrawal et al. (the so-called support-confidence framework).
Support is first used to find frequent (significant) itemsets exploiting
its downward closure property to prune the search space. Then confidence
is used in a second step to produce rules from the frequent itemsets
that exceed a minimum confidence threshold.

A limitation of confidence is its sensitivity to the frequency of the
consequent $Y$. Consequents with higher support tend to produce higher
confidence values even when the items are independent.

**Range:** $[0, 1]$

## Added Value {#addedvalue}

**Alias:** AV, Pavillon Index, Centered Confidence

**Reference:** @arules:Tan:2004

Added value quantifies how much the probability of $Y$ changes after
conditioning on transactions that contain $X$. It is defined as

$$AV(X \Rightarrow Y) = conf(X \Rightarrow Y) - supp(Y) = P(Y | X) - P(Y)$$

**Range:** $[-1+1/n, 1-1/n]$ for an empirical data set with $n$
transactions (0 indicates independence)

## Causal Confidence {#casualconfidence}

**Reference:** @arules:Kodratoff:2001

Causal confidence combines a rule's confidence with the confidence of
its contrapositive:

$$\textrm{causal-conf} =
\frac{1}{2} [conf(X \Rightarrow Y)
+ conf(\overline{Y} \Rightarrow \overline{X})] =
\frac{1}{2} [P(Y|X) + P(\overline{X}|\overline{Y})]$$

**Range:** $[0, 1]$

For backward compatibility, `interestMeasure()` exposes this measure as
`"casualConfidence"`.

## Causal Support {#casualsupport}

**Reference:** @arules:Kodratoff:2001

Causal support adds the proportion of examples to the proportion of
contrapositive examples:

$$\textrm{causal-supp} =
supp(X \cup Y) + supp(\overline{X} \cup \overline{Y}) =
P(X \cap Y) + P(\overline{X} \cap \overline{Y})$$

**Range:** $[0, 1]$

For backward compatibility, `interestMeasure()` exposes this measure as
`"casualSupport"`.

## Centered Confidence {#centeredconfidence}

**Alias:** relative accuracy, gain

**Reference**: @arules:Nada:1999

$$CC(X \Rightarrow Y) = conf(X \Rightarrow Y) - supp(Y)$$

**Range:** $[-1+1/n, 1-1/n]$

## Certainty Factor {#certainty}

**Alias:** CF, Loevinger

**Reference:** @arules:Galiano:2002

The certainty factor measures the change in the probability of $Y$ when
conditioning on $X$, scaled by the maximum possible positive change.
Positive values indicate positive association, and negative values
indicate negative association.

$$CF(X \Rightarrow Y) = \frac{conf(X \Rightarrow Y)-supp(Y)}{supp(\overline{Y})}
= \frac{P(Y|X)-P(Y)}{1-P(Y)}$$

**Range:** $(-\infty, 1]$ for this positive-change normalization (0
indicates independence)

## Chi-Squared {#chisquared}

**Reference:** @arules:Brin:1997b

For the analysis of $2 \times 2$ contingency tables, the [chi-squared
test statistic](https://en.wikipedia.org/wiki/Chi-squared_test) is a
measure of the relationship between two binary variables ($X$ and $Y$).
It can be used to test independence between $X$ and $Y$:

$$
\begin{aligned}
\textrm{chi-squared}(X \Rightarrow Y) 
& = \sum_i \frac{(O_i - E_i)^2}{E_i} \\
& = \frac{\left( n_{XY} - \frac{n_X n_Y}{n} \right)^2}{\frac{n_X n_Y}{n}}
+ \frac{\left( n_{\overline{X}Y} - \frac{n_{\overline{X}} n_Y}{n} \right)^2}{\frac{n_{\overline{X}} n_Y}{n}}
+ \frac{\left( n_{X\overline{Y}} - \frac{n_X n_{\overline{Y}}}{n} \right)^2}{\frac{n_X n_{\overline{Y}}}{n}}
+ \frac{\left( n_{\overline{X}\overline{Y}} - \frac{n_{\overline{X}} n_{\overline{Y}}}{n} \right)^2}{\frac{n_{\overline{X}} n_{\overline{Y}}}{n}} \\
& = n \frac{\left[P(X \cap Y)P(\overline{X} \cap \overline{Y})
- P(X \cap \overline{Y})P(\overline{X} \cap Y)\right]^2}
{P(X)P(Y)P(\overline{X})P(\overline{Y})}
\end{aligned}
$$

$O_i$ is the observed count of contingency table cell $i$ and $E_i$ is
the expected count given the marginals.\
The statistic has approximately a $\chi^2$ distribution with 1 degree of
freedom for a $2 \times 2$ contingency table. The critical value for
$\alpha=0.05$ is $3.84$; higher chi-squared values indicate that the
null hypothesis of independence between the LHS and RHS should be
rejected. Larger values indicate stronger evidence of an association.
The statistic can be converted into a p-value using the $\chi^2$
distribution.

**Notes:** The contingency tables for some rules may contain cells with
low expected values (less than 5), so [Fisher's exact
test](#fishersexacttest) might be more appropriate. Each rule
represents a statistical test, and
<a href="https://en.wikipedia.org/wiki/Multiple_comparisons_problem">
correction for multiple comparisons</a> may be necessary.

**Range:** $[0, \infty)$

## Collective Strength {#collectivestrength}

**Reference:** @arules:Aggarwal:1998

$$
\begin{aligned}
S(X,Y)
&= \frac{1-v(X,Y)}{1-E[v(X,Y)]}\frac{E[v(X,Y)]}{v(X,Y)} \\
&= \frac{P(X \cap Y)+P(\overline{X}\cap\overline{Y})}
{P(X)P(Y)+P(\overline{X})P(\overline{Y})}
\frac{P(X)P(\overline{Y})+P(\overline{X})P(Y)}
{P(X\cap\overline{Y})+P(\overline{X}\cap Y)}.
\end{aligned}
$$

where $v(X,Y)$ is the violation rate and $E[v(X,Y)]$ is its expected
violation rate for independent items. The violation rate is defined as
the fraction of transactions that contain some, but not all, of the
items. Collective strength gives 0 for perfectly negatively correlated
items, infinity for perfectly positively correlated items, and 1 if the
items co-occur as expected under independence.

For items with medium to low probabilities, the expected violation rate
can be dominated by the proportion of transactions that contain neither
item. In that case, collective strength produces values close to one,
even if the itemset appears several times more often than expected
to occur together.

**Range:** $[0, \infty)$

## Confidence Boost {#boost}

**Reference:** @arules:Balcazar:2013

Confidence boost is the ratio of a rule's confidence to the highest
confidence of any more general rule (i.e., a rule with the same
consequent and one or more items removed from the LHS).

$$\textrm{confidence-boost}(X \Rightarrow Y) =  
\frac{conf(X \Rightarrow Y)}{\max_{X' \subset X} conf(X' \Rightarrow Y)} =
\frac{conf(X \Rightarrow Y)}{conf(X \Rightarrow Y) - improvement(X \Rightarrow Y)} $$

Values larger than 1 mean the new rule boosts the confidence compared to
the best, more general rule. The measure is related to the [improvement
measure](#improvement).

**Range:** $[0, \infty)$ ($>1$ indicates a rule with confidence boost)

## Conviction {#conviction}

**Reference:** @arules:Brin:1997

$$\mathrm{conviction}(X \Rightarrow Y) =\frac{1-supp(Y)}{1-conf(X \Rightarrow Y)} 
= \frac{P(X)P(\overline{Y})}{P(X \cap \overline{Y})}$$

where $\overline{Y} = E_{\neg Y}$ is the event that $Y$ does not appear
in a transaction. Conviction was developed as an alternative to
confidence, which does not adequately capture the direction of
associations on its own. Conviction compares the probability that $X$
appears without $Y$ under independence with the observed frequency of
$X$ without $Y$. In that respect, it is similar to lift (see the section
about lift on this page). However, in contrast to lift, it is a directed
measure since it also uses the information of the absence of the
consequent. An interesting fact is that conviction is monotone in
confidence and lift.

**Range:** $[0, \infty)$ (1 indicates independence; rules that always
hold have $\infty$)

## Cosine {#cosine}

**Reference:** @arules:Tan:2004

Cosine is a null-invariant measure of correlation between the items in
$X$ and $Y$ defined as

$$\mathrm{cosine}(X \Rightarrow Y) 
= \frac{supp(X \cup Y)}{\sqrt{(supp(X)supp(Y))}} 
= \frac{P(X \cap Y)}{\sqrt{P(X)P(Y)}}
= \sqrt{P(X | Y) P(Y | X)}$$

**Range:** $[0, 1]$ (1 means that the two transaction sets are identical;
independence does not correspond to a fixed cosine value)

## Coverage {#coverage}

**Alias:** LHS Support

It measures the probability that a rule $X \Rightarrow Y$ applies to a
randomly selected transaction. It is estimated by the proportion of
transactions that contain the antecedent of the rule $X \Rightarrow Y$.
Therefore, coverage is sometimes called antecedent support or LHS
support.

$$\mathrm{cover}(X \Rightarrow Y) = supp(X) = P(X)$$

**Range:** $[0, 1]$

## Descriptive Confirmed Confidence {#confirmedconfidence}

**Reference:** @arules:Tan:2004

Descriptive confirmed confidence contrasts the rule's confidence with
the confidence of its negated consequent.

$$\textrm{confirmed-conf} = conf(X \Rightarrow Y) - conf(X \Rightarrow \overline{Y}) 
= P(Y|X) - P(\overline{Y}|X)$$

**Range:** $[-1, 1]$

## Difference of Confidence {#doc}

**Alias:** DOC, Difference of Proportions

**Reference:** @arules:Hofmann:2001

The difference of confidence is the difference between the proportions of
transactions containing $Y$ in the two groups of transactions that do
and do not contain $X$. For the analysis of $2 \times 2$ contingency
tables, this measure of the relationship between two binary variables is
typically called the difference of proportions. It is defined as

$$
\mathrm{doc}(X \Rightarrow Y) 
= conf(X \Rightarrow Y) - conf(\overline{X} \Rightarrow Y) 
= P(Y|X) - P(Y|\overline{X})
= n_{XY} / n_X - n_{\overline{X}Y} / n_{\overline{X}}
$$

**Range:** $[-1, 1]$ (0 means statistical independence)

## Example and Counter-Example Rate {#counterexample}

This measure contrasts the example and counterexample rates.

It is defined as

$$\mathrm{ecr}(X \Rightarrow Y) =
\frac{n_{XY} - n_{X\overline{Y}}}{n_{XY}} =
\frac{P(X \cap Y) - P(X \cap \overline{Y})}{P(X \cap Y)} =
1 - \frac{1}{sebag(X \Rightarrow Y)}
$$

The measure is related to the [Sebag-Schoenauer
Measure](#sebag).

**Range:** $(-\infty, 1]$

## Fisher's Exact Test {#fishersexacttest}

**Reference:** @arules:Hahsler:2007

If $X$ and $Y$ are independent, then $n_{XY}$ is a realization of
the random variable $C_{XY}$ which has a hypergeometric distribution
with $n_Y$ draws from a population with $n_X$ successes and
$n_{\overline{X}}$ failures. The p-value for [Fisher's one-sided exact
test](https://en.wikipedia.org/wiki/Fisher%27s_exact_test) giving the
probability of observing a contingency table with a count of at least
$n_{XY}$ given the observed marginal counts is

$$
\textrm{p-value} = P(C_{XY} \ge n_{XY}) 
$$

The p-value is related to [hyper-confidence](#hyperconfidence).
Compared to the [chi-squared test](#chisquared), Fisher's exact test
also applies when cells have low expected counts. Note that each rule
represents a statistical test, and [correction for multiple
comparisons](https://en.wikipedia.org/wiki/Multiple_comparisons_problem)
may be necessary.

**Range:** $[0, 1]$ (p-value scale)

## Generalized Improvement {#generalizedImprovement}

**Reference:** @arules:Hahsler:2023

This measure generalizes [improvement](#improvement) to arbitrary
interest measures.

$$
\mathrm{generalizedImprovement}(X \Rightarrow Y) 
= \min_{X' \subset X}\left[M(X \Rightarrow Y) - M(X' \Rightarrow Y)\right]
$$

where $M$ can be any measure that increases with interestingness.
The original definition of improvement uses the measure confidence.

**Range:** $(-\infty, \infty)$ (the actual range depends on the measure)

## Generalized Increase Ratio {#ginc}

**Reference:** @arules:Hahsler:2023

This measure generalizes [lift increase](#lic) to arbitrary interest
measures.

$$
\mathrm{INC}(X \Rightarrow Y) 
= \min_{X' \subset X} \left[ \frac{M(X \Rightarrow Y)}{M(X' \Rightarrow Y)} \right]
$$

where $M$ can be any positive interest measure. The original definition
of lift increase uses lift.

**Range:** $[0, \infty)$ ($> 1$ means an increase)

## Gini Index {#gini}

**Reference:** @arules:Tan:2004

The [Gini index](https://en.wikipedia.org/wiki/Gini_coefficient)
measures quadratic entropy as

$$\mathrm{gini}(X \Rightarrow Y) =
P(X)    [P(Y|X)^2+P(\overline{Y}|X)^2] +
P(\overline{X}) [P(Y|\overline{X})^2+P(\overline{Y}|\overline{X})^2] -
P(Y)^2 - P(\overline{Y})^2
$$

**Range:** $[0, 1/2]$ (0 means that the rule provides no
information about the data set)

## Hyper-Confidence {#hyperconfidence}

**Reference:** @arules:Hahsler:2007

Hyper-confidence measures unexpectedly high or low co-occurrence counts
under the hypergeometric model. Under independence, the co-occurrence
count is represented by a random variable $C_{XY}$ whose distribution is
determined by $n$, $n_X$, and $n_Y$. Hyper-confidence for unexpectedly
high co-occurrence is

$$ 
\textrm{hyper-conf}(X \Rightarrow Y) 
= 1 - P(C_{XY} \ge n_{XY} \mid n, n_X, n_Y)
$$

A confidence level greater than 0.95 indicates that the probability of
observing a count at least this large under independence is less than 5%.
For positive associations, hyper-confidence is one minus the one-sided
p-value from [Fisher's exact test](#fishersexacttest). Note that each
rule represents a statistical test and [correction for multiple
comparisons](https://en.wikipedia.org/wiki/Multiple_comparisons_problem)
may be necessary.

Hyper-confidence can also be used to detect substitutes, for which the
observed co-occurrence count is unexpectedly low:

$$ 
\textrm{hyper-conf}_\textrm{substitute}(X \Rightarrow Y)
= 1 - P(C_{XY} \le n_{XY} \mid n, n_X, n_Y)
$$

**Range:** $[0, 1]$

## Hyper-Lift {#hyperlift}

**Reference:** @arules:Hahsler:2007

Hyper-lift adapts lift by replacing the expected count under
independence, $E[C_{XY}] = n_X n_Y/n$, with a high quantile of the
hypergeometric count distribution. This is more
robust for low counts and results in fewer false positives when
hyper-lift is used for rule filtering. Hyper-lift is defined as:

$$
\textrm{hyper-lift}_\delta(X \Rightarrow Y) = 
\frac{n_{XY}}{Q_{\delta}[C_{XY}]}
$$

where $n_{XY}$ is the number of transactions containing $X$ and $Y$ and
$Q_{\delta}[C_{XY}]$ is the $\delta$-quantile of the hypergeometric
distribution determined by $n$, $n_X$, and $n_Y$. The value of $\delta$
is typically chosen as 0.99 or 0.95.

**Range:** $[0, \infty)$ (a value greater than 1 means that the observed
count exceeds the selected quantile)

## Imbalance Ratio {#imbalance}

**Alias:** IR

**Reference:** @arules:Wu:2010

The imbalance ratio measures the difference in the marginal frequencies
of the LHS and RHS. The ratio is close to 0 if the
conditional probabilities are similar (i.e., very balanced) and close to
1 if they are very different. It is defined as

$$
\mathrm{IB}(X \Rightarrow Y) 
= \frac{|P(X|Y) - P(Y|X)|}{P(X|Y) + P(Y|X) - P(X|Y)P(Y|X)}
= \frac{|supp(X) - supp(Y)|}{supp(X) + supp(Y) - supp(X \cup Y)}
$$

**Range:** $[0, 1]$ (0 indicates balanced marginal frequencies)

## Implication Index {#implicationindex}

**Reference:** @arules:Gras:1996

A variation of the [Lerman similarity](#lerman) defined as

$$
\mathrm{gras}(X \Rightarrow Y)
= \sqrt{n}\,
\frac{supp(X \cup \overline{Y}) - supp(X)supp(\overline{Y})}
{\sqrt{supp(X)supp(\overline{Y})}}
$$

Lower values indicate fewer counterexamples than expected under
independence and therefore stronger implication.

**Range:** $(-\infty, \infty)$

## Importance {#importance}

**Reference:**
<a href="https://docs.microsoft.com/en-us/sql/analysis-services/data-mining/microsoft-association-algorithm-technical-reference">MS
Analysis Services: Microsoft Association Algorithm Technical
Reference.</a>

In the Microsoft Association Algorithm Technical Reference, confidence
is called "probability," and a measure called importance is defined as
the log-likelihood of the right-hand side of the rule, given the
left-hand side of the rule:

$$
\mathrm{importance}(X \Rightarrow Y)
= \log_{10}\!\left(\frac{L(X \Rightarrow Y)}
{L(\overline{X} \Rightarrow Y)}\right)
$$

where $L$ is the [Laplace corrected
confidence](#laplace).

**Range:** $(-\infty, \infty)$

## Improvement {#improvement}

**Reference:** @arules:Bayardo:2000

The improvement of a rule is the minimum difference between its
confidence and the confidence of any proper sub-rule with the same
consequent. A large positive value indicates that the more specific rule
(with an additional item in the LHS) improves the confidence and should
be kept. Improvement is often used to filter redundant rules.

$$
\mathrm{improvement}(X \Rightarrow Y) 
= \min_{X' \subset X}\left[conf(X \Rightarrow Y) - conf(X' \Rightarrow Y)\right]
$$

**Range:** $[-1, 1]$

## Jaccard Coefficient {#jaccard}

**Reference:** @arules:Tan:2004

A null-invariant measure for dependence using the [Jaccard similarity
index](https://en.wikipedia.org/wiki/Jaccard_index) between the two sets
of transactions that contain the items in $X$ and $Y$, respectively.
Defined as

$$
\mathrm{jaccard}(X \Rightarrow Y) 
= \frac{supp(X \cup Y)}{supp(X) + supp(Y) - supp(X \cup Y)}
= \frac{P(X \cap Y)}{P(X)+P(Y)-P(X \cap Y)}
$$

**Range:** $[0, 1]$

## J-Measure {#jmeasure}

**Reference:** @arules:Smyth:1991

The J-measure is a scaled version of cross entropy to measure the
information content of a rule.

$$
J(X \Rightarrow Y) 
= P(X \cap Y) \log\left(\frac{P(Y|X)}{P(Y)}\right) +
P(X \cap \overline{Y}) \log\left(\frac{P(\overline{Y}|X)}{P(\overline{Y})}\right)
$$

**Range:** $[0, 1/e]$ when natural logarithms are used (0 means that
$X$ provides no information about $Y$)

## Kappa {#kappa}

**Alias:** Cohen's $\kappa$

**Reference:** @arules:Tan:2004

For a rule viewed as a classifier, [Cohen's kappa
coefficient](https://en.wikipedia.org/wiki/Cohen%27s_kappa) adjusts the
observed accuracy, $P(X \cap Y)+P(\overline{X}\cap\overline{Y})$, by the
accuracy expected under independence. Kappa is defined as

$$
\kappa(X \Rightarrow Y) 
= \frac{P(X \cap Y) + P(\overline{X} \cap \overline{Y}) - 
P(X)P(Y) - P(\overline{X})P(\overline{Y})}{1- P(X)P(Y) - 
P(\overline{X})P(\overline{Y})}
$$

**Range:** $[-1,1]$ (0 means the rule is not better than a random
classifier)

## Klösgen {#klosgen}

**Reference:** @arules:Tan:2004

Defined as a scaled version of the [added value measure](#addedvalue).

$$
\begin{aligned}
\mathrm{klosgen}(X \Rightarrow Y) 
& = \sqrt{supp(X \cup Y)}\,(conf(X \Rightarrow Y) - supp(Y)) \\ 
& = \sqrt{P(X \cap Y)}\, (P(Y|X) - P(Y)) \\
& = \sqrt{P(X \cap Y)}\, AV(X \Rightarrow Y)
\end{aligned}
$$

**Range:** $[-1, 1]$ (0 for independence)

## Kulczynski {#kulczynski}

**Reference:** @arules:Wu:2010

The null-invariant Kulczynski measure averages confidence in both
directions.

$$
\begin{aligned}
\mathrm{kulc}(X \Rightarrow Y)
& = \frac{1}{2} 
\left(conf(X \Rightarrow Y) + conf(Y \Rightarrow X) \right)
= \frac{1}{2} 
\left(\frac{supp(X \cup Y)}{supp(X)} + \frac{supp(X \cup Y)}{supp(Y)} \right) \\
& = \frac{1}{2} 
\left(P(X | Y) + P(Y | X) \right)
\end{aligned}
$$

**Range:** $[0, 1]$ (independence does not correspond to a fixed value)

## Lambda {#lambda}

**Alias:** Goodman-Kruskal's $\lambda$, Predictive Association

**Reference:** @arules:Tan:2004

Goodman and Kruskal's lambda assesses the association between the LHS
and RHS of the rule.

$$
\lambda(X \Rightarrow Y)
= \frac{\max(n_{XY},n_{X\overline{Y}})
+ \max(n_{\overline{X}Y},n_{\overline{X}\overline{Y}})
- \max(n_Y,n_{\overline{Y}})}
{n-\max(n_Y,n_{\overline{Y}})}
$$

**Range:** $[0, 1]$

## Laplace Corrected Confidence {#laplace}

**Alias:** Laplace Accuracy, L

**Reference:** @arules:Tan:2004

$$
L(X \Rightarrow Y) = \frac{n_{XY}+1}{n_X+k},
$$

where $k$ is the number of classes in the domain. For association rules,
$k$ is often set to 2. It is an approximate measure of the expected rule
accuracy, representing one minus the Laplace expected error estimate of
the rule. The Laplace-corrected accuracy estimate decreases with lower
support to account for estimation uncertainty with low counts.

**Range:** $[0, 1]$

## Least Contradiction {#leastcontradiction}

**Reference:** @arules:Aze:2002

$$
\textrm{least-contradiction}(X \Rightarrow Y)
= \frac{supp(X \cup Y) - supp(X \cup \overline{Y})}{supp(Y)}
= \frac{P(X \cap Y) - P(X \cap \overline{Y})}{P(Y)}
$$

**Range:** $(-\infty, 1]$

## Lerman Similarity {#lerman}

**Reference:** Lerman, I.C. (1981). *Classification et analyse ordinale
des données*. Paris.

Defined as

$$
\mathrm{lerman}(X \Rightarrow Y) 
= \frac{n_{XY} - \frac{n_X n_Y}{n}}{\sqrt{\frac{n_X n_Y}{n}}}
= \sqrt{n} \frac{supp(X \cup Y) - supp(X)supp(Y)}{\sqrt{supp(X)supp(Y)}} 
$$

**Range:** $(-\infty, \infty)$

## Leverage {#leverage}

**Alias:** Piatetsky-Shapiro, PS

**Reference:** @arules:Piatetsky-Shapiro:1991 $$
\mathrm{PS}(X \Rightarrow Y) = leverage(X \Rightarrow Y) 
= supp(X \Rightarrow Y) - supp(X)supp(Y) 
= P(X \cap Y) - P(X)P(Y)
$$

Leverage measures the difference of $X$ and $Y$ appearing together in
the data set and what would be expected if $X$ and $Y$ were
statistically independent. In a sales setting, it describes how many
more (or fewer) baskets contain both $X$ and $Y$ than expected under
independence.

Using minimum leverage thresholds incorporates at the same time an
implicit frequency constraint. For example, with a minimum leverage
threshold of 0.01% (corresponding to 10 occurrences in a data set with
100,000 transactions), one can first find all itemsets with minimum
support of 0.01% and then filter the resulting itemsets
using the leverage constraint. Because of this property, leverage also
can suffer from the rare item problem.

Leverage is an unnormalized version of the [phi correlation
coefficient](#phi).

**Range:** $[-1/4, 1/4]$ (0 indicates independence)

## Lift {#lift}

**Alias:** Interest, interest factor

**Reference:** @arules:Brin:1997

Lift was originally called interest by Brin et al. Later, lift, the name
of an equivalent measure popular in advertising and predictive modeling
became more common. Lift is defined as

$$
\textrm{lift}(X \Rightarrow Y) 
= \textrm{lift}(Y \Rightarrow X) 
= \frac{conf(X \Rightarrow Y)}{supp(Y)} 
= \frac{P(Y | X)}{P(Y)} 
= \frac{P(X \cap Y)}{P(X)P(Y)}
= n \frac{n_{XY}}{n_X n_Y}
$$

Lift measures how many times more often $X$ and $Y$ occur together than
expected if they were statistically independent. A lift value of 1
indicates independence between $X$ and $Y$. For statistical tests, see
the [chi-squared test statistic](#chisquared), [Fisher's exact
test](#fishersexacttest), and [hyper-confidence](#hyperconfidence).

Lift is not downward closed and does not suffer from the rare item
problem. However, lift is susceptible to noise in small databases. Rare
itemsets with low counts (low probability), which by chance occur a few
times (or only once) together, can produce enormous lift values.

**Range:** $[0, \infty)$ (1 means independence)

## Lift Increase {#lic}

**Reference:** @arules:Lopez:2014

Lift increase is related to [improvement](#improvement), but uses
[lift](#lift). It divides a rule's lift by the largest lift of any proper
subrule with the same consequent.

$$
\mathrm{LIC}(X \Rightarrow Y) 
= \min_{X' \subset X} \left[ \frac{lift(X \Rightarrow Y)}{lift(X' \Rightarrow Y)} \right]
$$

@arules:Lopez:2014 suggests that rules should satisfy $LIC > 1.05$ to justify
adding an item to the antecedent.

**Range:** $[0, \infty)$ ($>1$ means an increase)


## Max-Confidence {#maxconfidence}

**Reference:** @arules:Tan:2004

Max-confidence is a symmetric, null-invariant version of confidence:

$$
\textrm{maxConf}(X \Rightarrow Y) 
= \max\{ conf(X \Rightarrow Y),\ conf(Y \Rightarrow X) \}
= \max\{ P(Y | X),\ P(X | Y) \}
$$

**Range:** $[0, 1]$

## Mutual Information {#mutualinformation}

**Alias:** Uncertainty

**Reference:** @arules:Tan:2004

[Mutual information](https://en.wikipedia.org/wiki/Mutual_information)
measures the information obtained about $Y$ by observing $X$.

$$
\begin{aligned}
M(X \Rightarrow Y)
&= \frac{\displaystyle\sum_{i \in \{X, \overline{X}\}}
\sum_{j \in \{Y, \overline{Y}\}} \frac{n_{ij}}{n}
\log \frac{n\,n_{ij}}{n_i n_j}}
{\displaystyle\min\left(-\sum_{i \in \{X, \overline{X}\}} \frac{n_i}{n}
\log \frac{n_i}{n},
-\sum_{j \in \{Y, \overline{Y}\}} \frac{n_j}{n}
\log \frac{n_j}{n}\right)} \\
&= \frac{\displaystyle\sum_{i \in \{X, \overline{X}\}}
\sum_{j \in \{Y, \overline{Y}\}} P(i \cap j)
\log \frac{P(i \cap j)}{P(i)P(j)}}
{\displaystyle\min\left(-\sum_{i \in \{X, \overline{X}\}}P(i)\log P(i),
-\sum_{j \in \{Y, \overline{Y}\}}P(j)\log P(j)\right)}
\end{aligned}
$$

**Range:** $[0, 1]$ (0 means that $X$ provides no information about $Y$)

## Odds Ratio {#oddsratio}

**Reference:** @arules:Tan:2004

For the analysis of $2 \times 2$ contingency tables, the [odds
ratio](https://en.wikipedia.org/wiki/Odds_ratio) is a measure of the
relationship between two binary variables. It is defined as the ratio of
the odds of a transaction containing $Y$ in the groups of transactions
that contain and do not contain $X$.

$$
\mathrm{OR}(X \Rightarrow Y) 
= \frac{\frac{P(Y | X)}{1 - P(Y | X)}}{\frac{P(Y | \overline{X})}{1 - P(Y | \overline{X})}} 
= \frac{\frac{conf(X \Rightarrow Y)}{1 - conf(X \Rightarrow Y)}}{\frac{conf(\overline{X} \Rightarrow Y)}{1 - conf(\overline{X} \Rightarrow Y)}} 
= \frac{n_{XY} n_{\overline{X}\overline{Y}}}
{n_{X\overline{Y}} n_{\overline{X}Y}}
$$

A confidence interval around the odds ratio can be calculated
[@arules:Li:2014] using a normal approximation. $$
\omega = z_{1-\alpha/2} \sqrt{\frac{1}{n_{XY}} + \frac{1}{n_{X\overline{Y}}} + \frac{1}{n_{\overline{X}Y}} + \frac{1}{n_{\overline{X}\overline{Y}}}}
$$

$$
\mathrm{CI}(X \Rightarrow Y) = [OR(X \Rightarrow Y) \exp(-\omega), OR(X \Rightarrow Y) \exp(\omega)] 
$$

where $z_{1-\alpha/2}$ is the standard normal critical value for a
confidence level of $1-\alpha$.

**Range:** $[0, \infty)$ (1 indicates that $Y$ is not associated with $X$)

## Phi Correlation Coefficient {#phi}

**Reference:** @arules:Tan:2004

The [Phi correlation
coefficient](https://en.wikipedia.org/wiki/Phi_coefficient) between the
transactions containing $X$ and $Y$, represented as two binary vectors.
Phi correlation is equivalent to Pearson's product-moment correlation
coefficient $\rho$ for 0--1 values and is related to the [chi-squared
test statistic](#chisquared) for $2 \times 2$ contingency tables.

$$
\phi(X \Rightarrow Y) 
= \frac{n n_{XY} - n_Xn_Y}{\sqrt{n_X n_Y n_{\overline{X}} n_{\overline{Y}}}}
= \frac{P(X \cap Y) - P(X)P(Y)}{\sqrt{P(X) (1 - P(X)) P(Y) (1 - P(Y))}}
$$

In machine learning, phi correlation is also known as the Matthews
correlation coefficient (MCC). The magnitude of the correlation is also
related to the chi-squared statistic:

$$
|\phi(X \Rightarrow Y)| = \sqrt{\frac{\chi^2}{n}}
$$

**Range:** $[-1, 1]$ (0 when $X$ and $Y$ are independent)

## Ralambondrainy {#ralambondrainy}

**Reference:** @arules:Diatta:2007

This measure is the support of the counterexamples.

$$ 
\mathrm{ralambondrainy}(X \Rightarrow Y) 
= \frac{n_{X\overline{Y}}}{n}
= supp(X \cup \overline{Y})
= P(X \cap \overline{Y})
$$

**Range:** $[0, 1]$ (smaller is better)

## Relative Linkage Disequilibrium {#rld}

**Reference:** @arules:Kennett:2008

RLD is an association measure motivated by indices used in population
genetics. It evaluates the deviation of the rule's support from the
support expected under independence, given the supports of $X$ and $Y$.

$$D = \frac{n_{XY} n_{\overline{X}\overline{Y}} - n_{X\overline{Y}} n_{\overline{X}Y}}{n}$$

$$\mathrm{RLD} = \begin{cases} 
 D / (D + \min(n_{X\overline{Y}}, n_{\overline{X}Y})) & \text{if } D>0, \\
 D / (D - \min(n_{XY}, n_{\overline{X}\overline{Y}})) & \text{otherwise.}
\end{cases}$$

**Range:** $[0, 1]$

## Relative Risk {#relativerisk}

**Reference:** @arules:Sistrom:2004

For the analysis of $2 \times 2$ contingency tables, relative risk is a
measure of the relationship between two binary variables. It is the
ratio of the proportions of transactions containing $Y$ in the two
groups of transactions that contain and do not contain $X$. In
epidemiology,
this corresponds to the ratio of the risk of having disease $Y$ in the
exposed ($X$) and unexposed ($\overline{X}$) groups.

$$
\mathrm{RR}(X \Rightarrow Y)
= \frac{n_{XY} / n_X}{n_{\overline{X}Y} / n_{\overline{X}}} 
= \frac{P(Y | X)}{P(Y | \overline{X})} 
= \frac{conf(X \Rightarrow Y)}{conf(\overline{X} \Rightarrow Y)} 
$$

**Range:** $[0, \infty)$ ($RR = 1$ means $X$ and $Y$ are unrelated)

## Rule Power Factor {#rulepowerfactor}

**Reference:** @arules:Ochin:2008

The rule power factor weights a rule's confidence by its support and
therefore favors rules that have both high confidence and high support.

It is defined as

$$
\mathrm{rpf}(X \Rightarrow Y)
= supp(X \Rightarrow Y)\ conf(X \Rightarrow Y)
= \frac{P(X \cap Y)^2}{P(X)}
$$

**Range:** $[0, 1]$

## Right-Hand-Side Support {#rhssupport}

**Alias:** RHS support, consequent support

Support of the right-hand side of the rule.

$$
\mathrm{RHSsupp}(X \Rightarrow Y) 
= supp(Y)
= P(Y)
$$

**Range:** $[0, 1]$

## Sebag-Schoenauer {#sebag}

**Reference:** @arules:Sebag:1988

It is defined as

$$
\mathrm{sebag}(X \Rightarrow Y)
= \frac{conf(X \Rightarrow Y)}{conf(X \Rightarrow \overline{Y})}
= \frac{P(Y | X)}{P(\overline{Y} | X)}
= \frac{supp(X \cup Y)}{supp(X \cup \overline{Y})}
= \frac{P(X \cap Y)}{P(X \cap \overline{Y})}
$$

**Range:** $[0, \infty)$

## Standardized Lift {#stdlift}

**Reference:** @arules:McNicholas:2008

Standardized lift uses the minimum and maximum values that lift can
attain for each rule to map lift to the interval from 0 to 1. Its lower
bound is

$$
\lambda = \frac{\max\{P(X) + P(Y) - 1, 1/n\}}{P(X)P(Y)}.
$$

Its upper bound is

$$
\upsilon = \frac{1}{\max\{P(X), P(Y)\}}
$$

The standardized lift is defined as

$$
\mathrm{stdLift}(X \Rightarrow Y) 
= \frac{\mathrm{lift}(X \Rightarrow Y) - \lambda}{ \upsilon - \lambda}.
$$

The standardized lift measure can account for the minimum support $s$
and minimum confidence $c$ used in rule mining by replacing the lower
bound $\lambda$ with

$$
\lambda^* 
= \max\left\{\lambda, \frac{4s}{(1+s)^2}, \frac{s}{P(X)P(Y)}, \frac{c}{P(Y)}\right\}.
$$

**Range:** $[0, 1]$

## Varying Rates Liaison {#varyingliaison}

**Reference:** @arules:Bernard:1996

Varying rates liaison is defined as the [lift](#lift) of a rule minus 1;
therefore, 0 represents independence.

$$
\mathrm{VRL}(X \Rightarrow Y) = lift(X \Rightarrow Y) -1
$$

**Range:** $[-1, \infty)$ (0 for independence)

## Yule's Q {#yuleq}

**Reference:** @arules:Tan:2004

Yule's Q, also called Yule's coefficient of association, is a special case
of the [Goodman and Kruskal's
gamma](https://en.wikipedia.org/wiki/Goodman_and_Kruskal%27s_gamma). It
is defined as $$ Q(X \Rightarrow Y) = \frac{\alpha-1}{\alpha+1} $$

where $\alpha = OR(X \Rightarrow Y)$ is the [odds ratio](#oddsratio) of
the rule.

**Range:** $[-1, 1]$

## Yule's Y {#yuley}

**Reference:** @arules:Tan:2004

Yule's Y is also known as the [coefficient of
colligation](https://en.wikipedia.org/wiki/Coefficient_of_colligation)
to measure the association between two binary variables. It is defined
as $$ Y(X \Rightarrow Y) = \frac{\sqrt{\alpha}-1}{\sqrt{\alpha}+1} $$

where $\alpha = OR(X \Rightarrow Y)$ is the [odds ratio](#oddsratio) of
the rule.

**Range:** $[-1, 1]$

# References
