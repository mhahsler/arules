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


#' Adult Data Set
#' 
#' The `AdultUCI` data set contains the questionnaire data of the
#' _Adult_ database (originally called the _Census Income_
#' Database) formatted as a data.frame.  The `Adult` data set contains the
#' data already prepared and coerced to [transactions] for
#' use with \pkg{arules}.
#' 
#' The Adult database was extracted from the census bureau database
#' found at \url{https://www.census.gov/} in 1994 by Ronny Kohavi and Barry
#' Becker, Data Mining and Visualization, Silicon Graphics. It was originally
#' used to predict whether income exceeds USD 50K/yr based on census data. We
#' added the attribute `income` with levels `small` and `large`
#' (>50K).
#' 
#' We prepared the data set for association mining as shown in the section
#' Examples. We removed the continuous attribute `fnlwgt` (final weight).
#' We also eliminated `education-num` because it is just a numeric
#' representation of the attribute `education`.  The other 4 continuous
#' attributes we mapped to ordinal attributes as follows: 
#' 
#'  * age: cut into levels \code{Young} (0-25), \code{Middle-aged} (26-45),
#' \code{Senior} (46-65) and \code{Old} (66+)
#'  * hours-per-week: cut into levels \code{Part-time} (0-25), 
#'  \code{Full-time} (25-40), \code{Over-time}
#' (40-60) and \code{Too-much} (60+)
#'  * capital-gain and capital-loss: each cut into levels \code{None} (0),
#' \code{Low} (0 < median of the values greater zero < max) and \code{High}
#' (>=max)
#' 
#' @name Adult
#' @aliases Adult AdultUCI
#' @docType data
#' @format The AdultUCI data set contains a data frame with 48842
#' observations on the following 15 variables.  
#' \describe{ 
#'   \item{age}{a numeric
#' vector.} \item{workclass}{a factor with levels \code{Federal-gov},
#' \code{Local-gov}, \code{Never-worked}, \code{Private}, \code{Self-emp-inc},
#' \code{Self-emp-not-inc}, \code{State-gov}, and \code{Without-pay}.}
#' \item{education}{an ordered factor with levels \code{Preschool} <
#' \code{1st-4th} < \code{5th-6th} < \code{7th-8th} < \code{9th} < \code{10th}
#' < \code{11th} < \code{12th} < \code{HS-grad} < \code{Prof-school} <
#' \code{Assoc-acdm} < \code{Assoc-voc} < \code{Some-college} <
#' \code{Bachelors} < \code{Masters} < \code{Doctorate}.}
#' \item{education-num}{a numeric vector.} 
#' \item{marital-status}{a factor with
#' levels \code{Divorced}, \code{Married-AF-spouse}, \code{Married-civ-spouse},
#' \code{Married-spouse-absent}, \code{Never-married}, \code{Separated}, and
#' \code{Widowed}.} \item{occupation}{a factor with levels \code{Adm-clerical},
#' \code{Armed-Forces}, \code{Craft-repair}, \code{Exec-managerial},
#' \code{Farming-fishing}, \code{Handlers-cleaners}, \code{Machine-op-inspct},
#' \code{Other-service}, \code{Priv-house-serv}, \code{Prof-specialty},
#' \code{Protective-serv}, \code{Sales}, \code{Tech-support}, and
#' \code{Transport-moving}.} 
#' \item{relationship}{a factor with levels
#' \code{Husband}, \code{Not-in-family}, \code{Other-relative},
#' \code{Own-child}, \code{Unmarried}, and \code{Wife}.} 
#' \item{race}{a factor
#' with levels \code{Amer-Indian-Eskimo}, \code{Asian-Pac-Islander},
#' \code{Black}, \code{Other}, and \code{White}.} 
#' \item{sex}{a factor with levels \code{Female} and \code{Male}.} 
#' \item{capital-gain}{a numeric vector.} 
#' \item{capital-loss}{a numeric vector.} \item{fnlwgt}{a numeric vector.} 
#' \item{hours-per-week}{a numeric vector.} 
#' \item{native-country}{a factor with levels \code{Cambodia}, \code{Canada}, \code{China},
#' \code{Columbia}, \code{Cuba}, \code{Dominican-Republic}, \code{Ecuador},
#' \code{El-Salvador}, \code{England}, \code{France}, \code{Germany},
#' \code{Greece}, \code{Guatemala}, \code{Haiti}, \code{Holand-Netherlands},
#' \code{Honduras}, \code{Hong}, \code{Hungary}, \code{India}, \code{Iran},
#' \code{Ireland}, \code{Italy}, \code{Jamaica}, \code{Japan}, \code{Laos},
#' \code{Mexico}, \code{Nicaragua}, \code{Outlying-US(Guam-USVI-etc)},
#' \code{Peru}, \code{Philippines}, \code{Poland}, \code{Portugal},
#' \code{Puerto-Rico}, \code{Scotland}, \code{South}, \code{Taiwan},
#' \code{Thailand}, \code{Trinadad&Tobago}, \code{United-States},
#' \code{Vietnam}, and \code{Yugoslavia}.} 
#' \item{income}{an ordered factor with
#' levels \code{small} < \code{large}.} }
#' @author Michael Hahsler
#' @references A. Asuncion \& D. J. Newman (2007): UCI Repository of Machine
#' Learning Databases.  Irvine, CA: University of California, Department of
#' Information and Computer Science.
#' 
#' The data set was first cited in Kohavi, R. (1996): Scaling Up the Accuracy
#' of Naive-Bayes Classifiers: a Decision-Tree Hybrid.  _Proceedings of
#' the Second International Conference on Knowledge Discovery and Data Mining_.
#' @source \url{https://www.ics.uci.edu/~mlearn/MLRepository.html}
#' @keywords datasets
#' @examples
#' 
#' data("AdultUCI")
#' dim(AdultUCI)
#' AdultUCI[1:2, ]
#' 
#' ## remove attributes
#' AdultUCI[["fnlwgt"]] <- NULL
#' AdultUCI[["education-num"]] <- NULL
#' 
#' ## map metric attributes
#' AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)),
#'   labels = c("Young", "Middle-aged", "Senior", "Old"))
#' 
#' AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]],
#'   c(0,25,40,60,168)),
#'   labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
#' 
#' AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]],
#'   c(-Inf,0,median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]),
#'   Inf)), labels = c("None", "Low", "High"))
#' 
#' AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]],
#'   c(-Inf,0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]),
#'   Inf)), labels = c("None", "Low", "High"))
#' 
#' ## create transactions
#' Adult <- transactions(AdultUCI)
#' Adult
#' 
NULL

#' Epub Data Set
#' 
#' The \code{Epub} data set contains the download history of documents from the
#' electronic publication platform of the Vienna University of Economics and
#' Business Administration.  The data was recorded between Jan 2003 and Dec
#' 2008.
#' 
#' 
#' @name Epub
#' @docType data
#' @format Object of class \code{\linkS4class{transactions}} with 15729
#' transactions and 936 items.  Item labels are document IDs of the from
#' \code{"doc\_11d"}.  Session IDs and time stamps for transactions are also
#' provided.
#' @author Michael Hahsler
#' @source Provided by Michael Hahsler from ePub-WU at
#' \url{https://epub.wu-wien.ac.at}.
#' @keywords datasets
NULL

#' Groceries Data Set
#' 
#' The `Groceries` data set contains 1 month (30 days) of real-world
#' point-of-sale transaction data from a typical local grocery outlet.  The
#' data set contains 9835 transactions and the items are aggregated to 169
#' categories.
#' 
#' If you use this data set in your paper, please refer to the paper in the
#' references section.
#' 
#' 
#' @name Groceries
#' @docType data
#' @format Object of class [transactions].
#' @author Michael Hahsler
#' @references Michael Hahsler, Kurt Hornik, and Thomas Reutterer (2006)
#' Implications of probabilistic data modeling for mining association rules. In
#' M. Spiliopoulou, R. Kruse, C. Borgelt, A. Nuernberger, and W. Gaul, editors,
#' _From Data and Information Analysis to Knowledge Engineering, Studies
#' in Classification, Data Analysis, and Knowledge Organization_, pages
#' 598--605. Springer-Verlag.
#' @source The data set is provided for arules by Michael Hahsler, Kurt Hornik
#' and Thomas Reutterer.
#' @keywords datasets
NULL


#' Income Data Set
#' 
#' The `IncomeESL` data set originates from an example in the book
#' _The Elements of Statistical Learning_ (see Section source).  The
#' data set is an extract from this survey.  It consists of 8993 instances
#' (obtained from the original data set with 9409 instances, by removing those
#' observations with the annual income missing) with 14 demographic attributes.
#' The data set is a good mixture of categorical and continuous variables with
#' a lot of missing data.  This is characteristic of data mining applications.
#' The Income data set contains the data already prepared and coerced to
#' [transactions].
#' 
#' To create transactions for Income, the original data frame
#' in `IncomeESL` is prepared in a similar way as described in _The
#' Elements of Statistical Learning._ We removed cases with missing values and
#' cut each ordinal variable (age, education, income, years in bay area, number
#' in household, and number of children) at its median into two values (see
#' Section examples).
#' 
#' @name Income
#' @aliases Income IncomeESL
#' @docType data
#' @format \code{IncomeESL} is a data frame with 8993 observations on the
#' following 14 variables.  
#' \describe{ 
#' \item{income}{an ordered factor with
#' levels \code{[0,10)} < \code{[10,15)} < \code{[15,20)} < \code{[20,25)} <
#' \code{[25,30)} < \code{[30,40)} < \code{[40,50)} < \code{[50,75)} <
#' \code{75+}} 
#' \item{sex}{a factor with levels \code{male} \code{female}}
#' \item{marital status}{a factor with levels \code{married}
#' \code{cohabitation} \code{divorced} \code{widowed} \code{single}}
#' \item{age}{an ordered factor with levels \code{14-17} < \code{18-24} <
#' \code{25-34} < \code{35-44} < \code{45-54} < \code{55-64} < \code{65+}}
#' \item{education}{an ordered factor with levels \code{grade <9} <
#' \code{grades 9-11} < \code{high school graduate} < \code{college (1-3
#' years)} < \code{college graduate} < \code{graduate study}}
#' \item{occupation}{a factor with levels \code{professional/managerial}
#' \code{sales} \code{laborer} \code{clerical/service} \code{homemaker}
#' \code{student} \code{military} \code{retired} \code{unemployed}} 
#' \item{years
#' in bay area}{an ordered factor with levels \code{<1} < \code{1-3} <
#' \code{4-6} < \code{7-10} < \code{>10}} 
#' \item{dual incomes}{a factor with
#' levels \code{not married} \code{yes} \code{no}} 
#' \item{number in
#' household}{an ordered factor with levels \code{1} < \code{2} < \code{3} <
#' \code{4} < \code{5} < \code{6} < \code{7} < \code{8} < \code{9+}}
#' \item{number of children}{an ordered factor with levels \code{0} < \code{1}
#' < \code{2} < \code{3} < \code{4} < \code{5} < \code{6} < \code{7} < \code{8}
#' < \code{9+}} \item{householder status}{a factor with levels \code{own}
#' \code{rent} \code{live with parents/family}} 
#' \item{type of home}{a factor
#' with levels \code{house} \code{condominium} \code{apartment} \code{mobile
#' Home} \code{other}} 
#' \item{ethnic classification}{a factor with levels
#' \code{american indian} \code{asian} \code{black} \code{east indian}
#' \code{hispanic} \code{pacific islander} \code{white} \code{other}}
#' \item{language in home}{a factor with levels \code{english} \code{spanish}
#' \code{other}} }
#' @author Michael Hahsler
#' @source Impact Resources, Inc., Columbus, OH (1987).
#' 
#' Obtained from the web site of the book: Hastie, T., Tibshirani, R. \&
#' Friedman, J. (2001) _The Elements of Statistical Learning_.
#' Springer-Verlag.
#' @keywords datasets
#' @examples
#' 
#' data("IncomeESL")
#' IncomeESL[1:3, ]
#' 
#' ## remove incomplete cases
#' IncomeESL <- IncomeESL[complete.cases(IncomeESL), ]
#' 
#' ## preparing the data set
#' IncomeESL[["income"]] <- factor((as.numeric(IncomeESL[["income"]]) > 6) +1,
#'   levels = 1 : 2 , labels = c("$0-$40,000", "$40,000+"))
#' 	  
#' IncomeESL[["age"]] <- factor((as.numeric(IncomeESL[["age"]]) > 3) +1,
#'   levels = 1 : 2 , labels = c("14-34", "35+"))
#' 
#' IncomeESL[["education"]] <- factor((as.numeric(IncomeESL[["education"]]) > 4) +1,
#'   levels = 1 : 2 , labels = c("no college graduate", "college graduate"))
#' 
#' IncomeESL[["years in bay area"]] <- factor(
#'   (as.numeric(IncomeESL[["years in bay area"]]) > 4) +1,
#'   levels = 1 : 2 , labels = c("1-9", "10+"))
#' 
#' IncomeESL[["number in household"]] <- factor(
#'   (as.numeric(IncomeESL[["number in household"]]) > 3) +1,
#'   levels = 1 : 2 , labels = c("1", "2+"))
#' 
#' IncomeESL[["number of children"]] <- factor(
#'   (as.numeric(IncomeESL[["number of children"]]) > 1) +0,
#'   levels = 0 : 1 , labels = c("0", "1+"))
#' 	
#' ##  creating transactions
#' Income <- transactions(IncomeESL)
#' Income
#' 
NULL


#' Mushroom Data Set
#' 
#' The `Mushroom` data set includes descriptions of hypothetical samples
#' corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota
#' Family.  It contains information about 8124 mushrooms (transactions).  4208
#' (51.8\%) are edible and 3916 (48.2\%) are poisonous. The data contains 22
#' nominal features plus the class attribute (edible or not). These features
#' were translated into 114 items.
#' 
#' @name Mushroom
#' @docType data
#' @format Object of class \code{transactions}.
#' @author Michael Hahsler
#' @references Alfred A. Knopf (1981). Mushroom records drawn from The Audubon
#' Society Field Guide to North American Mushrooms. G. H. Lincoff (Pres.), New
#' York.
#' @source The data set was obtained from the UCI Machine Learning Repository
#' at \url{https://archive.ics.uci.edu/ml/datasets/Mushroom}.
#' @keywords datasets
NULL


#' The SunBai Data Set
#' 
#' A small example database for weighted association rule mining provided as an
#' object of class [transactions].
#' 
#' The data set contains the example database described in the paper by K. Sun
#' and F.Bai for illustration of the concepts of weighted association rule
#' mining. `weight` stored as transaction information denotes the
#' transaction weights obtained using the HITS algorithm.
#' 
#' @name SunBai
#' @family weighted association mining
#' @aliases SunBai sunbai
#' @docType data
#' @source K. Sun and F. Bai (2008). Mining Weighted Association Rules without
#' Preassigned Weights. _IEEE Transactions on Knowledge and Data
#' Engineering_, 4 (30), 489--495.
#' @keywords datasets
#' @examples
#' 
#' data(SunBai)
#' summary(SunBai)
#' inspect(SunBai)
#' 
#' transactionInfo(SunBai)
#' 
NULL
