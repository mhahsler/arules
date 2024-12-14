#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
# 			Bettina Gruen and Kurt Hornik
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
#' Becker (Data Mining and Visualization, Silicon Graphics). It was originally
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
#'  * age: cut into levels `Young` (0-25), `Middle-aged` (26-45),
#' `Senior` (46-65) and `Old` (66+)
#'  * hours-per-week: cut into levels `Part-time` (0-25),
#'  `Full-time` (25-40), `Over-time`
#' (40-60) and `Too-much` (60+)
#'  * capital-gain and capital-loss: each cut into levels `None` (0),
#' `Low` (0 < median of the values greater zero < max) and `High`
#' (>=max)
#'
#' @name Adult
#' @aliases adult Adult AdultUCI
#' @docType data
#' @format `Adult` is an object of class [transactions]
#' with `r data(Adult); nrow(Adult)` transactions
#' and `r data(Adult); ncol(Adult)` items. See below for details.
#'
#' The AdultUCI data set contains a data frame with 48842
#' observations on the following 15 variables.
#' \describe{
#'   \item{age}{a numeric
#' vector.} \item{workclass}{a factor with levels `Federal-gov`,
#' `Local-gov`, `Never-worked`, `Private`, `Self-emp-inc`,
#' `Self-emp-not-inc`, `State-gov`, and `Without-pay`.}
#' \item{education}{an ordered factor with levels `Preschool` <
#' `1st-4th` < `5th-6th` < `7th-8th` < `9th` < `10th`
#' < `11th` < `12th` < `HS-grad` < `Prof-school` <
#' `Assoc-acdm` < `Assoc-voc` < `Some-college` <
#' `Bachelors` < `Masters` < `Doctorate`.}
#' \item{education-num}{a numeric vector.}
#' \item{marital-status}{a factor with
#' levels `Divorced`, `Married-AF-spouse`, `Married-civ-spouse`,
#' `Married-spouse-absent`, `Never-married`, `Separated`, and
#' `Widowed`.} \item{occupation}{a factor with levels `Adm-clerical`,
#' `Armed-Forces`, `Craft-repair`, `Exec-managerial`,
#' `Farming-fishing`, `Handlers-cleaners`, `Machine-op-inspct`,
#' `Other-service`, `Priv-house-serv`, `Prof-specialty`,
#' `Protective-serv`, `Sales`, `Tech-support`, and
#' `Transport-moving`.}
#' \item{relationship}{a factor with levels
#' `Husband`, `Not-in-family`, `Other-relative`,
#' `Own-child`, `Unmarried`, and `Wife`.}
#' \item{race}{a factor
#' with levels `Amer-Indian-Eskimo`, `Asian-Pac-Islander`,
#' `Black`, `Other`, and `White`.}
#' \item{sex}{a factor with levels `Female` and `Male`.}
#' \item{capital-gain}{a numeric vector.}
#' \item{capital-loss}{a numeric vector.} \item{fnlwgt}{a numeric vector.}
#' \item{hours-per-week}{a numeric vector.}
#' \item{native-country}{a factor with levels `Cambodia`, `Canada`, `China`,
#' `Columbia`, `Cuba`, `Dominican-Republic`, `Ecuador`,
#' `El-Salvador`, `England`, `France`, `Germany`,
#' `Greece`, `Guatemala`, `Haiti`, `Holand-Netherlands`,
#' `Honduras`, `Hong`, `Hungary`, `India`, `Iran`,
#' `Ireland`, `Italy`, `Jamaica`, `Japan`, `Laos`,
#' `Mexico`, `Nicaragua`, `Outlying-US(Guam-USVI-etc)`,
#' `Peru`, `Philippines`, `Poland`, `Portugal`,
#' `Puerto-Rico`, `Scotland`, `South`, `Taiwan`,
#' `Thailand`, `Trinadad&Tobago`, `United-States`,
#' `Vietnam`, and `Yugoslavia`.}
#' \item{income}{an ordered factor with
#' levels `small` < `large`.} }
#' @author Michael Hahsler
#' @references A. Asuncion & D. J. Newman (2007): UCI Repository of Machine
#' Learning Databases.  Irvine, CA: University of California, Department of
#' Information and Computer Science.
#'
#' The data set was first cited in Kohavi, R. (1996): Scaling Up the Accuracy
#' of Naive-Bayes Classifiers: a Decision-Tree Hybrid.  _Proceedings of
#' the Second International Conference on Knowledge Discovery and Data Mining_.
#' @source \url{https://archive.ics.uci.edu/}
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
#'   labels = c("Young", "Middle-aged", "Senior", "Old")
#' )
#'
#' AdultUCI[["hours-per-week"]] <- ordered(
#'   cut(
#'     AdultUCI[["hours-per-week"]],
#'     c(0, 25, 40, 60, 168)
#'   ),
#'   labels = c("Part-time", "Full-time", "Over-time", "Workaholic")
#' )
#'
#' AdultUCI[["capital-gain"]] <- ordered(cut(
#'   AdultUCI[["capital-gain"]],
#'   c(
#'     -Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]),
#'     Inf
#'   )
#' ), labels = c("None", "Low", "High"))
#'
#' AdultUCI[["capital-loss"]] <- ordered(cut(
#'   AdultUCI[["capital-loss"]],
#'   c(
#'     -Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]),
#'     Inf
#'   )
#' ), labels = c("None", "Low", "High"))
#'
#' ## create transactions
#' Adult <- transactions(AdultUCI)
#' Adult
#'
NULL
