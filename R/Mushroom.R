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


#' The Mushroom Data Set as Transactions
#'
#' The `Mushroom` [transactions] data set includes descriptions of hypothetical samples
#' corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota
#' Family.
#'
#' The transaction set contains information about 8124 mushrooms (transactions).  4208
#' (51.8%) are edible and 3916 (48.2%) are poisonous. The data contains 22
#' nominal features plus the class attribute (edible or not). These features
#' were translated into 114 items.
#'
#' @name Mushroom
#' @aliases Mushroom mushroom
#' @docType data
#' @format Object of class [transactions]
#' with `r data(Mushroom); nrow(Mushroom)` transactions
#' and `r data(Mushroom); ncol(Mushroom)` items.
#' @author Michael Hahsler
#' @references Alfred A. Knopf (1981). Mushroom records drawn from The Audubon
#' Society Field Guide to North American Mushrooms. G. H. Lincoff (Pres.), New
#' York.
#' @source The data set was obtained from the UCI Machine Learning Repository
#' at \url{https://archive.ics.uci.edu/ml/datasets/Mushroom}.
#' @keywords datasets
NULL
