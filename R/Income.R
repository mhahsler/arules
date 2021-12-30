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


#' The Income Data Set
#' 
#' Survey example data from the book _The Elements of Statistical Learning_.
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
#' @aliases Income income IncomeESL
#' @docType data
#' @format 
#' 
#' The data is provided in two formats:
#' 
#' 1. `Income` is an object of class [transactions] 
#' with `r data(Income); nrow(Income)` transactions 
#' and `r data(Income); ncol(Income)` items. See below for details.
#' 
#' 2. `IncomeESL` is a data frame with 8993 observations on the
#' following 14 variables:
#' 
#' 
#' \describe{ 
#' \item{income}{an ordered factor with
#' levels `[0,10)` < `[10,15)` < `[15,20)` < `[20,25)` <
#' `[25,30)` < `[30,40)` < `[40,50)` < `[50,75)` <
#' `75+`} 
#' \item{sex}{a factor with levels `male` `female`}
#' \item{marital status}{a factor with levels `married`
#' `cohabitation` `divorced` `widowed` `single`}
#' \item{age}{an ordered factor with levels `14-17` < `18-24` <
#' `25-34` < `35-44` < `45-54` < `55-64` < `65+`}
#' \item{education}{an ordered factor with levels `grade <9` <
#' `grades 9-11` < `high school graduate` < \code{college (1-3
#' years)} < `college graduate` < `graduate study`}
#' \item{occupation}{a factor with levels `professional/managerial`
#' `sales` `laborer` `clerical/service` `homemaker`
#' `student` `military` `retired` `unemployed`} 
#' \item{years
#' in bay area}{an ordered factor with levels `<1` < `1-3` <
#' `4-6` < `7-10` < `>10`} 
#' \item{dual incomes}{a factor with
#' levels `not married` `yes` `no`} 
#' \item{number in
#' household}{an ordered factor with levels `1` < `2` < `3` <
#' `4` < `5` < `6` < `7` < `8` < `9+`}
#' \item{number of children}{an ordered factor with levels `0` < `1`
#' < `2` < `3` < `4` < `5` < `6` < `7` < `8`
#' < `9+`} \item{householder status}{a factor with levels `own`
#' `rent` `live with parents/family`} 
#' \item{type of home}{a factor
#' with levels `house` `condominium` `apartment` \code{mobile
#' Home} `other`} 
#' \item{ethnic classification}{a factor with levels
#' `american indian` `asian` `black` `east indian`
#' `hispanic` `pacific islander` `white` `other`}
#' \item{language in home}{a factor with levels `english` `spanish`
#' `other`} }
#' @author Michael Hahsler
#' @source Impact Resources, Inc., Columbus, OH (1987).
#' 
#' Obtained from the web site of the book: Hastie, T., Tibshirani, R. &
#' Friedman, J. (2001) _The Elements of Statistical Learning_.
#' Springer-Verlag.
#' @keywords datasets
#' @examples
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
NULL
