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

#' The SunBai Weighted Transactions Data Set
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
#' @format Object of class [transactions] with `r data(SunBai); nrow(SunBai)` transactions and `r data(SunBai); ncol(SunBai)` items. Weights are stored as transaction information.
#' @docType data
#' @source K. Sun and F. Bai (2008). Mining Weighted Association Rules without
#' Preassigned Weights. _IEEE Transactions on Knowledge and Data
#' Engineering_, 4 (30), 489--495.
#' @keywords datasets
#' @examples
#' data(SunBai)
#' summary(SunBai)
#' inspect(SunBai)
#' 
#' transactionInfo(SunBai)
NULL
