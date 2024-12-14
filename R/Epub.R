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


#' The Epub Transactions Data Set
#'
#' The `Epub` data set contains the download history of documents from the
#' electronic publication platform of the Vienna University of Economics and
#' Business Administration.  The data was recorded between Jan 2003 and Dec
#' 2008.
#'
#' @name Epub
#' @docType data
#' @format Object of class [transactions]
#' with `r data(Epub); nrow(Epub)` transactions
#' and `r data(Epub); ncol(Epub)` items.
#' Item labels are document IDs of the form `"doc_11d"`.
#' Session IDs and time stamps for transactions are also provided as transaction information.
#' @author Michael Hahsler
#' @source Provided by Michael Hahsler from the custom information system ePub-WU
#' (which has since been replaced by eprint).
#' @keywords datasets
#' @examples
#' data(Epub)
#' inspect(head(Epub))
NULL
