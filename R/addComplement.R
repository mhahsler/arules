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

#' @include itemMatrix.R transactions.R associations.R rules.R itemsets.R tidLists.R

#' @rdname addComplement
setGeneric("addComplement",
  function(x, labels, complementLabels = NULL) standardGeneric("addComplement"))

#' Add Complement-items to Transactions
#' 
#' Provides the generic function `addComplement()` and a method for
#' [transactions] to add complement items. That is,
#' it adds an artificial item to each transaction which does not contain the
#' original item. Such items are also called negative items (Antonie et al,
#' 2014).
#' 
#' @rdname addComplement
#' @aliases addComplement
#' @family transactions functions
#' 
#' @param x an object of class [transactions].
#' @param labels character strings; item labels for which complements should be
#' created.
#' @param complementLabels character strings; labels for the artificial
#' complement-items. If omitted then the original label is prepended by "!" to
#' form the complement-item label.
#' @return Returns an object of class [transactions] with complement items
#' added.
#' @author Michael Hahsler
#' @references Antonie L., Li J., Zaiane O. (2014) Negative Association Rules.
#' In: Aggarwal C., Han J. (eds) _Frequent Pattern Mining,_ Springer
#' International Publishing, pp. 135-145.
#' \doi{10.1007/978-3-319-07821-2_6}
#' @keywords manip
#' @examples
#' 
#' data("Groceries")
#' 
#' ## add a complement-items for "whole milk" and "other vegetables"
#' g2 <- addComplement(Groceries, c("whole milk", "other vegetables"))
#' g2
#' tail(itemInfo(g2))
#' inspect(head(g2, 3))
#' 
#' ## use a custom label for the complement-item
#' g3 <- addComplement(g2, "coffee", complementLabels = "NO coffee")
#' inspect(head(g2, 3))
#' 
#' ## add complements for all items (this is excessive for this dataset)
#' g4 <- addComplement(Groceries, itemLabels(Groceries))
#' g4
#' 
#' ## add complements for all items with a minimum support of 0.1
#' g5 <- addComplement(Groceries, names(which(itemFrequency(Groceries) >= 0.1)))
#' g5
#' 
setMethod("addComplement", signature(x = "transactions"),
  function(x, labels, complementLabels = NULL) {
    
    ### default names for complements
    if(is.null(complementLabels)) 
      complementLabels <- paste0("!", labels)
    
    ### find labels
    orig <- match(labels, itemLabels(x))
    
    ### add complements (this needs lots of memory for many items)
    add <- !as(x[, orig], "matrix")
    colnames(add) <- complementLabels
    tr <- as(add, "transactions")
    
    ### add variables and levels to original items
    if(is.null(itemInfo(x)$variables)) itemInfo(x)$variables <- itemLabels(x)
    if(is.null(itemInfo(x)$levels)) 
      itemInfo(x)$levels <- factor(rep(NA, ncol(x)), levels = c (TRUE, FALSE))
   
    ### add TRUE/FALSE levels to the items with compliments  
    itemInfo(x)$levels[orig] <- TRUE
    itemInfo(tr)$variables <- labels
    itemInfo(tr)$levels <- factor(rep(FALSE, ncol(tr)), levels = c (TRUE, FALSE))
    
    merge(x, tr)
  })

