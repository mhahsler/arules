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


## create generics

# is now a generic in R 2.11.0
#setGeneric("%in%",
#    function(x, table) standardGeneric("%in%"))

setGeneric("%pin%",
    function(x, table) standardGeneric("%pin%"))

setGeneric("%ain%",
    function(x, table) standardGeneric("%ain%"))

setGeneric("LIST",
    function(from, ...) standardGeneric("LIST"))

setGeneric("write",
    function(x, file = "", ...) standardGeneric("write"))

setGeneric("addComplement",
    function(x, labels, complementLabels=NULL) standardGeneric("addComplement"))

#setGeneric("aggregate",
#    function(x, ...) standardGeneric("aggregate"))

setGeneric("coverage",
    function(x, transactions = NULL, reuse = TRUE) standardGeneric("coverage"))

setGeneric("crossTable", function(x, ...) standardGeneric("crossTable"))

setGeneric("decode",
    function(x, ...) standardGeneric("decode"))

#setGeneric("duplicated",
#    function(x, incomparables = FALSE, ...) standardGeneric("duplicated"))

setGeneric("encode",
    function(x, ...) standardGeneric("encode"))

setGeneric("generatingItemsets",
    function(x) standardGeneric("generatingItemsets"))

setGeneric("info",
    function(x) standardGeneric("info"))

setGeneric("info<-",
    function(x, value) standardGeneric("info<-"))

setGeneric("inspect",
    function(x, ...) standardGeneric("inspect"))

#setGeneric("intersect",
#    function(x, y) standardGeneric("intersect"))

setGeneric("itemInfo",
    function(object) standardGeneric("itemInfo"))

setGeneric("itemInfo<-",
    function(object, value) standardGeneric("itemInfo<-"))

setGeneric("itemsetInfo",
    function(object) standardGeneric("itemsetInfo"))

setGeneric("itemsetInfo<-",
    function(object, value) standardGeneric("itemsetInfo<-"))

setGeneric("itemLabels",
    function(object, ...) standardGeneric("itemLabels"))

setGeneric("itemLabels<-",
    function(object, value) standardGeneric("itemLabels<-"))

setGeneric("items",
    function(x) standardGeneric("items"))

setGeneric("items<-",
    function(x, value) standardGeneric("items<-"))

setGeneric("itemFrequency",
    function(x, ...) standardGeneric("itemFrequency"))

setGeneric("itemFrequencyPlot",
    function(x, ...) standardGeneric("itemFrequencyPlot"))

#setGeneric("is.element",
#    function(el, set) standardGeneric("is.element"))

setGeneric("is.superset",
    function(x, y = NULL, proper = FALSE, sparse = FALSE, ...) standardGeneric("is.superset"))

setGeneric("is.subset",
    function(x, y = NULL, proper = FALSE, sparse = FALSE, ...) standardGeneric("is.subset"))

setGeneric("is.significant",
    function(x, transactions, alpha = 0.01, adjust = "bonferroni") standardGeneric("is.significant"))

setGeneric("is.redundant",
    function(x, ...) standardGeneric("is.redundant"))

#setGeneric("labels",
#    function(object, ...) standardGeneric("labels"))

setGeneric("lhs",
    function(x) standardGeneric("lhs"))

setGeneric("lhs<-",
    function(x, value) standardGeneric("lhs<-"))

setGeneric("interestMeasure",
    function(x,  measure, transactions = NULL, reuse = TRUE, ...) 
    standardGeneric("interestMeasure"))

#setGeneric("match",
#    function(x,  table, nomatch = NA_integer_, incomparables = NULL) 
#    standardGeneric("match"))

#setGeneric("merge",
#    function(x,  y, ...) 
#    standardGeneric("merge"))

setGeneric("nitems",
    function(x, ...) standardGeneric("nitems"))

setGeneric("is.closed",
    function(x) standardGeneric("is.closed"))

setGeneric("is.maximal",
    function(x, ...) standardGeneric("is.maximal"))

setGeneric("itemUnion",
  function(x, y) standardGeneric("itemUnion"))

setGeneric("itemSetdiff",
  function(x, y) standardGeneric("itemSetdiff"))

setGeneric("itemIntersect",
  function(x, y) standardGeneric("itemIntersect"))
setGeneric("quality",
    function(x) standardGeneric("quality"))

setGeneric("quality<-",
    function(x, value) standardGeneric("quality<-"))

setGeneric("recode",
    function(x, ...) standardGeneric("recode"))

setGeneric("rhs",
    function(x) standardGeneric("rhs"))

setGeneric("rhs<-",
    function(x, value) standardGeneric("rhs<-"))

setGeneric("ruleInduction",
    function(x, ...) standardGeneric("ruleInduction"))

# is now a generic in R 2.11.0
#setGeneric("sample",
#    function(x, size, replace = FALSE, prob = NULL) standardGeneric("sample"))

# is now a generic in R 2.11.0
#setGeneric("sort",
#    function(x, decreasing = FALSE, ...) standardGeneric("sort"))

#setGeneric("setdiff",
#    function(x, y) standardGeneric("setdiff"))

#setGeneric("setequal",
#    function(x, y) standardGeneric("setequal"))


setGeneric("support",
    function(x, transactions, ...) standardGeneric("support"))

setGeneric("supportingTransactions",
    function(x, transactions, ...) 
    standardGeneric("supportingTransactions"))

setGeneric("size",
    function(x, ...) standardGeneric("size"))

#setGeneric("subset",
#    function(x, ...) standardGeneric("subset"))

#setGeneric("summary",
#   function(object, ...) standardGeneric("summary"))

setGeneric("tidLists", function(x) standardGeneric("tidLists"))

setGeneric("transactionInfo",
    function(x) standardGeneric("transactionInfo"))

setGeneric("transactionInfo<-",
    function(x, value) standardGeneric("transactionInfo<-"))

#setGeneric("union",
#    function(x, y) standardGeneric("union"))

#setGeneric("unique",
#    function(x, incomparables = FALSE, ...) standardGeneric("unique"))

##***************************************************************
## for clustering

setGeneric("affinity",
    function(x) standardGeneric("affinity"))

setGeneric("dissimilarity",
    function(x, y = NULL, method = NULL, args = NULL, ...)
    standardGeneric("dissimilarity"))

#setGeneric("predict",
#    function(object, ...) standardGeneric("predict"))


