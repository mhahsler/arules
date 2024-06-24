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


#' Read and Write PMML
#'
#' This function reads and writes PMML representations (version 4.1) of
#' [associations] ([itemsets] and [rules]). Write delegates to package \pkg{pmml}.
#'
#' @name pmml
#' @family import/export
#'
#' @param x a [rules] or [itemsets] object.
#' @param file name of the PMML file (for `read.PMML()` also a XML root node can be
#' supplied).
#' @author Michael Hahsler
#' @seealso [pmml::pmml()].
#' @references PMML 4.4 - Association Rules.
#' \url{https://dmg.org/pmml/v4-4/AssociationRules.html}
#' @keywords interface
#' @examples
#' ### only run this example if pmml is installed
#' if("pmml" %in% rownames(installed.packages())) {
#' data("Groceries")
#'
#' rules <- apriori(Groceries, parameter = list(support = 0.001))
#' rules <- head(rules, by = "lift")
#' rules
#'
#' ### save rules as PMML
#' write.PMML(rules, file = "rules.xml")
#'
#' ### read rules back
#' rules2 <- read.PMML("rules.xml")
#' rules2
#'
#' ### compare rules
#' inspect(rules[1])
#' inspect(rules2[1])
#'
#' ### clean up
#' unlink("rules.xml")
#' }
NULL

#' @rdname pmml
write.PMML <- function(x, file) {
  check_installed("pmml")
  
  ### FIXME: Otherwise pmml does not find XML
  #require("pmml")
  
  XML::saveXML(pmml::pmml(x), file = file)
}

#' @rdname pmml
read.PMML <- function(file) {
  check_installed("XML")
  
  doc <- XML::xmlRoot(XML::xmlTreeParse(file))
  
  ## check model type
  if (is.element("AssociationModel", names(doc)))
    return(.read.PMML.arules(doc))
  
  stop("File does not contain an AssociationModel.")
}

.read.PMML.arules <- function(doc) {
  ## extract model, items, itemsets (match item ids)
  
  model <- doc[["AssociationModel"]]
  
  items <- t(sapply(model[names(model) == "Item"], XML::xmlAttrs))
  
  itemsets <- lapply(
    model[names(model) == "Itemset"],
    FUN = function(x)
      sapply(XML::xmlChildren(x), XML::xmlAttrs)
  )
  itemsets <-
    lapply(
      itemsets,
      FUN = function(x)
        match(x, items[, "id"])
    )
  
  ## create arules itemsets object
  
  im <- encode(itemsets, itemLabels = items[, "value"])
  
  ## itemsets (= no rules)
  
  if (!is.element("AssociationRule", names(model))) {
    ## get support and info
    ## FIXME: maybe remove "id" and "numberofItems"
    
    quality <- t(sapply(
      model[names(model) == "Itemset"],
      FUN = function(x)
        XML::xmlAttrs(x)
    ))
    mode(quality) <- "numeric"
    rownames(quality) <- NULL
    quality <- as.data.frame(quality)
    
    info <- XML::xmlAttrs(doc[["AssociationModel"]])
    info <-
      list(
        ntransactions = as.integer(info["numberOfTransactions"]),
        support = as.numeric(info["minimumSupport"]),
        confidence = as.numeric(info["minimumConfidence"])
      )
    
    ## create itemsets
    
    is <- new("itemsets",
      items = im,
      quality = quality,
      info = info)
    
    return(is)
  } else{
    ## rules
    rules <- t(sapply(model[names(model) == "AssociationRule"],
      XML::xmlAttrs))
    
    quality <-
      rules[, !colnames(rules) %in% c("antecedent", "consequent")]
    mode(quality) <- "numeric"
    rownames(quality) <- NULL
    quality <- as.data.frame(quality)
    
    lhs <- rules[, "antecedent"]
    rhs <- rules[, "consequent"]
    
    ## match with itemset ids!
    itemsetIDs <- sapply(
      model[names(model) == "Itemset"],
      FUN = function(x)
        XML::xmlAttrs(x)["id"]
    )
    
    lhs <- im[match(lhs, itemsetIDs), ]
    rhs <- im[match(rhs, itemsetIDs), ]
    
    info <- XML::xmlAttrs(doc[["AssociationModel"]])
    info <-
      list(
        ntransactions = as.integer(info["numberOfTransactions"]),
        support = as.numeric(info["minimumSupport"]),
        confidence = as.numeric(info["minimumConfidence"])
      )
    
    ## create rules
    rules <- new(
      "rules",
      lhs = lhs,
      rhs = rhs,
      quality = quality,
      info = info
    )
    
    return(rules)
  }
}
