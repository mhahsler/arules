#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
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



##*******************************************************
## Function ruleInduction
##
## Provides an interface to various functions which induce
## (strong) rules from (1) arbitrary itemsets and (2) from
## a complete set of frequent itemsets. 

setMethod("ruleInduction",  signature(x = "itemsets"),
    function(x, transactions, confidence = 0.8, control = NULL) {

        ## control args are: method, reduce and verbose
        verbose <- if (is.null(control$v)) FALSE   else control$v

        method  <- if (is.null(control$m)) "ptree" else control$m
        reduce  <- if (is.null(control$r)) FALSE   else control$r

        ## FIXME: tidlists does not work correctly and is disabled for now
        ##methods <- c("apriori", "ptree", "tidlists")
        methods <- c("apriori", "ptree")
    
        method <-  methods[pmatch(method , methods)]
        if(is.na(method)) stop("unknown method specify one of: ", 
          paste(methods, collapse = ", "))

        ## induce directly from frequent itemsets
        if (missing(transactions)) {
          if(method != "ptree") 
            warning("Can only use ptree without specified transactions!")  
          if(verbose) cat("ruleInduction: using method", method, "\n")
          return(ruleInduction.index(x, confidence, verbose))
        }
        
        if(verbose) cat("ruleInduction: using method", method, "\n")
          
        pt1 <- proc.time()
        ## check data
        nItems <- nitems(transactions)
        if (nItems != nitems(items(x)))
        stop("Dimensions of x and transactions do not match!")
        if (any(itemLabels(x) != itemLabels(transactions)))
          stop("Item labels for x and transactions do not match!")
        
        if(reduce && method != "ptree") {
            ifreq <- itemFrequency(items(x), type = "abs") 
            items.involved <- which(ifreq > 0)

            if (verbose) cat("reducing data from", nitems(items(x)), 
                "items to", length(items.involved) ,"items\n")
            x.sub <- x
            x.sub@items <- x@items[, items.involved]
            transactions.sub <- transactions[, items.involved]    

        }else{
            x.sub <- x
            transactions.sub <- transactions
        }
        
        pt2 <- proc.time()
        if(verbose) cat("preprocessing done [", pt2[1]-pt1[1], "s].\n", 
            sep = "")
        
        ## find rules
        rules <- 
        if(method == "ptree") ruleInduction.ptree(x, transactions, 
            confidence, reduce, verbose)
        else if(method == "tidlists") ruleInduction.tidlists(x.sub, 
            transactions.sub, confidence, verbose)
        else ruleInduction.apriori(x.sub, transactions.sub, 
            confidence, verbose)
        
        pt3 <-  proc.time()
        if(verbose) cat("searching done [", pt3[1]-pt2[1], "s].\n", sep = "")


        if(reduce && method != "ptree"){
            ## expand items back to full space
            ## -1 since indices in ngCMatix start with 0 
            items.index <- items.involved - 1L

            ## fix dim
            rules@lhs@data@Dim[1] <- nItems
            rules@rhs@data@Dim[1] <- nItems

            ## fix column indices
            ## +1 since indices in ngCMatix start with 0
            rules@lhs@data@i <- items.index[(rules@lhs@data@i + 1L)]
            rules@rhs@data@i <- items.index[(rules@rhs@data@i + 1L)]

            ## fix item labels
            rules@lhs@itemInfo <- itemInfo(transactions)
            rules@rhs@itemInfo <- itemInfo(transactions)
        }

        info <- x@info
        if (is.null(info$data))
            info <- c(x = match.call()$x, info)
        ## apriori
        if (is.null(info$confidence))
            info <- c(info, confidence = confidence)
        else
            info$confidence <- confidence
        rules@info <- info
        
        pt4 <-  proc.time()
        if(verbose) cat("postprocessing done [", pt4[1]-pt3[1], "s].\n", 
            sep = "")

        ## return found rules
        rules 

    })


ruleInduction.apriori <- function(x, transactions, confidence = 0.8,
        verbose = FALSE) {

        empty_rules <- function(trans) {
          em <- as(trans, "itemMatrix")[0]
          new("rules", lhs=em, rhs=em, 
            quality=data.frame(
              support = numeric(0),
              confidence = numeric(0),
              lift = numeric(0)
            ))
        }
  
        if(length(transactions) < 1) return(empty_rules(transactions))
  
        ## itemset sizes
        isetsSize <-  size(x)

        ## find minimal support and mine all rules
        ## Note: minSupport is reduced by epsilon so we get the rules
        ##	with support == min support in x
        minSupport <-  min(quality(x)$support) - 1/length(transactions)

        rules <- apriori(transactions,  parameter = list(
                support = minSupport,
                confidence = confidence, 
                target = "rules", 
                minlen = min(isetsSize), 
                maxlen = max(isetsSize)),
            control = list(verbose = verbose))

        ## find rules which were generated by the itemsets
        if (verbose) cat(paste("starting to filter", length(rules), "rules.\n"))
        take <- !is.na(match(items(rules), items(x)))
        if (verbose) cat("filtering done.\n")

        rules <- rules[take]
        if (verbose) cat("left with", length(rules), "rules.\n")

        ## return found rules
        rules 
    }

ruleInduction.tidlists <- function(x, transactions, confidence = 0.8, verbose = FALSE) {
    tid <- as(transactions, "tidLists")
    data <- .Call("R_tid_rules" ,tid@data, x@items@data, PACKAGE="arules")
    names(data) <- c("support", "confidence",
        "lhs_i", "lhs_p", "rhs_i", "rhs_p", "Dim")

    quality <- data.frame(support = data$support, confidence = data$confidence)

    lhs <- new("ngCMatrix", i = data$lhs_i, p = data$lhs_p, Dim = data$Dim)
    rhs <- new("ngCMatrix", i = data$rhs_i, p = data$rhs_p, Dim = data$Dim)

    lhs <- new("itemMatrix", data = lhs, itemInfo = x@items@itemInfo)
    rhs <- new("itemMatrix", data = rhs, itemInfo = x@items@itemInfo)

    rules <- new("rules", lhs = lhs, rhs = rhs, quality = quality)
    
    rules <- rules[quality(rules)$confidence > confidence]
    rules
}

## ptree support counting

ruleInduction.ptree <- 
function(x, transactions, confidence = 0.8, reduce = FALSE, verbose = FALSE) {
    r <- .Call("R_pncount", x@items@data, transactions@data, FALSE, 
	    reduce, verbose, PACKAGE="arules")
    
    names(r) <- c("data.lhs","data.rhs","support","confidence","lift", "itemset")
    
    ## quality: set NAs to 0 since they are the result of items missing 
    ## in transactions
    q <- as.data.frame(r[3:6])
    q[is.na(q)] <- 0
   
    take <- q$confidence >= confidence
     
    new("rules",
        lhs     = new("itemMatrix", data     = r$data.lhs, 
                                    itemInfo = transactions@itemInfo)[take, ],
        rhs     = new("itemMatrix", data     = r$data.rhs, 
                                    itemInfo = transactions@itemInfo)[take, ],
        quality = q[take, ])
}

## ptree indexing

ruleInduction.index <-
function(x, confidence = 0.8, verbose = FALSE) {
    if (is.null(quality(x)$support))
        stop("cannot induce rules because support is missing! Specify transactions.")

    r <- data.frame(.Call("R_pnrindex", x@items@data, verbose, PACKAGE="arules"))
    names(r) <- c("i", "li", "ri")

    if (!all(r$li) || !all(r$ri))
        stop("cannot induce rules because itemsets are incomplete! Specify transactions.")

    r$support    <- x@quality$support[r$i]
    r$confidence <- r$support /
                    x@quality$support[r$li]
    # filter
    r <- r[r$confidence >= confidence,]
    if (dim(r)[1] == 0)
        return(new("rules"))
    r$lift       <- r$confidence / x@quality$support[r$ri]

    new("rules", lhs     = x@items[r$li],
                 rhs     = x@items[r$ri],
                 quality = r[4:6])
}

###
