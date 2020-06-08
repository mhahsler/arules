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



##*******************************************************
## Class APappearance
##
## parameters for item appearance in APRIORI


setAs("NULL", "APappearance",
    function(from, to) { new(to) })

setAs("list", "APappearance", 
    function(from, to) {
        if (!length(from)) return(new("APappearance"))

        if (is.null(from$labels)) 
        stop("labels are missing")

        args <- c("lhs", "rhs", "both", "none", "items")
        other <- c("default", "labels")
        if(!all(names(from) %in% c(args, other))) 
        stop(paste(names(from)[!names(from) %in% c(args, other)], 
                "is an unknown appearance indicator, use:", 
                paste(args, collapse=" "), collapse=", "))
       
        ## cannot set items and lhs, rhs or both
        if(!is.null(from$items) && 
            (!is.null(from$lhs) || !is.null(from$rhs) || !is.null(from$both))) {
          stop("Cannot set appearance for mining association rules (lhs, rhs, both) and frequent itemset mining (items) at the same time!")
        }
         
        ## guess default
        if (is.null(from$default)) {
          if(is.null(from$lhs) 
            && is.null(from$rhs)) from$default <- "both"
          if(!is.null(from$lhs) 
            && is.null(from$rhs)) from$default <- "rhs"
          if(is.null(from$lhs) 
            && !is.null(from$rhs)) from$default <- "lhs"
          
          if(!is.null(from$rhs)
            && !is.null(from$lhs)) from$default <- "none"
          
          if(!is.null(from$both)) from$default <- "none"
          
          ## for itemsets
          if(!is.null(from$items)) from$default <- "none"
        }
          
        set <- c()
        items <- c()
        for (i in seq_len(length(args))) {
            indicator <- from[[args[i]]]
            if(is.null(indicator)) add_items <- NULL
            else if(!all(indicator %in% from$labels))
            stop(paste(indicator[!indicator %in% from$labels], 
                    "is an unknown item label", collapse=", "))
            else add_items <- unlist(sapply(indicator,
                    function(x) { which(from$labels == x) - 1 }))
            items <- c(items, add_items)
            set <- c(set, length(add_items))
        }

        ## check for items in multiple positions (crashes C code!)
        if(any(dup <- duplicated(items))) 
          stop("The following items cannot be specified in multiple appearance locations: ",
          paste(from$labels[items[dup]+1L], collapse = ", "))
        
        ## check NA's
        return(new("APappearance", default = from$default, 
                items = as.integer(items),
                set = set, labels = from$labels))
    }) 



