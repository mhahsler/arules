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

##**********************************************************
## itemMatrix
setClass("itemMatrix",
  representation(
    data        = "ngCMatrix", 
    itemInfo    = "data.frame",
    itemsetInfo = "data.frame"
  ), 
  
  prototype(itemInfo = data.frame(), itemsetInfo = data.frame()),
  
  validity= function(object) {
    ## itemInfo needs a labels column of appropriate length
    ## also the labels must be unique for matching objects.
    if (length(itemInfo(object)[["labels"]]) != nitems(object))
      return("item labels do not match number of columns")
    if (length(unique(itemInfo(object)[["labels"]])) != nitems(object))
      return("item labels not unique")
    
    if (ncol(itemsetInfo(object)) && 
        nrow(itemsetInfo(object)) != length(object))
      return("itemsetInfo does not match number of rows")
    
    TRUE
  }
)

setMethod(initialize, "itemMatrix", function(.Object, ...) { 
  .Object <- callNextMethod()
  
  validObject(.Object)
  .Object
})


setClass("summary.itemMatrix",
  representation(
    Dim           = "integer",
    density       = "numeric",
    itemSummary   = "integer",
    lengths       = "table",
    lengthSummary = "table",
    itemInfo      = "data.frame",
    itemsetInfo      = "data.frame"
  )
)

##**********************************************************
## transactions 

setClass("transactions",
  contains = "itemMatrix",
  
  validity = function(object) {
    ## check dimensions
    ## no transactionInfo (empty data.frame)
    if (length(object@itemsetInfo) &&
        length(object@itemsetInfo[[1]]) != length(object))
      return("transactionInfo does not match number of transactions")
    
    TRUE
  }
)

setMethod(initialize, "transactions", function(.Object, ...) { 
  .Object <- callNextMethod()
  
  validObject(.Object)
  .Object
})


setClass("summary.transactions",
  contains = "summary.itemMatrix"
)


##**********************************************************
## transaction ID lists
##
## FIXME optional item labels is not a good idea!

setClass("tidLists",
  representation(
    data            = "ngCMatrix",
    itemInfo        = "data.frame",
    transactionInfo = "data.frame"
  ),
  
  prototype(itemInfo = data.frame(), transactionInfo = data.frame()),
  
  validity = function(object) { 
    ## check number of labels in itemInfo
    ## no labels (empty data.frame)
    if (length(object@itemInfo) &&
        length(object@itemInfo[["labels"]]) != dim(object)[1])
      return("number of item labels does not match number of rows")
    
    if (length(object@transactionInfo) &&
        length(object@transactionInfo[[1]]) != dim(object)[2])
      return("transactionInfo does not match number of transactions")
    
    TRUE
  }
)

setMethod(initialize, "tidLists", function(.Object, ...) { 
  .Object <- callNextMethod()
  
  validObject(.Object)
  .Object
})


setClassUnion("tidLists_or_NULL", c("tidLists", "NULL"))

setClass("summary.tidLists",
  representation(
    Dim                = "integer",
    transactionSummary = "integer",
    lengths            = "table",
    lengthSummary      = "table",
    itemInfo           = "data.frame"
  )
)


##**********************************************************
## mining parameter

setClass("APappearance",
  representation(
    set     = "integer",
    items   = "integer",
    labels  = "character",
    default = "character"),
  
  prototype(
    set     = rep(0L, 5),
    items   = integer(),
    labels  = "",
    default = "both"),
  
  validity = function(object) {
    if (!object@default %in% c("lhs", "rhs", "none", "both")) 
      return("Default value not specified correctly")
    else if (!sum(object@set) == length(object@items)) 
      return("Slots 'set' and 'items' do not match")
    return(TRUE)
  })


setClass("ASparameter",
  representation(
    support = "numeric",
    minlen  = "integer",
    maxlen  = "integer",
    target  = "character",
    ext     = "logical"),
  
  prototype(
    target  = "frequent itemsets",
    support = 0.1,
    minlen  = 1L,
    maxlen  = 5L,
    ext     = TRUE),
  
  validity = function(object) {
    if (!object@target %in% .types()) 
      return(paste("target =", object@target, "not supported."))
    if (object@support > 1) 
      return(paste("support =", object@support, "> 1"))
    if (object@minlen <= 0)  
      return(paste("minlen =", object@minlen, "<= 0"))
    if (object@minlen > object@maxlen) 
      return(paste("minlen =", object@minlen,
        "> maxlen =", object@maxlen))
    return(TRUE)
  })

setClass("APparameter",
  representation(
    confidence  = "numeric",
    minval      = "numeric",
    smax        = "numeric",
    arem        = "character",
    aval        = "logical",
    originalSupport = "logical",
    maxtime     = "numeric"),
  contains = "ASparameter",
  
  prototype(new("ASparameter"),
    target      = "rules",
    confidence  = 0.8,
    minval      = 0.1,
    smax        = 1.0,
    arem        = "none",
    originalSupport = TRUE,
    aval = FALSE,
    maxtime = 5.0),
  
  validity = function(object) {
    if (!object@arem %in% .aremtypes()) 
      return(paste("arem =", object@arem, "not supported."))
   
    if (pmatch(object@target, .types("apriori")) >4) {
      if (is.na(object@confidence) 
        || object@confidence > 1 
        || object@confidence < 0)  
        return(paste("confidence is not in [0,1]"))
    }else{ 
      if (!is.na(object@confidence))
        return(paste("frequent itemsets do not use confidence (should be NA)"))
    }
    
    
    if (object@smax < 0) 
      return(paste("smax is not < 0"))
    
    return(TRUE)
  })

setClass("ECparameter",
  representation(tidLists = "logical"),
  contains = "ASparameter",
  
  prototype(new("ASparameter"),
    tidLists = FALSE),
  
  validity = function(object) {
    if (object@target %in% .types(method = "eclat")) return(TRUE) 
    else return(paste(object@target, "not supported"))
  })

setClass("AScontrol",
  representation(
    sort    = "integer",
    verbose = "logical"),
  
  prototype(
    verbose = TRUE,
    sort    = 2L),
  
  validity = function(object) {
    if (object@sort > 2 | object@sort < -2) 
      return(paste("sort =", object@sort,"not one of 1: ascending,",
        "-1: descending, 0: do not sort, 2: ascending,",
        "-2: descending w.r.t. transaction size sum"))
    else return(TRUE) 
  })

setClass("APcontrol",
  representation(
    filter  = "numeric",
    tree    = "logical",
    heap    = "logical",
    memopt  = "logical",
    load    = "logical"),
  contains    = "AScontrol",
  
  prototype(new("AScontrol"),
    filter  = 0.1,
    sort    = 2L,
    tree    = TRUE,
    heap    = TRUE,
    memopt  = FALSE,
    load    = TRUE),
  
  validity = function(object) {
    if (object@filter > 1 || object@filter < -1) 
      return(paste("filter =", object@filter, "is not in [-1,1]"))
    else return(TRUE)
  })

setClass("ECcontrol",
  representation(sparse = "numeric"),
  contains = "AScontrol",
  
  prototype(new("AScontrol"),
    sparse  = 7,
    sort    = -2L))



##****************************************************************
## associations

setClass("associations",
  representation(quality = "data.frame", info = "list", "VIRTUAL"))

setMethod(initialize, "associations", function(.Object, ...) {
  .Object <- callNextMethod()
  
  ## fix empty data.frame in itemsetInfo
  if(all(dim(.Object@quality) == 0)) 
    .Object@quality <- data.frame(matrix(ncol = 0, nrow = length(.Object)))
  
  validObject(.Object)
  .Object
})

setClass("itemsets",
  representation(
    items    = "itemMatrix", 
    tidLists = "tidLists_or_NULL"
  ),
  
  contains = "associations",
  
  prototype(tidLists = NULL, quality = data.frame(), info = list()),
  
  validity = function(object) {
    ## if tidLists exists, check dimensions
    ## Note, we cannot check dim(object@tidLists)[2] here since we
    ## don't know the number of transactions in the used data set! 
    if (length(tidLists(object)) && 
        length(tidLists(object)) != length(object))
      return("tidLists does not match number of itemsets")
    
    ## if quality exists, check dimensions
    if (length(quality(object)) && 
        nrow(quality(object)) != length(object))
      return("quality does not match number of itemsets")
    
    TRUE
  }
)

## rules is the lhs
setClass("rules",
  representation(
    lhs = "itemMatrix", 
    rhs = "itemMatrix"
  ),
  contains = "associations",
  
  prototype(quality = data.frame(), info = list()),
  
  validity = function(object) {
    ## check dimensions
    if (!all(dim(object@lhs) == dim(object@rhs))) 
      return("dimensions of lhs and rhs do not match")
    
    ## check quality
    if (length(object@quality) && 
        length(object@quality[[1]]) != length(object@lhs))
      return("quality does not match the number of rules")
    
    ## check lhs and rhs
    if(!validObject(object@lhs)) 
      return("lhs of rules is not a valid itemMatrix")
    if(!validObject(object@rhs)) 
      return("rhs of rules is not a valid itemMatrix")
    
    ## Removed check so rhs can be empty... (MFH) 
    ## check size of rhs
    #if(any(size(object@rhs)<1)) return("rhs is empty for at least one rule")
    
    ## check item labels
    if(!identical(colnames(object@lhs), colnames(object@rhs))) return("item labels in lhs and rhs do not match")
    
    ## check union
    if(any(size(items(object)) != size(object@rhs)+size(object@lhs))) return("lhs and rhs of some rules is not distinct")
    
    TRUE
  }
)

setClass("summary.associations",
  representation(length  ="integer", quality = "table", 
    info = "list" , "VIRTUAL"))

setClass("summary.itemsets",
  representation(tidLists = "logical", items = "summary.itemMatrix"),
  contains = "summary.associations")

setClass("summary.rules",
  representation(lengths = "table", lengthSummary = "table"),
  contains = "summary.associations")


##**********************************************************
## proximities for clustering

## we reuse S3 dist for compatibility reasons
setOldClass("dist")

## similarity
setClass("ar_similarity",
  contains = "matrix",
  representation(method = "character")
)

## distances between x and y
setClass("ar_cross_dissimilarity",
  contains = "matrix",
  representation(method = "character")
)

###
