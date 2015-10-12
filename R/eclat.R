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
## Function eclat
##
## Call the ECLAT algorithm



eclat <-  function(data, parameter = NULL, control = NULL)
  {
    
    ## prepare data
    data <- as(data, "transactions")
    items <- data@data
    parameter <- as(parameter, "ECparameter")
    control <- as(control, "ECcontrol")
    
    if(control@verbose) {
      ## print parameter
      cat("\nparameter specification:\n")
      print(parameter)
      cat("\nalgorithmic control:\n")
      print(control)
      cat("\n")
    }

    ## sanity check for support (abs. support >1)
    abs_supp <- as.integer(parameter@support * length(data))
    if(abs_supp < 2) warning(sprintf("You chose a very low absolute support count of %d. You might run out of memory! Increase minimum support.\n", abs_supp), 
        immediate.=TRUE)



    ## the C code of eclat dies when no item is frequent so we do this
    if(max(itemFrequency(data)) <= parameter@support) {
      if(control@verbose) cat("eclat - zero frequent items\n")
      return(new("itemsets"))
    }

    ## call eclat
    result <- .Call("reclat", 
        ## transactions
        items@p,
        items@i,
        items@Dim,
        ## parameter
        parameter, control,
        data@itemInfo,
        PACKAGE = "arules")                  
    
    ## validate sparse Matrix (this takes care of sorting vector i)
    validObject(result@items@data)
    
    ## copy itemInfo
    result@items@itemInfo <- data@itemInfo
   
    ## empty itemsetInfo
    result@items@itemsetInfo <- 
      data.frame(matrix(nrow = length(result), ncol = 0))
     
    ## make sure quality is a data.frame
    result@quality <- as.data.frame(result@quality,)
    
    ## add some reflectance 
    call <- match.call()
    result@info <- list(data = call$data,
        ntransactions = length(data),
        support = parameter@support
    ) 
    
    ## make sure tid list itemInfo is ok
    if (!is.null(result@tidLists)) {
      result@tidLists@itemInfo <- data.frame(labels = labels(result))
      result@tidLists@transactionInfo <- data@transactionInfo
    }
    
    result
  }

