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
## Function apriori
##
## Call the APRIORI algorithm


apriori <-  function(data, parameter = NULL, appearance = NULL, control = NULL)
  {
    ## prepare data
    data <- as(data, "transactions")
    items <- data@data
    
    if (is(appearance, "list")) appearance <- 
      as(c(appearance, list(labels = itemLabels(data))), "APappearance")
    appearance <- as(appearance, "APappearance")   
    
    control <- as(control, "APcontrol")
    parameter <- as(parameter, "APparameter")

    if(control@verbose) {
      ## print parameter
      cat("\nParameter specification:\n")
      print(parameter)
      cat("\nAlgorithmic control:\n")
      print(control)
      cat("\n")
    }
    
    ## sanity check for support (abs. support >1)
    abs_supp <- as.integer(parameter@support * length(data))
    if(abs_supp < 2) warning(sprintf("You chose a very low absolute support count of %d. You might run out of memory! Increase minimum support.\n", abs_supp),
        immediate.=TRUE)

    ## call apriori
    result <- .Call("rapriori", 
        ## transactions
        items@p,
        items@i,
        items@Dim,
        ## parameter
        parameter, control,
        appearance,
        data@itemInfo,
        PACKAGE = "arules")                  

    ## add some reflectance
    call <- match.call()
    result@info <- list(data = call$data,
        ntransactions = length(data),
        support = parameter@support,
        confidence = parameter@confidence    
    )    

    if (is(result, "rules"))  { 
      ## validate sparse Matrix (this takes care of sorting vector i)
      validObject(result@lhs@data)
      validObject(result@rhs@data)
    } else {
      ## validate sparse Matrix (this takes care of sorting vector i)
      validObject(result@items@data)     
    }   
    result
  }

