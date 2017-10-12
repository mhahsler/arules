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
      cat("Apriori\n")
      ## print parameter
      cat("\nParameter specification:\n")
      print(parameter)
      cat("\nAlgorithmic control:\n")
      print(control)
    }
   
    ## sanity check for support (abs. support >1)
    abs_supp <- as.integer(parameter@support * length(data))
    if(control@verbose) {
      cat("\nAbsolute minimum support count:", abs_supp,"\n\n")
    }

    ## call apriori
    result <- .Call(R_rapriori, 
        ## transactions
        items@p,
        items@i,
        items@Dim,
        ## parameter
        parameter, control,
        appearance,
        data@itemInfo)                  

    ## add some reflectance
    call <- match.call()
    result@info <- list(
        data = call$data,
        ntransactions = length(data),
        support = parameter@support,
        confidence = parameter@confidence    
    )    

    ## add count to quality
    quality(result)$count <- quality(result)$support*length(data)
    
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

