#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler and Sudheer Chelluboina
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


## FIXME: abbreviate destroys appreviate in base!

## Make abbreviate generic
abbreviate <- function(...) UseMethod("abbreviate")
abbreviate.default <- function(...) base::abbreviate(...)


abbreviate.itemMatrix <- function(data, minlength = 4, ..., 
  method = "both.sides"){
  
  ## both sides helps with labels of form variable=level 
  
  itemInfo(data)$labels_orig <- itemInfo(data)$labels
  
  itemInfo(data)$labels<- as.factor(
    abbreviate(itemInfo(data)$labels, minlength, ..., method = method)
  )
  
  data
}

abbreviate.transactions <- function(data, minlength = 4, ..., 
  method = "both.sides"){
  abbreviate.itemMatrix(data, minlength, ..., method=method)
}

abbreviate.rules <- function(data, minlength = 4, ..., 
  method = "both.sides"){
  data@lhs <- abbreviate(lhs(data), minlength, ..., method)
  data@rhs <- abbreviate(rhs(data), minlength, ..., method)
  data   
}

abbreviate.itemsets <- function(data, minlength = 4, ..., 
  method = "both.sides"){
  data@items = abbreviate(items(data), minlength, ..., method)
  data
}

abbreviate.tidLists <- function(data, minlength = 4, ..., 
  method = "both.sides"){
  abbreviate.itemMatrix(data, minlength, ..., method=method)
}



