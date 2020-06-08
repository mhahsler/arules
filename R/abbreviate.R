#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011-2015 Michael Hahsler and Sudheer Chelluboina
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


setMethod("abbreviate", signature(names.arg = "itemMatrix"),
  function(names.arg, minlength = 4, ..., 
    method = "both.sides"){
    
    ## both sides helps with labels of form variable=level 
    itemInfo(names.arg)$labels_orig <- itemInfo(names.arg)$labels
    
    itemInfo(names.arg)$labels<- as.factor(
      abbreviate(itemInfo(names.arg)$labels, minlength, ..., method = method)
    )
    
    names.arg
  })

setMethod("abbreviate", signature(names.arg = "transactions"),
  function(names.arg, minlength = 4, ..., 
    method = "both.sides"){
    abbreviate(as(names.arg, "itemMatrix"), minlength = minlength, 
      ..., method=method)
  })

setMethod("abbreviate", signature(names.arg = "rules"),
  function(names.arg, minlength = 4, ..., 
    method = "both.sides"){
    names.arg@lhs <- abbreviate(lhs(names.arg), minlength = minlength, 
      ..., method = method)
    names.arg@rhs <- abbreviate(rhs(names.arg), minlength = minlength, 
      ..., method = method)
    names.arg   
  })
  
setMethod("abbreviate", signature(names.arg = "itemsets"),
  function(names.arg, minlength = 4, ..., 
    method = "both.sides"){
    names.arg@items <- abbreviate(items(names.arg), minlength = minlength, 
      ..., method = method)
    names.arg
  })

setMethod("abbreviate", signature(names.arg = "tidLists"),
  function(names.arg, minlength = 4, ..., 
    method = "both.sides"){
    abbreviate(as(names.arg, "itemMatrix"), minlength = minlength, 
      ..., method=method)
  })



