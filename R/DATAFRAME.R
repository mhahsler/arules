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


setMethod("DATAFRAME", signature(from = "rules"),
  function(from, separate = TRUE, ...) {
    if(separate) {
      antes <- labels(lhs(from), ...)
      conseqs <- labels(rhs(from), ...)
      
      data.frame(
        LHS = factor(antes, levels = unique(antes)),
        RHS = factor(conseqs, levels = unique(conseqs)),
        quality(from)
      )
    }else{
      rule <- labels(from, ...)
      
      data.frame(
        rule = factor(rule, levels = unique(rule)),
        quality(from)
      )
    }
  }
)


setMethod("DATAFRAME", signature(from = "itemsets"),
  function(from, ...) {
    is <- labels(from, ...)
    
    data.frame(
      items = factor(is, levels = unique(is)),
      quality(from)
    )
  }
)

setMethod("DATAFRAME", signature(from = "itemMatrix"),
  function(from, ...) {
    is <- labels(from, ...)
    
    df <- data.frame(items = factor(is, levels = unique(is)))
    if(nrow(itemsetInfo(from)) == nrow(df))  df <- cbind(df, itemsetInfo(from))
    
    df
  }
)
    