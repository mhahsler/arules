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


## Find redundant rules
## redundant rules are rules which do not improve confidence over the
## confidence of their proper sub-rules (i.e., have a negative improvement).

setMethod("is.redundant", signature(x = "rules"),
  function(x, measure = "confidence") {
    interestMeasure(x, measure = "improvement", transactions = NULL, reuse = TRUE, 
      quality_measure = measure) < 0
  }
)
