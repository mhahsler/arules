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


#' @rdname associations-class
#' @name plot
#' @aliases plot.associations plot.itemMatrix
NULL

### produce a better error message for plot of associations
plot.associations <- function(x, ...) stop("Needed package 'arulesViz' not installed or loaded!")

### call image for itemMatrix and transactions 
plot.itemMatrix <- function(x, ...) {
  warning("Use image() instead of plot().")
  image(x, ...)
}
