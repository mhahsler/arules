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


#######################################################################3
### Support for weighted association rule mining (HITS and weclat)
### ceeboo 2008

hits <- function(data, iter = 16L, tol = NULL,
  type = c("normed","relative","absolute"), verbose = FALSE) {
  
  data <- as(data, "transactions")
  type <- match.arg(type)
  
  r <- .Call(R_hits_ngCMatrix, data@data, iter, tol, verbose, PACKAGE = "arules")
  names(r) <- transactionInfo(data)[["transactionID"]]
  
  switch(type,
    normed   = r / sqrt(sum(r^2)),
    relative = r / sum(r),
    absolute = r
  )
}

##
weclat <- function(data, parameter = NULL, control = NULL) {
  data <- as(data, "transactions")
  
  weight <- transactionInfo(data)[["weight"]]
  if(is.null(weight)) { 
    weight <- rep(1, length(data))
    if (!is.null(control) && control$v) 
      cat("Transactions do not contain weights in transactionInfo. Using a weight of 1 for each.")
  }
  
  weight <- as.numeric(weight) 
  
  if (!is(parameter, "ASparameter"))
    parameter <- do.call("new",c(list("ASparameter"), parameter))
  if (!is(control, "AScontrol"))
    control <- do.call("new", c(list("AScontrol"), control))
  
  ## these are not available
  parameter@target <- NA_character_
  parameter@ext <- NA
  control@sort <- NA_integer_
  
  if (control@verbose) {
    cat("\nparameter specification:\n")
    print(parameter)
    cat("\nalgorithmic control:\n")
    print(control)
    cat("\n")
  }
  ## r <- .Call(R_transpose_ngCMatrix, data@data)
  r <- selectMethod("t", class(data@data))(data@data)
  r <- .Call(R_weclat_ngCMatrix, r, weight,
    parameter@support,
    parameter@minlen,
    parameter@maxlen,
    control@verbose, PACKAGE = "arules")
  names(r) <- c("data", "support")
  validObject(r$data)
  
  quality <- data.frame(support = r$support)
  
  r <- new("itemMatrix", data = r$data, itemInfo = data@itemInfo)
  info <- c(
    data = match.call()$data,
    ntransactions = length(data),
    support = parameter@support
  )
  
  r <- new("itemsets", items    = r,
    quality  = quality,
    info     = info)
  r
}

###
