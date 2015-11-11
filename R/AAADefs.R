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

## common definitions for arules
.installed <- function(pkg) !is(try(utils::installed.packages()[pkg,],
        silent=TRUE), "try-error")

.types <- function(method = "apriori") {
    targets     <- c("frequent itemsets", "maximally frequent itemsets", 
        "closed frequent itemsets", "rules", "hyperedgesets")
    methods     <- c("apriori", "eclat")
    method      <- match.arg(tolower(method), methods)
    if (method == "eclat") return(targets[1:3])
    else return(targets)
}

.aremtypes <- function() {
    c(  "none",      # no additional evaluation measure
        "diff",      # absolute confidence difference
        "quot",      # difference of conf. quotient to 1
        "aimp",      # abs. diff. of improvement to 1
        "info",      # information difference to prior
        "chi2")      # normalized chi^2 measure
}


.list2object <-  function(from, to) {
    if (!length(from)) return(new(to)) 
    s <- slotNames(to)
    p <- pmatch(names(from), s)
    #if(any(is.na(p))) stop(paste("\nInvalid slot name(s) for class",
    #        to, ":", paste(names(from)[is.na(p)], collapse=" ")))
    if(any(is.na(p))) stop(paste("\nInvalid parameter:",
            paste(names(from)[is.na(p)], collapse=" ")), call.=FALSE)
    names(from) <- s[p]
    do.call("new", c(from, Class = to))
}


## FIXME: this is defined in base and the only way to make it work 
## is to redefine it here
"%in%" <-  function(x, table) match(x, table, nomatch = 0) > 0


## Combine Meta data (used for assoctiations and itemMatrix)
## x, y ... two S4 objects with data.frames as meta data
## name ... name of the slot with the data.frame
## value: new combined data.frame
.combineMeta <- function(x, y, name, ...) {
    mx <- slot(x, name)
    my <- slot(y, name)
    
    ## add empty data.frame if nrows is 0 or corrupt
    if(nrow(mx) != length(x)) mx <- data.frame(matrix(nrow = nrow(x), ncol = 0))
    if(nrow(my) != length(y)) my <- data.frame(matrix(nrow = nrow(y), ncol = 0))
    
    ## make column names conforming (rbind fixes order) 
    cols <- unique(c(colnames(mx), colnames(my)))
    
    ## Note: rbind does not preserve rows is ncol==0!
    if(length(cols) > 0) {
      for(col in cols[!(cols %in% colnames(mx))]) mx[[col]] <- NA
      for(col in cols[!(cols %in% colnames(my))]) my[[col]] <- NA
      rbind(mx, my)
    }else{
      data.frame(matrix(nrow = length(x)+length(y), ncol = 0))
    }
}


###
