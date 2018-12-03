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



##*******************************************************************
## general dissimilarity method

##*******************************************************************
## dissimilarity; currently implemented using dense matrices

## Memory usage of dissimilarity with  "jaccard", "matching" ore "dice"
## (on a 32 bit machine)
##
## input: x and y (integer, 4 byte) 
## intermediate: 4 matrices size nx x ny (double, 8 byte)
## result: either 1 dist nx x ny /2 or full matrix for crossdissim.
##
## total w/o input: about 5 * nx * ny * 8 byte


setMethod("dissimilarity", signature(x = "matrix"),
  function(x, y = NULL, method = NULL, args = NULL) {
    ## Compute dissimilarities on binary data
    
    ## make sure the input is a 0-1 matrix or a logical matrix
    is.zeroone <- function(x) (all(x == 0 | x == 1))
    
    storage.mode(x) <- "numeric"
    if (!is.zeroone(x)) stop("x is not a binary matrix (0-1 or logical)!")
    
    ## cross dissimilarities? Check y
    if (!is.null(y)) {
      if (!is.matrix(y)) stop("'y' not a matrix")
      storage.mode(y) <- "numeric"
      if (!is.zeroone(y)) stop("y is not a binary matrix (0-1 or logical)!") 
      cross <- TRUE
    } else cross <- FALSE
    
    builtin_methods <- c("affinity", "jaccard", "matching", "dice", 
      "cosine", "euclidean", "pearson", "phi")
    
    if(is.null(method)) ind <- 2      # Jaccard is standard
    else if(is.na(ind <- pmatch(tolower(method),
      tolower(builtin_methods))))
      stop(gettextf("Value '%s' is not a valid abbreviation for a similarity method.", method), domain = NA)
    
    method <- builtin_methods[ind]
    
    ## affinity is special!
    if(ind == 1) {
      ## Affinity.
      ## for rules and itemsets we need transactions or affinities
      
      ## given affinities or transactions? Otherwise, calculate!
      if(!is.null(args$aff)) affinities <- args$aff
      else if(!is.null(args$trans)) affinities <- affinity(args$trans)
      else affinities <- affinity(x)
      
      ## Normalize transaction incidences by transaction length.
      x <- x / pmax(rowSums(x), 1)
      
      if(!cross) {
        dist <- 1 - as.dist(x %*% affinities %*% t(x))
      }else{
        y <- y / pmax(rowSums(y), 1)
        dist <- new("ar_cross_dissimilarity", 
          1 - x %*% affinities %*% t(y))
      }
      
      ## Euclidean is special too
    } else if(ind == 6) {
      if(cross) stop("Euclidean cross-distance not implemented.")
      
      dist <- dist(x, method = "euclidean")
      
      ## Pearson correlation coefficient (Note: cor is calculated 
      ##    between columns and we only use pos. correlation)
      ## Phi is the same as pearson
    } else if(ind == 7 || ind==8) {
      if(!cross) {
        ## warnings for zero variance!
        suppressWarnings(cm <- cor(t(x), method = "pearson"))
        ## pairwise complete is very slow
        #cm <- cor(t(x), method = "pearson",
        #	  use="pairwise.complete.obs")
        cm[cm < 0 | is.na(cm)] <- 0
        dist <- as.dist(1 - cm)
      } else {
        suppressWarnings(cm <- cor(t(x), t(y), method = "pearson")) 
        #cm <- cor(t(x), t(y), method = "pearson", 
        #	use="pairwise.complete.obs")
        cm[cm < 0 | is.na(cm)] <- 0
        dist <- new("ar_cross_dissimilarity", 1 - cm) 
      }
    } else {
      
      if(!cross) y <- x
      
      ## prepare a, b, c, d (response table) for the rest of measures
      ## see: Gower, J. C. and P. Legendre.  1986.  Metric and 
      ## Euclidean properties of dissimilarity coefficients.  
      ## J. Classif. 3: 5 - 48. 
      #a <- x %*% t(y)
      a <- tcrossprod(x, y)
      
      ## save some memory
      #b <- x %*% (1 - t(y))
      #c <- (1 - x) %*% t(y)
      #d <- ncol(x) - a - b - c
      
      # even faster code adapted from Leisch (2005): A toolbox for
      # K-centroids cluster analysis, Preprint.
      nx <- nrow(x) 
      ny <- nrow(y)
      
      c <- matrix(rowSums(x), nrow = nx, ncol = ny) - a
      b <- matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE) - a
      
      #a_b_c <- matrix(rowSums(x), nrow = nx, ncol = ny) +
      #matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE) - a
      
      
      if(ind == 2) {
        ## Jaccard == binary (Sneath, 1957) 
        #dist <- dist(as(x, "matrix"), "binary")
        
        dist <- 1 - a/(a + b + c)
        #dist <- 1 - (a/a_b_c)
        
      } else if(ind == 3){
        ## Matching Coefficient (Sokal and Michener, 1958)
        
        #we need d only here
        #d <- ncol(x) - a_b_c
        d <- ncol(x) - (a+b+c)
        
        dist <- 1 - (a + d) / (a + b + c + d)  
        #dist <- 1 - ((a + d) / (a_b_c + d))  
        
      } else if(ind == 4) {
        ## Dice Coefficient (Dice, 1945)
        dist <- 1 - 2 * a / (2*a + b + c)
        #dist <- 1 - 2 * a / (a + a_b_c)
        
      } else if(ind == 5) {
        ## Cosine
        dist <- 1 - a / sqrt((a+b)*(a+c))
      }
    }
    
    # in case we divided by zero
    dist[is.nan(dist)] <- 0
    
    if(!cross) {
      # return a S3 "dist" object just add "ar_dissimilarity"
      dist <- as.dist(dist)
      attr(dist, "method") <- method
      #class(dist) <- c("ar_dissimilarity", class(dist))
      return(dist)
      
    }else{ 
      # return a S4 "ar_cross_dist" object
      dist <- new("ar_cross_dissimilarity", dist, method = method)
      return(dist)
    }
    
  })


##*******************************************************************
## wrapper for itemMatrix (transactions)
setMethod("dissimilarity", signature(x = "itemMatrix"),
  function(x, y = NULL, method = NULL, args = NULL, 
    which = "transactions") {
    
    ## use items?
    if(is.null(which)) which <- "transactions"
    items <- pmatch(tolower(which), "items", nomatch = 0) == 1 
    
    x <- as(x, "matrix")
    if(items) x <- t(x) 
    
    if(!is.null(y)) { 
      y <- as(y, "matrix")
      if(items) y <- t(y) 
    }
    
    dissimilarity(x = x, y = y, method = method, args = args)
  })

##*******************************************************************
## wrapper for associations
setMethod("dissimilarity", signature(x = "associations"),
  function(x, y = NULL, method = NULL, args = NULL, 
    which = "associations") {
    
    if(is.null(method)) method <- "jaccard"   # Jaccard is standard
    
    builtin_methods <- c("gupta", "toivonen")
    
    if(!is.na(ind <- pmatch(tolower(method),
      tolower(builtin_methods)))) {
      
      method <- builtin_methods[ind]
      if(!is.null(y)) stop("Cross dissimilarities not implemented for this method!")
      trans <- args$trans
      if(is.null(trans)) stop("Transactions needed in args for this method!")
      
      if(ind == 1) {
        ##gupta: use tidlist intersection
        
        ## FIXME: tidlist operations should be made functions
        
        ## calculate tidlist for rules
        tids <- .associationTidLists(x, trans)
        
        m_X <- sapply(tids, length)
        n <- length(x)
        
        m <- matrix(nrow=n,ncol=n)
        for(i in 1:n){
          for(j in i:n) {
            m_XiXj <- length(intersect(tids[[i]], tids[[j]]))
            m[i,j] <- 1-m_XiXj/(m_X[i]+m_X[j]-m_XiXj) 
            m[j,i] <- m[i,j] ## dists are symmetric
          }
        }
        
        dist <- as.dist(m)
        attr(dist, "method") <- method
        return(dist)
        
      }else{
        ## toivonen
        
        ## only one RHS allowed
        if(length(unique(rhs(x)))!=1) stop("Only a single RHS allowed for this method!")
        
        ## calculate tidlist for rules
        tids <- .associationTidLists(x, trans)
        
        m_X <- sapply(tids, length)
        n <- length(x)
        
        m <- matrix(nrow=n,ncol=n)
        for(i in 1:n) {
          for(j in i:n) {
            m_XiXj <- length(intersect(tids[[i]], tids[[j]]))
            m[i,j] <- m_X[i]+m_X[j]-2*m_XiXj
            m[j,i] <- m[i,j] ## dists are symmetric
          }
        }
        
        dist <- as.dist(m)
        attr(dist, "method") <- method
        return(dist)
        
      }
    }
    
    
    ## use methods for transactions
    if(!is.null(y)) y <- items(y)
    dissimilarity(items(x), y, method = method, args = args, 
      which = which)
  })   


## helper to compute tidLists for associations
## returns a list and not a tidList object
.associationTidLists <- function(x, trans) {
  I <- LIST(items(x), decode=FALSE)
  tlists <- LIST(as(trans, "tidLists"), decode = FALSE)
  
  tids <- list()
  for(i in 1:length(I)) {
    v <- I[[i]]
    tids[[i]] <- tlists[[v[1]]]
    if(length(v)>1) for(j in 2:length(v)) {
      tids[[i]] <- intersect(tids[[i]], tlists[[v[j]]])
    }
  }
  
  tids
}


##*******************************************************************
## Affinity
## For "Affinity" between items, see
## Aggarwal et al., "Finding Localized Association Rules in Market
## Basket Data".

setMethod("affinity", signature(x = "matrix"),
  function(x) {
    ## Affinity is basically the Jaccard similarity between items
    new("ar_similarity", 
      as.matrix(1 - dissimilarity(t(x), method = "Jaccard")),
      method = "Affinity")
    ## Fix: S4 as(..., "matrix") does not work
  })

setMethod("affinity", signature(x = "itemMatrix"),
  function(x) {
    affinity(as(x, "matrix"))
  })

