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

#########################################################################
## dissimilarity is currently implemented using dense matrices
##
## Memory usage of dissimilarity with  "jaccard", "matching" ore "dice"
## (on a 32 bit machine)
##
## input: x and y (integer, 4 byte)
## intermediate: 4 matrices size nx x ny (double, 8 byte)
## result: either 1 dist nx x ny /2 or full matrix for crossdissim.
##
## total w/o input: about 5 * nx * ny * 8 byte


#' Classes dist, ar_cross_dissimilarity and ar_similarity --- Proximity
#' Matrices
#'
#' Simple classes to represent proximity matrices.  For compatibility with
#' clustering functions in \code{R}, we represent dissimilarities as the
#' \code{S3} class \code{dist}.  For cross-dissimilarities and similarities, we
#' provide the \code{S4} classes \code{ar_cross_dissimilarities} and
#' \code{ar_similarities}.
#'
#'
#' @name proximity-classes
#' @family proximity classes and functions
#' 
#' @section Objects from the Class: \code{dist} objects are the result of
#' calling the method \code{\link{dissimilarity}} with one argument or any
#' \code{R} function returning a \code{S3 dist} object.
#'
#' \code{ar_cross_dissimilarity} objects are the result of calling the method
#' \code{\link{dissimilarity}} with two arguments, by calls of the form
#' \code{new("similarity", ...)}, or by coercion from matrix.
#'
#' \code{ar_similarity} objects are the result of calling the method
#' \code{\link{affinity}}, by calls of the form \code{new("similarity", ...)},
#' or by coercion from matrix.
#' @author Michael Hahsler
#' @seealso [stats::dist()], [proxy::dist()]
#' @keywords classes
setOldClass("dist")

#' @rdname proximity-classes
setClass("ar_similarity",
  contains = "matrix",
  representation(method = "character"))

#' @rdname proximity-classes
setClass("ar_cross_dissimilarity",
  contains = "matrix",
  representation(method = "character"))


#' Dissimilarity Matrix Computation for Associations and Transactions
#'
#' Provides the generic function \code{dissimilarity} and the S4 methods to
#' compute and returns distances for binary data in a \code{matrix},
#' [transactions] or [associations] which
#' can be used for grouping and clustering. See Hahsler (2016) for an
#' introduction to distance-based clustering of association rules.
#'
#'
#' @aliases dissimilarity dist
#' @family proximity classes and functions
#' 
#' @param x the set of elements (e.g., `matrix`, [itemMatrix], [transactions],
#' [itemsets], [rules]).
#' @param y `NULL` or a second set to calculate cross dissimilarities.
#' @param method the distance measure to be used. Implemented measures are
#' (defaults to `"jaccard"`):
#'
#'    * `"affinity"`:
#'       measure based on the [affinity()], a similarity measure between
#'       items. It is defined as the average affinity between the items in two
#'       transactions (see Aggarwal et al. (2002)). If `x` is not the full
#'       transaction set `args` needs to contain either precalculated affinities
#'       as element `"affinities"` or the transaction set as element
#'       `"transactions"`.
#'    * `"cosine"`: the Cosine distance.
#'    *  `"dice"`: Dice's coefficient defined by Dice (1945).
#'      Similar to Jaccard but gives double the weight to agreeing items.
#'    * `"euclidean"`: the Euclidean distance.
#'    * `"jaccard"`: the number of items which occur in both elements
#'      divided by the total number of items in the elements (Sneath, 1957).  This
#'      measure is often also called: binary, asymmetric binary, etc.
#'    * `"matching"`: the matching coefficient defined by
#'       Sokal and Michener (1958). This coefficient gives the same weight to
#'       presents and absence of items.
#'    * `"pearson"` A distance calculated by \eqn{1 - r}
#'       if \eqn{r>1} and \eqn{1} otherwise, where \eqn{r} is the Pearson's correlation
#'       coefficient.
#'    * `"phi"`: same as `"pearson"`. Pearson's correlation coefficient
#'      reduces to the phi coefficient for the 2x2 contingency tables used
#'      here.
#'    * `"toivonen"`: Method described in Toivonen et al. (1995).  For
#'      rules this measure is only defined between rules with the same consequent.
#'      The distance between two rules is defined as the number of transactions
#'      which is covered by only one of the two rules.  The transactions used to
#'      mine the associations has to be passed on via `args` as element
#'      `"transactions"`.
#'   * `"gupta"`: Method described in Gupta et al. (1999).  The
#'      distance between two rules is defined as 1 minus the proportion of
#'      transactions which are covered by both rules in the transactions covered by
#'      each rule individually.  The transactions used to mine the associations has
#'      to be passed on via `args` as element `"transactions"`.
#'
#' @param args a list of additional arguments for the methods.
#' @param which a character string indicating if the dissimilarity should be
#' calculated between transactions/associations (default) or items (use `"items"`).
#' @param ... further arguments.
#' @return returns an object of class `dist`.
#' @author Michael Hahsler
#' @references Aggarwal, C.C., Cecilia Procopiuc, and Philip S. Yu. (2002)
#' Finding localized associations in market basket data.  _IEEE Trans. on
#' Knowledge and Data Engineering_ 14(1):51--62.
#'
#' Dice, L. R. (1945) Measures of the amount of ecologic association between
#' species. _Ecology_ 26, pages 297--302.
#'
#' Gupta, G., Strehl, A., and Ghosh, J. (1999) Distance based clustering of
#' association rules. _In Intelligent Engineering Systems Through
#' Artificial Neural Networks (Proceedings of ANNIE 1999)_, pages 759-764. ASME
#' Press.
#'
#' Hahsler, M. (2016) Grouping association rules using lift. In C.  Iyigun, R.
#' Moghaddess, and A. Oztekin, editors, _11th INFORMS Workshop on Data Mining
#' and Decision Analytics_ (DM-DA 2016).
#'
#' Sneath, P. H. A. (1957) Some thoughts on bacterial classification.
#' _Journal of General Microbiology_ 17, pages 184--200.
#'
#' Sokal, R. R. and Michener, C. D. (1958) A statistical method for evaluating
#' systematic relationships. _University of Kansas Science Bulletin_ 38,
#' pages 1409--1438.
#'
#' Toivonen, H., Klemettinen, M., Ronkainen, P., Hatonen, K. and Mannila H.
#' (1995) Pruning and grouping discovered association rules. _In
#' Proceedings of KDD'95_.
#' @keywords cluster models
#' @examples
#'
#' ## cluster items in Groceries with support > 5%
#' data("Groceries")
#'
#' s <- Groceries[, itemFrequency(Groceries) > 0.05]
#' d_jaccard <- dissimilarity(s, which = "items")
#' plot(hclust(d_jaccard, method = "ward.D2"), main = "Dendrogram for items")
#'
#' ## cluster transactions for a sample of Adult
#' data("Adult")
#' s <- sample(Adult, 500)
#'
#' ##  calculate Jaccard distances and do hclust
#' d_jaccard <- dissimilarity(s)
#' hc <- hclust(d_jaccard, method = "ward.D2")
#' plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Jaccard)")
#'
#' ## get 20 clusters and look at the difference of the item frequencies (bars)
#' ## for the top 20 items) in cluster 1 compared to the data (line)
#' assign <- cutree(hc, 20)
#' itemFrequencyPlot(s[assign == 1], population = s, topN = 20)
#'
#' ## calculate affinity-based distances between transactions and do hclust
#' d_affinity <- dissimilarity(s, method = "affinity")
#' hc <- hclust(d_affinity, method = "ward.D2")
#' plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Affinity)")
#'
#' ## cluster association rules
#' rules <- apriori(Adult, parameter = list(support = 0.3))
#' rules <- subset(rules, subset = lift > 2)
#'
#' ## use affinity to cluster rules
#' ## Note: we need to supply the transactions (or affinities) from the
#' ## dataset (sample).
#' d_affinity <- dissimilarity(rules, method = "affinity",
#'   args = list(transactions = s))
#' hc <- hclust(d_affinity, method = "ward.D2")
#' plot(hc, main = "Dendrogram for Rules (Affinity)")
#'
#' ## create 4 groups and inspect the rules in the first group.
#' assign <- cutree(hc, k = 3)
#' inspect(rules[assign == 1])
#'
setGeneric("dissimilarity",
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    ...)
    standardGeneric("dissimilarity"))

#' @rdname dissimilarity
setMethod("dissimilarity", signature(x = "matrix"),
  function(x,
    y = NULL,
    method = NULL,
    args = NULL) {
    ## Compute dissimilarities on binary data
    
    ## make sure the input is a 0-1 matrix or a logical matrix
    is.zeroone <- function(x)
      (all(x == 0 | x == 1))
    
    storage.mode(x) <- "numeric"
    if (!is.zeroone(x))
      stop("x is not a binary matrix (0-1 or logical)!")
    
    ## cross dissimilarities? Check y
    if (!is.null(y)) {
      if (!is.matrix(y))
        stop("'y' not a matrix")
      storage.mode(y) <- "numeric"
      if (!is.zeroone(y))
        stop("y is not a binary matrix (0-1 or logical)!")
      cross <- TRUE
    } else
      cross <- FALSE
    
    builtin_methods <- c("affinity",
      "jaccard",
      "matching",
      "dice",
      "cosine",
      "euclidean",
      "pearson",
      "phi")
    
    if (is.null(method))
      ind <- 2      # Jaccard is standard
    else if (is.na(ind <- pmatch(tolower(method),
      tolower(builtin_methods))))
      stop(
        gettextf(
          "Value '%s' is not a valid abbreviation for a similarity method.",
          method
        ),
        domain = NA
      )
    
    method <- builtin_methods[ind]
    
    ## affinity is special!
    if (ind == 1) {
      ## Affinity.
      ## for rules and itemsets we need transactions or affinities
      
      ## given affinities or transactions? Otherwise, calculate!
      if (!is.null(args$aff))
        affinities <- args$aff
      else if (!is.null(args$trans))
        affinities <- affinity(args$trans)
      else
        affinities <- affinity(x)
      
      ## Normalize transaction incidences by transaction length.
      x <- x / pmax(rowSums(x), 1)
      
      if (!cross) {
        dist <- 1 - stats::as.dist(x %*% affinities %*% t(x))
      } else{
        y <- y / pmax(rowSums(y), 1)
        dist <- new("ar_cross_dissimilarity",
          1 - x %*% affinities %*% t(y))
      }
      
      ## Euclidean is special too
    } else if (ind == 6) {
      if (cross)
        stop("Euclidean cross-distance not implemented.")
      
      dist <- dist(x, method = "euclidean")
      
      ## Pearson correlation coefficient (Note: cor is calculated
      ##    between columns and we only use pos. correlation)
      ## Phi is the same as pearson
    } else if (ind == 7 || ind == 8) {
      if (!cross) {
        ## warnings for zero variance!
        suppressWarnings(cm <- stats::cor(t(x), method = "pearson"))
        ## pairwise complete is very slow
        #cm <- stats::cor(t(x), method = "pearson",
        #	  use="pairwise.complete.obs")
        cm[cm < 0 | is.na(cm)] <- 0
        dist <- stats::as.dist(1 - cm)
      } else {
        suppressWarnings(cm <- stats::cor(t(x), t(y), method = "pearson"))
        #cm <- stats::cor(t(x), t(y), method = "pearson",
        #	use="pairwise.complete.obs")
        cm[cm < 0 | is.na(cm)] <- 0
        dist <- new("ar_cross_dissimilarity", 1 - cm)
      }
    } else {
      if (!cross)
        y <- x
      
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
      b <-
        matrix(rowSums(y),
          nrow = nx,
          ncol = ny,
          byrow = TRUE) - a
      
      #a_b_c <- matrix(rowSums(x), nrow = nx, ncol = ny) +
      #matrix(rowSums(y), nrow = nx, ncol = ny, byrow = TRUE) - a
      
      
      if (ind == 2) {
        ## Jaccard == binary (Sneath, 1957)
        #dist <- dist(as(x, "matrix"), "binary")
        
        dist <- 1 - a / (a + b + c)
        #dist <- 1 - (a/a_b_c)
        
      } else if (ind == 3) {
        ## Matching Coefficient (Sokal and Michener, 1958)
        
        #we need d only here
        #d <- ncol(x) - a_b_c
        d <- ncol(x) - (a + b + c)
        
        dist <- 1 - (a + d) / (a + b + c + d)
        #dist <- 1 - ((a + d) / (a_b_c + d))
        
      } else if (ind == 4) {
        ## Dice Coefficient (Dice, 1945)
        dist <- 1 - 2 * a / (2 * a + b + c)
        #dist <- 1 - 2 * a / (a + a_b_c)
        
      } else if (ind == 5) {
        ## Cosine
        dist <- 1 - a / sqrt((a + b) * (a + c))
      }
    }
    
    # in case we divided by zero
    dist[is.nan(dist)] <- 0
    
    if (!cross) {
      # return a S3 "dist" object just add "ar_dissimilarity"
      dist <- stats::as.dist(dist)
      attr(dist, "method") <- method
      #class(dist) <- c("ar_dissimilarity", class(dist))
      return(dist)
      
    } else{
      # return a S4 "ar_cross_dist" object
      dist <- new("ar_cross_dissimilarity", dist, method = method)
      return(dist)
    }
    
  })


#' @rdname dissimilarity
setMethod("dissimilarity", signature(x = "itemMatrix"),
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    which = "transactions") {
    ## use items?
    if (is.null(which))
      which <- "transactions"
    items <- pmatch(tolower(which), "items", nomatch = 0) == 1
    
    x <- as(x, "matrix")
    if (items)
      x <- t(x)
    
    if (!is.null(y)) {
      y <- as(y, "matrix")
      if (items)
        y <- t(y)
    }
    
    dissimilarity(
      x = x,
      y = y,
      method = method,
      args = args
    )
  })

#' @rdname dissimilarity
setMethod("dissimilarity", signature(x = "associations"),
  function(x,
    y = NULL,
    method = NULL,
    args = NULL,
    which = "associations") {
    if (is.null(method))
      method <- "jaccard"   # Jaccard is standard
    
    builtin_methods <- c("gupta", "toivonen")
    
    if (!is.na(ind <- pmatch(tolower(method),
      tolower(builtin_methods)))) {
      method <- builtin_methods[ind]
      if (!is.null(y))
        stop("Cross dissimilarities not implemented for this method!")
      trans <- args$trans
      if (is.null(trans))
        stop("Transactions needed in args for this method!")
      
      if (ind == 1) {
        ##gupta: use tidlist intersection
        
        ## FIXME: tidlist operations should be made functions
        
        ## calculate tidlist for rules
        tids <- .associationTidLists(x, trans)
        
        m_X <- sapply(tids, length)
        n <- length(x)
        
        m <- matrix(nrow = n, ncol = n)
        for (i in 1:n) {
          for (j in i:n) {
            m_XiXj <- length(intersect(tids[[i]], tids[[j]]))
            m[i, j] <- 1 - m_XiXj / (m_X[i] + m_X[j] - m_XiXj)
            m[j, i] <- m[i, j] ## dists are symmetric
          }
        }
        
        dist <- stats::as.dist(m)
        attr(dist, "method") <- method
        return(dist)
        
      } else{
        ## toivonen
        
        ## only one RHS allowed
        if (length(unique(rhs(x))) != 1)
          stop("Only a single RHS allowed for this method!")
        
        ## calculate tidlist for rules
        tids <- .associationTidLists(x, trans)
        
        m_X <- sapply(tids, length)
        n <- length(x)
        
        m <- matrix(nrow = n, ncol = n)
        for (i in 1:n) {
          for (j in i:n) {
            m_XiXj <- length(intersect(tids[[i]], tids[[j]]))
            m[i, j] <- m_X[i] + m_X[j] - 2 * m_XiXj
            m[j, i] <- m[i, j] ## dists are symmetric
          }
        }
        
        dist <- stats::as.dist(m)
        attr(dist, "method") <- method
        return(dist)
        
      }
    }
    
    
    ## use methods for transactions
    if (!is.null(y))
      y <- items(y)
    dissimilarity(items(x),
      y,
      method = method,
      args = args,
      which = which)
  })


## helper to compute tidLists for associations
## returns a list and not a tidList object
.associationTidLists <- function(x, trans) {
  I <- LIST(items(x), decode = FALSE)
  tlists <- LIST(as(trans, "tidLists"), decode = FALSE)
  
  tids <- list()
  for (i in seq_len(length(I))) {
    v <- I[[i]]
    tids[[i]] <- tlists[[v[1]]]
    if (length(v) > 1)
      for (j in 2:length(v)) {
        tids[[i]] <- intersect(tids[[i]], tlists[[v[j]]])
      }
  }
  
  tids
}


#' Computing Affinity Between Items
#'
#' Provides the generic function `affinity()` and the S4 methods to compute
#' and return a similarity matrix with the affinities between items for a set
#' of [transactions].
#'
#' Affinity between the two items \eqn{i} and \eqn{j} is defined by Aggarwal et
#' al. (2002) as \deqn{A(i,j) = \frac{sup(\{i,j\})}{sup(\{i\}) + sup(\{j\}) -
#' sup(\{i,j\})},}{A(i,j) = sup({i,j})/(sup({i}) + sup({j}) - sup({i,j})),}
#' where \eqn{sup(.)} is the support measure. This means that affinity is the
#' Jaccard similarity between items.
#'
#' @family proximity classes and functions
#' @param x a matrix or an object of class [itemMatrix] or
#'   [transactions].
#' @return returns an object of class [ar_similarity-class] which represents the
#'   affinities between items in `x`.
#' @author Michael Hahsler
#' @references Charu C. Aggarwal, Cecilia Procopiuc, and Philip S. Yu (2002)
#' Finding localized associations in market basket data, _IEEE Trans. on
#' Knowledge and Data Engineering,_ 14(1):51--62.
#' @keywords cluster models
#' @examples
#' data("Adult")
#'
#' ## choose a sample, calculate affinities
#' s <- sample(Adult, 500)
#' s
#'
#' a <- affinity(s)
#' image(a)
setGeneric("affinity",
  function(x)
    standardGeneric("affinity"))


#' @rdname affinity
setMethod("affinity", signature(x = "matrix"),
  function(x) {
    ## Affinity is basically the Jaccard similarity between items
    new("ar_similarity",
      as.matrix(1 - dissimilarity(t(x), method = "Jaccard")),
      method = "Affinity")
    ## Fix: S4 as(..., "matrix") does not work
  })

#' @rdname affinity
setMethod("affinity", signature(x = "itemMatrix"),
  function(x) {
    affinity(as(x, "matrix"))
  })
