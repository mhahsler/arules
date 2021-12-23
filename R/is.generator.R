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


## Generators
## Yves Bastide, Niolas Pasquier, Rafik Taouil, Gerd Stumme, Lotfi Lakhal
## Mining Minimal Non-redundant Association Rules Using Frequent Closed Itemsets
## July 2000, Lecture Notes in Computer Science
## DOI: 10.1007/3-540-44957-4_65

## Definition 4 (Generators). An itemset g \subseteq I is a (minimal) generator of a
## closed itemset l iff lambda(g) = l and no g' \subseteq I with g' \subseteq g
## such that lambda(g') = l.

## Guimei Liu, Jinyan Li, Limsoon Wong.
## A new concise representation of frequent itemsets using generators and a positive border.
## October 2008, Knowledge and Information Systems 17(1):35-56
## DOI: 10.1007/s10115-007-0111-5

## Definition 1 (Generator). Itemset l is a generator if there does not exist l'
## such that l' \subset l and support(l') = support(l). By definition, the empty set is a generator.

## support(l') >= support(l) so min(support(l' in all subsets of l)) != support(l)





#' Find Generator Itemsets
#'
#' Provides the generic function and the S4 method `is.generator() for
#' finding generator itemsets. Generators are part of concise representations
#' for frequent itemsets.  A generator in a set of itemsets is an itemset that
#' has no subset with the same support (Liu et al, 2008). Note that the empty
#' set is by definition a generator, but it is typically not stored in the
#' itemsets in \pkg{arules}.
#'
#' @family postprocessing
#' @family associations functions
#' 
#' @param x a set of itemsets.
#' @return a logical vector with the same length as \code{x} indicating for
#' each element in \code{x} if it is a generator itemset.
#' @author Michael Hahsler
#' @references Yves Bastide, Niolas Pasquier, Rafik Taouil, Gerd Stumme, Lotfi
#' Lakhal (2000). Mining Minimal Non-redundant Association Rules Using Frequent
#' Closed Itemsets. In \emph{International Conference on Computational Logic},
#' Lecture Notes in Computer Science (LNCS 1861). pages 972--986.
#' \doi{10.1007/3-540-44957-4_65}
#'
#' Guimei Liu, Jinyan Li, Limsoon Wong (2008). A new concise representation of
#' frequent itemsets using generators and a positive border.  \emph{Knowledge
#' and Information Systems} 17(1):35-56.
#' \doi{10.1007/s10115-007-0111-5}
#' @keywords models
#' @examples
#' # Example from Liu et al (2008)
#' trans_list <- list(
#'       t1 = c("a","b","c"),
#'       t2 = c("a","b", "c", "d"),
#'       t3 = c("a","d"),
#'       t4 = c("a","c")
#'       )
#'
#' trans <- transactions(trans_list)
#' its <- apriori(trans, support = 1/4, target = "frequent itemsets")
#'
#' is.generator(its)
#'
setGeneric("is.generator",
  function(x)
    standardGeneric("is.generator"))

#' @rdname is.generator
setMethod("is.generator", signature(x = "itemsets"),
  function(x) {
    if (any(is.na(match(x, unique(x), nomatch = NA))))
      stop("itemsets not unique")
    
    sup <- quality(x)[["support"]]
    #supersets <- is.superset(x, proper = TRUE)
    #l <- .Call(R_asList_ngCMatrix, t(supersets), NULL)
    # subsets are transposed supersets
    subsets <- is.subset(x, proper = TRUE)
    l <- .Call(R_asList_ngCMatrix, subsets, NULL)
    
    min_sup_l_prime <-
      sapply(
        l,
        FUN = function(s)
          min(c(sup[s], 1))
      )
    
    sup != min_sup_l_prime
  })
