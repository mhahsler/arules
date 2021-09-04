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
        
        min_sup_l_prime <- sapply(l, FUN = function(s) min(c(sup[s], 1)))
        
        sup != min_sup_l_prime
    })
