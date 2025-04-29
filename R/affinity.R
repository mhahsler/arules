#' Computing Affinity Between Items
#'
#' Provides the generic function `affinity()` and methods to compute
#' and return a similarity matrix with the affinities between items for a set
#' itemsets stored in a matrix or in [transactions] via its superclass [itemMatrix].
#'
#' Affinity between the two items \eqn{i} and \eqn{j} is defined by Aggarwal et
#' al. (2002) as \deqn{A(i,j) = \frac{supp(\{i,j\})}{supp(\{i\}) + supp(\{j\}) -
#' supp(\{i,j\})},}{A(i,j) = supp({i,j})/(supp({i}) + supp({j}) - supp({i,j})),}
#' where \eqn{supp(.)} is the support measure. Note that affinity is equivalent to the
#' Jaccard similarity between items.
#'
#' @family proximity classes and functions
#' @param x a matrix or an object of class [itemMatrix] or
#'   [transactions] containing itemsets.
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
setGeneric(
  "affinity",
  function(x) {
    standardGeneric("affinity")
  }
)


#' @rdname affinity
setMethod(
  "affinity", signature(x = "matrix"),
  function(x) {
    ## Affinity is equivalent to the Jaccard similarity between items
    new("ar_similarity",
        as.matrix(1 - dissimilarity(x, method = "jaccard", items = TRUE)),
        method = "Affinity"
    )
    ## Fix: S4 as(..., "matrix") does not work
  }
)

#' @rdname affinity
setMethod(
  "affinity", signature(x = "itemMatrix"),
  function(x) {
    #affinity(as(x, "matrix"))
    new("ar_similarity",
        as(1 - dissimilarity(x, method = "jaccard", items = TRUE), "matrix"),
        method = "Affinity"
    )
  }
)
