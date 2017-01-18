library("arules")
library("testthat")

context("ngCMatrix")

e <- new("ngCMatrix")
x <- new("ngCMatrix", p   = as.integer(c(0,3,5,7,7,10)),
                      i   = as.integer(c(1,2,4,1,2,0,4,0,2,5)),
                      Dim = as.integer(c(6,5)))
rownames(x) <- paste("I", 1:6, sep = "")
colnames(x) <- paste("T", 1:5, sep = "")
#x

##
#unclass(x)

## validity
expect_true(getValidity(getClassDef("CsparseMatrix"))(x))
expect_true(.Call(arules:::R_valid_ngCMatrix, x))
expect_true(.Call(arules:::R_valid_ngCMatrix, e))

## t
expect_identical(.Call(arules:::R_transpose_ngCMatrix, x), t(x))
expect_identical(.Call(arules:::R_transpose_ngCMatrix, e), t(e))

## column/row subset (index can only be integer now)
s <- as.integer(c(1,1,3,4))
expect_equal(x[,s], .Call(arules:::R_colSubset_ngCMatrix, x, s))

#
expect_identical(.Call(arules:::R_colSubset_ngCMatrix, e, integer()), e)
expect_equal(x[s,], .Call(arules:::R_rowSubset_ngCMatrix, x, s))

#
expect_identical(.Call(arules:::R_rowSubset_ngCMatrix, e, integer()), e)

## reorder
expect_equal(.Call(arules:::R_recode_ngCMatrix, x, 6:1), x[6:1,])
expect_equal(.Call(arules:::R_recode_ngCMatrix, e, integer()), e)

## recode (add columns)
expect_identical(.Call(arules:::R_recode_ngCMatrix, x, c(1L,3:7)),
  as(rbind(x[1,,drop=FALSE], 
    Matrix(0, ncol = 5, sparse = TRUE), 
    x[2:6,, drop = FALSE]), "ngCMatrix"))

## cbind
expect_identical(.Call(arules:::R_cbind_ngCMatrix, e, e), cbind(e,e))
expect_identical(.Call(arules:::R_cbind_ngCMatrix, x, x), cbind(x, x))

## logical OR
expect_identical(.Call(arules:::R_or_ngCMatrix, x, x), 
  as(as(x|x, "dgCMatrix"), "ngCMatrix"))
expect_identical(.Call(arules:::R_or_ngCMatrix, e, e), 
  as(as(e|e, "dgCMatrix"), "ngCMatrix"))

## row sums
expect_equal(.Call(arules:::R_rowSums_ngCMatrix, x), rowSums(x))
expect_equal(.Call(arules:::R_rowSums_ngCMatrix, e), rowSums(e))

## column sums
expect_equal(colSums(x), .Call(arules:::R_colSums_ngCMatrix, x))
expect_equal(.Call(arules:::R_colSums_ngCMatrix, e), colSums(e))

## crossprod and tcrossprod
expect_equal(.Call(arules:::R_crosstab_ngCMatrix, x, NULL, TRUE),
  as(Matrix::tcrossprod(as(x, "dgCMatrix")), "matrix"))
expect_equal(.Call(arules:::R_crosstab_ngCMatrix, x, NULL, FALSE),
  as(Matrix::crossprod(as(x, "dgCMatrix")), "matrix"))

###

