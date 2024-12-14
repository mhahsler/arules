e <- new("ngCMatrix")
x <- new("ngCMatrix",
  p = as.integer(c(0, 3, 5, 7, 7, 10)),
  i = as.integer(c(1, 2, 4, 1, 2, 0, 4, 0, 2, 5)),
  Dim = as.integer(c(6, 5))
)
rownames(x) <- paste("I", 1:6, sep = "")
colnames(x) <- paste("T", 1:5, sep = "")
# x

##
# unclass(x)

## validity
expect_true(getValidity(getClassDef("CsparseMatrix"))(x))
expect_true(.Call(arules:::R_valid_ngCMatrix, x))
expect_true(.Call(arules:::R_valid_ngCMatrix, e))

## reorder
expect_equal(.Call(arules:::R_recode_ngCMatrix, x, 6:1), x[6:1, ])
expect_equal(.Call(arules:::R_recode_ngCMatrix, e, integer()), e)

## recode (add columns)
expect_identical(
  .Call(arules:::R_recode_ngCMatrix, x, c(1L, 3:7)),
  as(rbind(
    x[1, , drop = FALSE],
    Matrix(0, ncol = 5, sparse = TRUE),
    x[2:6, , drop = FALSE]
  ), "nsparseMatrix")
)

## cbind
expect_identical(.Call(arules:::R_cbind_ngCMatrix, e, e), cbind(e, e))
expect_identical(.Call(arules:::R_cbind_ngCMatrix, x, x), cbind(x, x))

## logical OR
expect_identical(
  .Call(arules:::R_or_ngCMatrix, x, x),
  as(x | x, "nsparseMatrix")
)
expect_identical(
  .Call(arules:::R_or_ngCMatrix, e, e),
  as(e | e, "nsparseMatrix")
)

## crossprod and tcrossprod
expect_equal(
  .Call(arules:::R_crosstab_ngCMatrix, x, NULL, TRUE),
  as(Matrix::tcrossprod(as(as(x, "ngCMatrix"), "dsparseMatrix")), "matrix")
)
expect_equal(
  .Call(arules:::R_crosstab_ngCMatrix, x, NULL, FALSE),
  as(Matrix::crossprod(as(as(x, "ngCMatrix"), "dsparseMatrix")), "matrix")
)

###
