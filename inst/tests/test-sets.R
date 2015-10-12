library("arules")
library("testthat")

context("set operations")

data <- list(
    c("A", "B"),
    c("A", "B", "C", "G"),
    c("C", "D"),
    c("E", "F"),
    c("A", "B", "C", "D")
    )


### is.superset / is.subset
is <- new("itemsets",  items = as(data, "itemMatrix"))

### find supersets in is
ss <- is.superset(is, is)
expect_identical(colnames(ss), labels(is))
expect_identical(rownames(ss), labels(is))
expect_equal(unname(diag(ss)), rep(TRUE, nrow(ss)))

ss2 <- is.superset(is)
expect_identical(ss, ss2)

ss <- is.superset(is, is, proper = TRUE)
expect_equal(unname(diag(ss)), rep(FALSE, nrow(ss)))

ss <- is.superset(is[5], is)
expect_equal(unname(ss), t(c(T, F, T, F, T)))

### sparse (should all be true)
expect_equal(as.matrix(is.superset(is, is, sparse=TRUE)),is.superset(is, is))
expect_equal(as.matrix(is.superset(is, sparse=TRUE)), is.superset(is))
expect_equal(as.matrix(is.superset(is[5], is, sparse=TRUE)), 
  is.superset(is[5], is))
expect_equal(as.matrix(is.superset(is, is, proper=TRUE, sparse=TRUE)),
  is.superset(is, is, proper=TRUE))

### find subsets in is
ss <- is.subset(is, is)
expect_identical(colnames(ss), labels(is))
expect_identical(rownames(ss), labels(is))
expect_equal(unname(diag(ss)), rep(TRUE, nrow(ss)))

ss <- is.subset(is, is, proper = TRUE)
expect_identical(colnames(ss), labels(is))
expect_identical(rownames(ss), labels(is))
expect_equal(unname(diag(ss)), rep(FALSE, nrow(ss)))
  
ss <- is.subset(is[1], is)
expect_equal(unname(ss), t(c(T, T, F, F, T)))

### is.maximal
quality(is)$isMaximal <- is.maximal(is)
#inspect(is)
expect_equal(quality(is)$isMaximal, c(F, T, F, T, T))


### is.closed
db <- as(data, "transactions")
expect_warning(
  is <- eclat(db, parameter = list(supp = 0), control = list(verbose = FALSE))
)

quality(is) <- cbind(quality(is), isClosed = is.closed(is))
#inspect(is)
expect_equal(quality(is)$isClosed[1:5], c(T, T, F, F, F))
