library("arules")
library("testthat")

set.seed(20070611)

context("itemMatrix")

### Generate random data and coerce data to itemMatrix.
m <- matrix(runif(50)>0.8, ncol=5)
dn <- list(paste("trans", seq(nrow(m)), sep=""), 
           paste("item", seq(ncol(m)), sep=""))
dimnames(m) <- dn
i <- as(m, "itemMatrix")


### number of rows
expect_identical(length(i), nrow(m))
expect_identical(dim(i), dim(m))

### subsetting
expect_identical(as(i[1:5], "matrix"), m[1:5,])
expect_identical(as(i[1:5,1:2], "matrix"), m[1:5, 1:2])
expect_identical(as(i[-1,-1], "matrix"), m[-1,-1])
expect_identical(as(i[rep(c(T,F), nrow(m)/2), c(T,F,F,F,T)], "matrix"),
                 m[rep(c(T,F), nrow(m)/2), c(T,F,F,F,T)])

### dimnames
expect_identical(dimnames(i), dn)
expect_identical(dimnames(i[c("trans1", "trans10"), c("item5", "item4")]),
                 dimnames(m[c("trans1", "trans10"), c("item5", "item4")]))

### dimnames<-
j <- i
dn2 <- list(paste("trans", LETTERS[1:nrow(j)], sep = ""), 
    paste("items", LETTERS[1:ncol(j)], sep = ""))
dimnames(j) <- dn2
expect_identical(dimnames(j), dn2)

### test for unique items
expect_error(i[,c(1,1)])

### test export as sparse matrix
#dgc <- as(i, "dgCMatrix")
#expect_true(all(t(m)==dgc))
#expect_identical(dimnames(dgc),dimnames(t(m)))

ngc <- as(i, "ngCMatrix")
expect_true(all(t(m)==ngc))
expect_identical(dimnames(ngc),dimnames(t(m)))

#expect_identical(i, as(dgc, "itemMatrix"))
expect_identical(i, as(ngc, "itemMatrix"))

## crossTable
expect_equivalent(crossTable(i), crossprod(as(i, "matrix")))


