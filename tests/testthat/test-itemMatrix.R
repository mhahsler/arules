set.seed(20070611)

### Generate random data and coerce data to itemMatrix.
m <- matrix(runif(50) > 0.8, ncol = 5)
dn <- list(
  paste("trans", seq(nrow(m)), sep = ""),
  paste("item", seq(ncol(m)), sep = "")
)
dimnames(m) <- dn
i <- as(m, "itemMatrix")

### number of rows
expect_identical(length(i), nrow(m))
expect_identical(dim(i), dim(m))

### subsetting
expect_identical(as(i[1:5], "matrix"), m[1:5, ])
expect_identical(as(i[1:5, 1:2], "matrix"), m[1:5, 1:2])
expect_identical(as(i[-1, -1], "matrix"), m[-1, -1])
expect_identical(
  as(i[rep(c(T, F), nrow(m) / 2), c(T, F, F, F, T)], "matrix"),
  m[rep(c(T, F), nrow(m) / 2), c(T, F, F, F, T)]
)

### dimnames
expect_identical(dimnames(i), dn)
expect_identical(
  dimnames(i[c("trans1", "trans10"), c("item5", "item4")]),
  dimnames(m[c("trans1", "trans10"), c("item5", "item4")])
)

### dimnames<-
j <- i
dn2 <- list(
  paste("trans", LETTERS[1:nrow(j)], sep = ""),
  paste("items", LETTERS[1:ncol(j)], sep = "")
)
dimnames(j) <- dn2
expect_identical(dimnames(j), dn2)

### test for unique items
expect_error(i[, c(1, 1)])

### combine (itemInfo)
i2 <- i3 <- i
# no info
itemsetInfo(i2) <- data.frame(matrix(nrow = length(i2), ncol = 0))

expect_equal(
  itemsetInfo(c(i, i2)),
  data.frame(itemsetID = c(
    itemsetInfo(i)$itemsetID,
    rep(NA, times = length(i2))
  ), stringsAsFactors = FALSE)
)
expect_equal(
  itemsetInfo(c(i2, i)),
  data.frame(itemsetID = c(
    rep(NA, times = length(i2)),
    itemsetInfo(i)$itemsetID
  ), stringsAsFactors = FALSE)
)

# additional info
itemsetInfo(i3)$moreInfo <- sample(1:2, length(i3), replace = TRUE)

expect_equal(itemsetInfo(c(i, i3)), data.frame(
  itemsetID = c(itemsetInfo(i)$itemsetID, itemsetInfo(i3)$itemsetID),
  moreInfo = c(rep(NA, times = length(i)), itemsetInfo(i3)$moreInfo),
  stringsAsFactors = FALSE
))
expect_equal(itemsetInfo(c(i3, i)), data.frame(
  itemsetID = c(itemsetInfo(i3)$itemsetID, itemsetInfo(i)$itemsetID),
  moreInfo = c(itemsetInfo(i3)$moreInfo, rep(NA, times = length(i))),
  stringsAsFactors = FALSE
))

# empty associations
expect_equal(itemsetInfo(c(i, i[0])), itemsetInfo(i))
expect_equal(itemsetInfo(c(i[0], i3)), itemsetInfo(i3))



### test export as sparse matrix
# dgc <- as(i, "dgCMatrix")
# expect_true(all(t(m)==dgc))
# expect_identical(dimnames(dgc),dimnames(t(m)))

ngc <- as(i, "ngCMatrix")
expect_true(all(t(m) == ngc))
expect_identical(dimnames(ngc), dimnames(t(m)))

# expect_identical(i, as(dgc, "itemMatrix"))
expect_identical(i, as(ngc, "itemMatrix"))

## crossTable
expect_equivalent(crossTable(i), crossprod(as(i, "matrix")))
