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
expect_equal(unname(as(ss, "matrix")), t(c(T, F, T, F, T)))

### sparse (should all be true)
expect_equal(as.matrix(is.superset(is, is)),is.superset(is, is, sparse = FALSE))
expect_equal(as.matrix(is.superset(is)), is.superset(is, sparse = FALSE))
expect_equal(as.matrix(is.superset(is[5], is)), 
  is.superset(is[5], is, sparse = FALSE))
expect_equal(as.matrix(is.superset(is, is, proper=TRUE)),
  is.superset(is, is, proper=TRUE, sparse = FALSE))

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
expect_equal(unname(as(ss, "matrix")), t(c(T, T, F, F, T)))

### is.maximal
quality(is)$isMaximal <- is.maximal(is)
#inspect(is)
expect_equal(quality(is)$isMaximal, c(F, T, F, T, T))


### is.closed
db <- as(data, "transactions")
is <- eclat(db, parameter = list(supp = 0), control = list(verbose = FALSE))

quality(is) <- cbind(quality(is), isClosed = is.closed(is))
#inspect(is)
expect_equal(quality(is)$isClosed[1:5], c(T, T, F, F, F))


### union, intersection, setequal, setdiff, is.element
rules <- apriori(data, control = list(verbose = FALSE))
r1 <- rules[1:10]
r2 <- rules[6:20]

expect_equal(length(union(r1,r2)), 20L)

expect_equal(length(intersect(r1,r2)), 5L)

expect_false(setequal(r1,r2))
expect_true(setequal(r1,r1))
expect_true(setequal(r1,c(rules[5:10], rules[1:5])))

expect_equal(length(setdiff(r1, r2)), 5L)
expect_equal(length(setdiff(r2, r1)), 10L)

expect_true(is.element(rules[5], r1))
expect_false(is.element(rules[15], r1))

# union(setA,setB)= setA + setB - intersect(setA,setB)
expect_equal(length(union(r1, r2)), length(c(r1, r2)) - length(intersect(r1, r2)))

# Test setequal with incompatible itemMatrices containing the same itemsets
d1 <- as(data, "itemMatrix")
expect_true(setequal(d1,d1))

d2 <- merge(d1[,6:7], d1[,1:5])
compatible(d1, d2)
expect_warning(expect_true(setequal(d1,d2)))
expect_warning(expect_true(setequal(union(d1, d2), intersect(d1, d2))))


