### TODO: test dissimilarities between associations

verb <- FALSE
options(digits = 2)

data <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("b", "e"),
  c("a", "c"),
  c("c", "e"),
  c("a", "b", "d", "e")
)
names(data) <- paste("Tr", c(1:7), sep = "")
trans <- transactions(data)


a1 <- affinity(as(trans, "matrix"))
a2 <- affinity(trans)

builtin_methods <- c(
  "jaccard",
#  "affinity", # affinities are different. Needs more testing
  "matching",
  "dice",
  "cosine",
  "euclidean",
  "pearson",
  "phi"
)

for (m in builtin_methods) {
  # compare sparse with dense version
  d <- dissimilarity(trans, method = m)
  expect_equal(dissimilarity(as(trans, "matrix"), method = m), d)
  
  dx <- dissimilarity(trans, trans, method = m)
  expect_identical(dissimilarity(as(trans, "matrix"), as(trans, "matrix"), method = m), dx)
  expect_equivalent(as.dist(dx), d)
}

# test between items
d <- dissimilarity(trans, method = "jaccard", items = TRUE)
expect_equal(attr(d, "Size"), ncol(trans))

d <- dissimilarity(trans, trans, method = "jaccard", items = TRUE)
expect_equal(nrow(d), ncol(trans))
