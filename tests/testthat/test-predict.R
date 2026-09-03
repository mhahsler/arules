data <- transactions(list(
  a = "a",
  b = "b",
  ac = c("a", "c"),
  bc = c("b", "c")
))

examples <- data[1:2]
newdata <- data
cluster_labels <- c(10L, 20L)

expect_identical(
  predict(examples, newdata, labels = cluster_labels),
  c(10L, 20L, 10L, 20L)
)

## Force predict() to process one new observation per block.
one_row_in_mb <- 5 * length(examples) * 8 / 1024^2
expect_identical(
  predict(examples, newdata, labels = cluster_labels, blocksize = one_row_in_mb),
  c(10L, 20L, 10L, 20L)
)

## A one-row input must also enter and complete the blocked path.
expect_identical(
  predict(examples, newdata[1], labels = cluster_labels, blocksize = one_row_in_mb),
  10L
)
