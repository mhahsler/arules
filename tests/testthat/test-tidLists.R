data <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("b", "e"),
  c("a", "d"),
  c("d", "e"),
  c("d", "f"),
  c("a", "b", "d", "e", "f", "g")
)
names(data) <- paste("Tr", c(1:8), sep = "")

##################################################
### test transactions

trans <- as(data, "transactions")


##########################################################################
### test tidLists

tl <- (as(trans, "tidLists"))
# tl
# inspect(tl)

expect_identical(dim(tl), rev(dim(trans)))
expect_identical(length(tl), nitems(trans))
expect_identical(transactionInfo(tl), transactionInfo(trans))

expect_identical(length(as(tl, "list")), nitems(trans))
expect_identical(as(tl, "matrix"), t(as(trans, "matrix")))

## coercion back to transactions
expect_identical(as(tl, "transactions"), trans)

## extract
expect_identical(tl[2:3, 3:4], as(trans[3:4, 2:3], "tidLists"))

## size
expect_identical(size(tl), unname(sapply(as(tl, "list"), length)))

## check
transactionInfo(tl) <- cbind(transactionInfo(tl), additional = 1)
