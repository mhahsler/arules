data("Income")

## find and some frequent itemsets
its <- eclat(Income, control=list(verbose=FALSE))[1:5]

expect_equal(support(its, Income), 
  quality(its)$support)
expect_equal(support(its, Income, type = "absolute"), 
  quality(its)$support * length(Income))

expect_equal(support(its, Income, control=list("tidlists")), 
  quality(its)$support)
expect_equal(support(its, Income, type = "absolute", control=list("tidlists")), 
  quality(its)$support * length(Income))

## weights are missing
expect_error(support(its, Income, weighted = TRUE))

## add weights
transactionInfo(Income)$weight <- 1
expect_equal(support(its, Income, weighted = TRUE),
  quality(its)$support)

## check SunBai data
data(SunBai)

## compare weclat with eclat
data(Adult)
is1 <- eclat(Adult, control=list(verbose = FALSE))
is2 <- weclat(Adult, control=list(verbose = FALSE))
k <- match(is1, is2)
expect_equal(quality(is1)$support, quality(is2)$support[k])  

## degenerate transactions
t <- new("transactions")
s <- weclat(t, control=list(verbose = FALSE))
#s
expect_equal(length(s), 0L)

