library("arules")
library("testthat")

skip_if_not_installed("fim4r")

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

r_a <- apriori(trans, control = list(verbose = verb))
r_f <- fim4r(trans, method = "apriori", target = "rules", verbose = verb)

expect_equal(length(r_a), length(r_f))
expect_equal(sort(quality(r_a)$support), sort(quality(r_f)$support))

data(Adult)

### fim4r uses LHS support!
r_a <- apriori(Adult, control = list(verbose = verb))
r_f <- fim4r(Adult, method = "apriori", target = "rules", verbose = verb)
r_ffp <- fim4r(Adult, method = "fpgrowth", target = "rules", verbose = verb)

expect_equal(length(r_a), length(r_f))
expect_equal(length(r_a), length(r_ffp))
expect_equal(sort(quality(r_a)$support), sort(quality(r_f)$support))
expect_equal(sort(quality(r_a)$support), sort(quality(r_ffp)$support))


### fim4r returns also the empty itemset
r_a <- apriori(Adult, target = "frequent itemset", control = list(verbose = verb))
r_f <- fim4r(Adult, method = "apriori", target = "frequent", verbose = verb)
r_ffp <- fim4r(Adult, method = "fpgrowth", target = "frequent", verbose = verb)

expect_equal(length(r_a), length(r_f) - 1L)
expect_equal(length(r_a), length(r_ffp) - 1L)


