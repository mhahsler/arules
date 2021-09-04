library("arules")
library("testthat")

# Example from Liu et al (2008)
trans_list <- list(
  t1 = c("a","b","c"),
  t2 = c("a","b", "c", "d"),
  t3 = c("a","d"),
  t4 = c("a","c")
)

trans <- transactions(trans_list)
its <- apriori(trans, support = 1/4, target = "frequent itemsets")

context("is.generator")
# with minsup = 1, frequent generator itemsets are: emptyset, b, c, d, bd, cd.
ig <- is.generator(its)
expect_true(setequal(names(ig)[ig], c("{b}", "{c}", "{d}", "{b,d}", "{c,d}")))

context("is.closed")
# with minsup = 1, frequent closed itemsets are: a, ac, ad, abc, abcd.
ic <- is.closed(its)
expect_true(setequal(names(ic)[ic], c("{a}", "{a,c}", "{a,d}", "{a,b,c}", "{a,b,c,d}")))

ic2 <- apriori(trans, support = 1/4, target = "closed frequent itemsets")
expect_true(setequal(its[ic], ic2))

context("is.maximal")
# 
im <- is.maximal(its)
expect_true(setequal(names(im)[im], c("{a,b,c,d}")))

im2 <- apriori(trans, support = 1/4, target = "maximally frequent itemsets")
expect_true(setequal(its[im], im2))
