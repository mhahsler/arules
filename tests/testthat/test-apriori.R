library("arules")
library("testthat")


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


##########################################################
# test the APRIORI interface
context("APRIORI interface")

### rules (all warnings are for low support)
r <- apriori(trans,
  parameter = list(supp = 0.25, conf = 0),
  control = list(verb = FALSE))

expect_identical(length(r), 18L)

#r
#summary(r)
#inspect(sort(r, by = "lift")[1:7])

### test appearance
r <- apriori(
  trans,
  parameter = list(supp = 0.25, conf = 0),
  appearance = list(rhs = c("a", "b")),
  control = list(verb = FALSE)
)

expect_identical(length(r), 6L)
#inspect(r)

r <- apriori(
  trans,
  parameter = list(supp = 0.25, conf = 0),
  appearance = list(lhs = c("a", "b"), rhs = "c"),
  control = list(verb = FALSE)
)
expect_identical(length(r), 2L)

r <- apriori(
  trans,
  parameter = list(supp = 0.25, conf = 0),
  appearance = list(none = c("a", "b")),
  control = list(verb = FALSE)
)
expect_identical(length(r), 3L)

expect_error(as(list(
  rhs = c("a", "b"),
  lhs = "a",
  labels = itemLabels(trans)
), "APappearance"))


### test lhs.support
r <- apriori(
  trans,
  parameter = list(
    supp = 0.25,
    conf = 0,
    originalSupp = FALSE,
    ext = TRUE
  ),
  control = list(verb = FALSE)
)

expect_true("coverage" %in% colnames(quality(r)))
#inspect(r[1:2])



##########################################################
# test the ECLAT interface

context("ECLAT interface")

f <- eclat(trans, control = list(verb = FALSE))

expect_identical(length(f), 20L)
sup <- c(
  0.14,
  0.14,
  0.14,
  0.29,
  0.29,
  0.29,
  0.14,
  0.14,
  0.29,
  0.14,
  0.14,
  0.14,
  0.14,
  0.29,
  0.57,
  0.71,
  0.71,
  0.43,
  0.43,
  0.29
)
expect_equal(round(quality(f)$support, 2), sup)
expect_equal(labels(f)[5], "{a,d}")

#f
#summary(f)
#inspect(f[1:2])
#labels(f[1:2])


### test subset
f.sub <- subset(f, subset = items %in% "a")
l <- labels(f.sub)
expect_equal(l, grep("a", l, value = T))

### test tidlists
f <-
  eclat(trans,
    parameter = list(tidLists = TRUE),
    control = list(verb = FALSE))

#f
#summary(f)
tl <- tidLists(f)

expect_identical(dim(tl), c(20L, 7L))

#tl
#summary(tl)
#inspect(tl)

expect_equal(as(tl[5], "list"), list('{a,d}' = c("Tr3", "Tr7")))


## Compare if APRIOR and ECLAT produce the same results
data("Income")
fsets <-
  apriori(
    Income,
    parameter = list(target = "frequ", supp = 0.2),
    control = list(verb = FALSE)
  )
esets <-
  eclat(
    Income,
    parameter = list(target = "frequ", supp = 0.2),
    control = list(verb = FALSE)
  )

## compare if output is the same
expect_true(all(table(match(fsets, esets)) == 1))

##########################################################
# test maximal and closed itemset mining

context("Maximal and closed itemsets")

is_a_freq <- apriori(trans, parameter = list(target = "frequent"))
is_a_max <- apriori(trans, parameter = list(target = "max"))
is_a_closed <- apriori(trans, parameter = list(target = "closed"))
is_a_gen <- apriori(trans, parameter = list(target = "generator"))

is_e_freq <- eclat(trans, parameter = list(target = "frequent"))
is_e_max <- eclat(trans, parameter = list(target = "max"))
is_e_closed <- eclat(trans, parameter = list(target = "closed"))
is_e_gen <- eclat(trans, parameter = list(target = "generator"))

expect_true(setequal(is_a_freq, is_e_freq))
expect_true(setequal(is_a_max, is_e_max))
expect_true(setequal(is_a_closed, is_e_closed))
expect_true(setequal(is_a_gen, is_e_gen))

#inspect(is_a_freq)
#inspect(is_e_freq)
