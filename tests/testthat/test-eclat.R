library("arules")
library("testthat")

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

f <- eclat(trans, control = list(verb = verb))

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

# f
# summary(f)
# inspect(f[1:2])
# labels(f[1:2])


### test subset
f.sub <- subset(f, subset = items %in% "a")
l <- labels(f.sub)
expect_equal(l, grep("a", l, value = T))

### test tidlists
f <-
  eclat(trans,
    parameter = list(tidLists = TRUE),
    control = list(verb = verb)
  )

# f
# summary(f)
tl <- tidLists(f)

expect_identical(dim(tl), c(20L, 7L))

# tl
# summary(tl)
# inspect(tl)

expect_equal(as(tl[5], "list"), list("{a,d}" = c("Tr3", "Tr7")))
