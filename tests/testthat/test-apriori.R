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

### rules (all warnings are for low support)
r <- apriori(trans,
  parameter = list(supp = 0.25, conf = 0),
  control = list(verb = verb))

expect_length(r, 18L)

#r
#summary(r)
#inspect(sort(r, by = "lift")[1:7])

### test appearance
r <- apriori(
  trans,
  parameter = list(supp = 0.25, conf = 0),
  appearance = list(rhs = c("a", "b")),
  control = list(verb = verb)
)

expect_length(r, 6L)
#inspect(r)

r <- apriori(
  trans,
  parameter = list(supp = 0.25, conf = 0),
  appearance = list(lhs = c("a", "b"), rhs = "c"),
  control = list(verb = verb)
)
expect_length(r, 2L)

r <- apriori(
  trans,
  parameter = list(supp = 0.25, conf = 0),
  appearance = list(none = c("a", "b")),
  control = list(verb = verb)
)
expect_length(r, 3L)

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
  control = list(verb = verb)
)

expect_true("coverage" %in% colnames(quality(r)))
#inspect(r[1:2])

## Compare if APRIOR and ECLAT produce the same results
data("Income")
fsets <-
  apriori(
    Income,
    parameter = list(target = "frequ", supp = 0.2),
    control = list(verb = verb)
  )
esets <-
  eclat(
    Income,
    parameter = list(target = "frequ", supp = 0.2),
    control = list(verb = verb)
  )

## compare if output is the same
expect_true(all(table(match(fsets, esets)) == 1))

