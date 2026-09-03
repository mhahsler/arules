data <- transactions(list(
  T1 = c("a", "b"),
  T2 = "a",
  T3 = c("b", "c"),
  T4 = c("a", "b", "c")
))
groups <- c("group1", "group1", "group2")

aggregated <- aggregate(data, groups)

expect_s4_class(aggregated, "transactions")
expect_identical(itemLabels(aggregated), c("group1", "group2"))
expect_identical(
  as(aggregated, "matrix"),
  matrix(
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
    nrow = 4,
    dimnames = list(c("T1", "T2", "T3", "T4"), c("group1", "group2"))
  )
)
expect_identical(transactionInfo(aggregated), transactionInfo(data))

itemInfo(data)$group <- groups
expect_identical(
  as(aggregate(data, "group"), "matrix"),
  as(aggregated, "matrix")
)

sets <- itemsets(
  list("a", "b", c("a", "b"), "c"),
  itemLabels = data,
  quality = data.frame(support = c(.5, .5, .25, .5))
)
aggregated_sets <- aggregate(sets, groups)
expect_s4_class(aggregated_sets, "itemsets")
expect_setequal(labels(aggregated_sets), c("{group1}", "{group2}"))
expect_length(quality(aggregated_sets), 0L)

rules <- rules(
  lhs = list("a", c("a", "b")),
  rhs = list("b", "c"),
  itemLabels = data,
  quality = data.frame(support = c(.5, .25), confidence = c(1, .5))
)
aggregated_rules <- aggregate(rules, groups)
expect_s4_class(aggregated_rules, "rules")
expect_identical(labels(aggregated_rules), c("{} => {group1}", "{group1} => {group2}"))
expect_length(quality(aggregated_rules), 0L)
