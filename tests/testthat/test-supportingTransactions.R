data <- transactions(list(
  T1 = c("a", "b"),
  T2 = "a",
  T3 = c("b", "c"),
  T4 = c("a", "b", "c")
))
sets <- itemsets(
  list("a", c("a", "b"), "c"),
  itemLabels = data
)

supporting <- supportingTransactions(sets, data)

expect_s4_class(supporting, "tidLists")
expect_identical(
  as(supporting, "list"),
  list(
    `{a}` = c("T1", "T2", "T4"),
    `{a,b}` = c("T1", "T4"),
    `{c}` = c("T3", "T4")
  )
)
expect_identical(itemLabels(supporting), labels(sets))
expect_identical(transactionInfo(supporting), transactionInfo(data))
expect_equal(unname(size(supporting)) / length(data), support(sets, data))

rules <- rules(
  lhs = list("a", "b"),
  rhs = list("b", "c"),
  itemLabels = data
)
expect_identical(
  unname(as(supportingTransactions(rules, data), "list")),
  list(c("T1", "T4"), c("T3", "T4"))
)
