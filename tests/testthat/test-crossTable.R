data(Groceries)

itset <- new("itemsets",
  items = encode(c("whole milk", "soda"), itemLabels = Groceries)
)

supp <- support(itset, Groceries, type = "absolute")

expect_equal(
  crossTable(Groceries, measure = "count")["whole milk", "soda"],
  supp
)

expect_equal(
  crossTable(Groceries, measure = "support")["whole milk", "soda"],
  supp / length(Groceries)
)

expect_equal(
  crossTable(Groceries, measure = "lift")["whole milk", "soda"],
  supp / length(Groceries) / prod(itemFrequency(Groceries)[c("whole milk", "soda")])
)
