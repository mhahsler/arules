test_that("confidence intervals and significance", {
debug <- FALSE

data("Adult")
## Mine association rules.
rules <- apriori(Adult,
  parameter = list(
    supp = 0.5,
    conf = 0.9,
    target = "rules"
  ), control = list(verb = FALSE)
)

measures <- c(
  "count",
  "confidence",
  "lift",
  "oddsRatio",
  "phi",
  "support"
)

# smoothCounts = .5 is the Haldane-Anscombe correction.

for (m in measures) {
  if (debug) cat("CI for", m, "\n")
  ci <- cbind(
    measure = interestMeasure(rules, m, smoothCounts = .5),
    confint(rules, m, smoothCounts = .5)
  )
  if (debug) print(ci)
  expect_false(any(ci[, 1] < ci[, 2] | ci[, 1] > ci[, 3] | ci[, 2] > ci[, 3], na.rm = TRUE),
    info = m
  )
}

# exact intervals should be tighter
# confint(rules, "oddsRatio_normal") - confint(rules, "oddsRatio")
# confint(rules, "support_normal") - confint(rules, "support")
# confint(rules, "confidence_normal") - confint(rules, "confidence")
# ci <- confint(rules, "confidence")
# ci <- confint(rules, "support")
# ci[,2] - ci[,1]


# is.significant
s <- is.significant(rules, method = "Fisher", alpha = 0.05, adjust = "none")
s2 <- interestMeasure(rules, "Fisher") <= 0.05

expect_equal(s, s2)

# Approximate intervals apply the pseudocount exactly once to every cell.
raw_counts <- as.data.frame(arules:::.getCounts(rules, smoothCounts = 0))
smoothed_ci <- confint(
  rules,
  "confidence",
  method = "normal",
  smoothCounts = .5
)
expected_ci <- arules:::ci.prop(
  raw_counts$n11 + .5,
  raw_counts$n1x + 1,
  level = .95
)
expect_equal(as.vector(smoothed_ci), as.vector(expected_ci))
expect_false(isTRUE(all.equal(
  as.vector(smoothed_ci),
  as.vector(confint(rules, "confidence", method = "normal"))
)))

# Bootstrap intervals smooth both the estimated multinomial probabilities and
# each simulated contingency table exactly once.
one_rule <- rules[1]
one_count <- as.data.frame(arules:::.getCounts(one_rule, smoothCounts = 0))
cells <- c("n11", "n10", "n01", "n00")
smoothed_count <- one_count[, cells] + .5
bootstrap_n <- sum(smoothed_count)
bootstrap_p <- smoothed_count / bootstrap_n

set.seed(123)
bootstrap_ci <- confint(
  one_rule,
  "oddsRatio",
  method = "bootstrap",
  smoothCounts = .5,
  replications = 200
)
set.seed(123)
simulated_counts <- t(stats::rmultinom(200, bootstrap_n, bootstrap_p))
expected_or <- (simulated_counts[, 1] + .5) *
  (simulated_counts[, 4] + .5) /
  ((simulated_counts[, 2] + .5) * (simulated_counts[, 3] + .5))
expect_equal(
  as.vector(bootstrap_ci),
  as.vector(stats::quantile(expected_or, c(.025, .975)))
)

# Smoothing keeps bootstrap odds-ratio intervals finite for a zero cell.
small_transactions <- transactions(list(
  c("A", "B"), c("A", "B"), "A", "C"
))
small_rules <- apriori(
  small_transactions,
  parameter = list(support = 0, confidence = 0, minlen = 2),
  control = list(verbose = FALSE)
)
zero_cell_rule <- small_rules[labels(small_rules) == "{A} => {B}"]
set.seed(321)
zero_cell_ci <- confint(
  zero_cell_rule,
  "oddsRatio",
  method = "bootstrap",
  smoothCounts = .5,
  replications = 200
)
expect_true(all(is.finite(zero_cell_ci)))

count_matrix <- as.matrix(one_count[, cells])
expect_error(
  arules:::.basicRuleMeasure(
    count_matrix,
    "confidence",
    smoothCounts = -.5
  ),
  "smoothCounts >= 0"
)
})
