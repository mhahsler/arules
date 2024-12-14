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

# smooth = .5 is Haldane-Anscombe correction

for (m in measures) {
  if (debug) cat("CI for", m, "\n")
  ci <- cbind(measure = interestMeasure(rules, m, smooth = .5), confint(rules, m))
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
