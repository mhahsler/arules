library("arules")
library("testthat")

context("confint")

data("Adult")
## Mine association rules.
rules <- apriori(Adult,
  parameter = list(
    supp = 0.5,
    conf = 0.9,
    target = "rules"
  ))

measures <- c(
  "count",
  "confidence",
  "confidence_normal",
  "lift",
  "oddsRatio",
  "oddsRatio_normal",
  "support",
  "support_normal"
)

for (m in measures) {
  ci <- cbind(measure = interestMeasure(rules, gsub("\\_\\w+", "", m)), confint(rules, m, smoothCounts = 0))
  expect_false(any(ci[,1] < ci[,2] | ci[,1] > ci[,3] | ci[,2] > ci[,3], na.rm = TRUE), info = m)
}

# exact intervals should be tighter
#confint(rules, "oddsRatio_normal") - confint(rules, "oddsRatio")
#confint(rules, "support_normal") - confint(rules, "support")
#confint(rules, "confidence_normal") - confint(rules, "confidence")
#ci <- confint(rules, "confidence")
#ci <- confint(rules, "support")
#ci[,2] - ci[,1]

