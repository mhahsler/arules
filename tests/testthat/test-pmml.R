test_that("PMML import and export", {
skip_if_not_installed("pmml")
skip_if_not_installed("XML")

data <- transactions(list(
  c("a", "b"),
  "a",
  c("b", "c"),
  c("a", "b", "c")
))

rules <- apriori(
  data,
  parameter = list(support = .25, confidence = .5, minlen = 2),
  control = list(verbose = FALSE)
)
rules <- head(rules, 3)

rules_file <- tempfile(fileext = ".xml")
on.exit(unlink(rules_file), add = TRUE)
suppressWarnings(write.PMML(rules, rules_file))
rules_copy <- read.PMML(rules_file)

expect_s4_class(rules_copy, "rules")
expect_identical(labels(rules_copy), labels(rules))
expect_equal(
  quality(rules_copy)[, c("support", "confidence", "lift")],
  quality(rules)[, c("support", "confidence", "lift")],
  ignore_attr = TRUE
)

sets <- eclat(
  data,
  parameter = list(support = .25, minlen = 1),
  control = list(verbose = FALSE)
)
sets <- head(sets, 3)

sets_file <- tempfile(fileext = ".xml")
on.exit(unlink(sets_file), add = TRUE)
suppressWarnings(write.PMML(sets, sets_file))
sets_copy <- read.PMML(sets_file)

expect_s4_class(sets_copy, "itemsets")
expect_identical(labels(sets_copy), labels(sets))
expect_equal(quality(sets_copy)$support, quality(sets)$support)

invalid_file <- tempfile(fileext = ".xml")
on.exit(unlink(invalid_file), add = TRUE)
writeLines("<PMML></PMML>", invalid_file)
expect_error(read.PMML(invalid_file), "does not contain an AssociationModel", fixed = TRUE)
})
