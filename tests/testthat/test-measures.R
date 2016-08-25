library("arules")
library("testthat")

context("measures")

options(digits=2)

data <- list(
    c("A", "B"),
    c("A", "B", "C", "G"),
    c("C", "D"),
    c("C", "D"),
    c("E", "F")
    )

trans <- as(data, "transactions")


##################################################################
# Test the original example from
# Edward R. Omiecinski. Alternative interest measures for mining 
# associations in databases. IEEE Transactions on Knowledge and 
# Data Engineering, 15(1):57-69, Jan/Feb 2003.

# complains about low support
expect_warning(
  fsets <- eclat(trans, parameter = list(supp = 0), control=list(verb=FALSE))
  )

# add all-confidence
quality(fsets)$allConfidence <- 
  interestMeasure(fsets, measure = "allConfidence", trans)
#inspect(fsets[order(size(fsets))])

# check
ac <- c(1.00, 0.67, 0.33, 0.33, 0.50, 0.33, 0.33, 0.50, 0.50, 0.33, 0.33, 
  1.00, 0.33, 0.60, 0.40, 0.40, 0.20, 0.40, 0.20, 0.20)
expect_equal(round(quality(fsets)$allConfidence, 2), ac)

###################################################################
## test all measures for itemsets
m1 <- interestMeasure(fsets, transactions = trans)

## now recalculate the measures using the transactions
m2 <- interestMeasure(fsets, transactions = trans, reuse = FALSE)
expect_equal(m1, m2)


###################################################################
# test measures for rules

rules <- apriori(trans, parameter=list(supp=0.01, conf = 0.5), 
  control=list(verb=FALSE))

## calculate all measures (just to see if one creates an error)
m1 <- interestMeasure(rules, transactions = trans)

## coverage
expect_equal(coverage(rules), support(lhs(rules), trans = trans))
expect_equal(coverage(rules, trans = trans, reuse = FALSE), 
  support(lhs(rules), trans = trans))


## is.redundant (this test does not help much)!
context("is.redundant")

red <- is.redundant(rules)
imp <- interestMeasure(rules, measure = "improvement")
expect_equal(red, imp<=0)

#inspect(rules[!red])
#inspect(rules[red])

## FIXME: test others

