library("testthat")
library("arules")


data <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("b","e"),
  c("a","c"),
  c("c","e"),
  c("a","b","d","e")
)
names(data) <- paste("Tr",c(1:7), sep = "")
trans <- as(data, "transactions")

context("Rule Induction")

### rules (all warnings are for low support)
expect_warning(
  is <- apriori(trans, parameter=list(supp=0.25, target = "frequent"), 
    control=list(verb=FALSE)),
  regex = "low absolute support"
)
# inspect(is)

## without transactions
r1 <- ruleInduction(is)
# inspect(r1)
#   lhs      rhs support   confidence lift
#3  {d}   => {b} 0.2857143 1.0        1.40
#5  {d}   => {a} 0.2857143 1.0        1.40
#9  {b}   => {a} 0.5714286 0.8        1.12
#10 {a}   => {b} 0.5714286 0.8        1.12
#11 {b,d} => {a} 0.2857143 1.0        1.40
#12 {a,d} => {b} 0.2857143 1.0        1.40

expect_equal(length(r1), 6L)
expect_true(all(quality(r1)$support >= .25))

r2 <- ruleInduction(is, confidence = 1)
# inspect(r2)
#  lhs      rhs support   confidence lift
#3  {d}   => {b} 0.2857143 1          1.4 
#5  {d}   => {a} 0.2857143 1          1.4 
#11 {b,d} => {a} 0.2857143 1          1.4 
#12 {a,d} => {b} 0.2857143 1          1.4 

expect_equal(length(r2), 4L)
expect_true(all(quality(r2)$confidence == 1))

## missing itemsets
is_incomplete <- is[labels(is) != "{a,b}"]
expect_error(r_incomplete <- ruleInduction(is_incomplete), 
  regex = "cannot induce rules")

## missing support
is_nosupp <- is

# empty quality
quality(is_nosupp) <- data.frame()
expect_error(r_nosupp <- ruleInduction(is_nosupp), regex = "support is missing")

# no support
quality(is_nosupp) <- data.frame(weird_measure = runif(length(is_nosupp))) 
# inspect(is_nosupp)
expect_error(r_nosupp <- ruleInduction(is_nosupp), regex = "support is missing")


expect_equal_rules <- function(r1, r2) {
  expect_equal(length(r1), length(r2))
  r2 <- r2[match(labels(r2), labels(r1))]
  expect_equal(labels(r1), labels(r1))
  ### Note rownames may differ in quality after matching
  q1 <- quality(r1)[, c("support", "confidence", "lift")]
  rownames(q1) <- NULL
  q2 <- quality(r2)[, c("support", "confidence", "lift")]
  rownames(q2) <- NULL
  expect_equal(q1, q2) 
}

## with transactions
r1_w <- ruleInduction(is, transactions = trans)
expect_equal_rules(r1, r1_w)

r2_w <- ruleInduction(is, transactions = trans, confidence =1)
expect_equal_rules(r2, r2_w)

r2_incomplete <- ruleInduction(is_incomplete, transactions = trans)
#> inspect(r2_incomplete)
#   lhs      rhs support   confidence lift itemset
#3  {d}   => {b} 0.2857143 1          1.4   7     
#5  {d}   => {a} 0.2857143 1          1.4   8     
#9  {b,d} => {a} 0.2857143 1          1.4  10     
#10 {a,d} => {b} 0.2857143 1          1.4  10  
expect_equal(length(r2_incomplete), 4)


r2_nosupp <- ruleInduction(is_nosupp, transactions = trans)
expect_equal_rules(r2_nosupp, r1)


## test method apriori and compare to ptree method (default)
## they all need specified transactions
expect_warning(
  r1_a <- ruleInduction(is, transactions = trans, 
    control=list(method="apriori")),
  regex = "low absolute support"
)
expect_equal_rules(r1_a, r1)

expect_warning(
  r2_a <- ruleInduction(is, transactions = trans, 
    control=list(method="apriori"), confidence = 1),
  regex = "low absolute support"
)
expect_equal_rules(r2_a, r2)

# test tidlists
# FIXME: tidlists does not work correctly!
# r1_t <- ruleInduction(is, transactions = trans, 
#     control=list(method="tidlists"))
# expect_equal_rules(r1_t, r1)
# 
# r2_t <- ruleInduction(is, transactions = trans, 
#     control=list(method="tidlists"), confidence = 1)
# expect_equal_rules(r2_t, r2)

## test with problematic transactions (items have support of 0)
r_t0 <- ruleInduction(is, transactions = trans[0])
expect_equal(length(r_t0), 0L)

r_t2 <- ruleInduction(is, transactions = trans[1:2])
#> inspect(trans[1:2])
#  items   transactionID
#1 {a,b,c} Tr1          
#2 {a,b}   Tr2 

#> inspect(r_t2)
#  lhs    rhs support confidence lift itemset
#1  {c} => {a} 0.5     1          1     6     
#9  {b} => {a} 1.0     1          1    10     
#10 {a} => {b} 1.0     1          1    10   
expect_equal(length(r_t2), 3L)

# method apriori
r_t0a <- ruleInduction(is, transactions = trans[0], control=list(method="apriori"))
expect_equal(length(r_t0a), 0L)

expect_warning(
  r_t2a <- ruleInduction(is, transactions = trans[1:2], 
    control=list(method="apriori")),
  regex = "low absolute support"
)

## FIXME: apriori returns rules like {} -> rule induction does not!
expect_equal_rules(r_t2, r_t2a[size(r_t2a)>1])

## test with problematic transactions (items missing, items in different order)
expect_error(ruleInduction(is, transactions = trans[,rev(1:nitems(trans))]), 
  regex = "Item labels")

expect_error(ruleInduction(is, transactions = trans[,-2]), 
  regex = "Dimensions")
