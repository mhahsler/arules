setwd(tempdir())

## test skip and quote
data <- paste(
  "# this is some test data",
  "\"item 1\" item2",
  "'item 1'",
  "item2 item3",
  sep = "\n"
)
# cat(data)
write(data, file = "demo_basket")

## read demo data (skip comment line)
tr <- read.transactions("demo_basket", format = "basket", sep = "", skip = 1)
# inspect(tr)
expect_equal(dim(tr), c(3L, 3L))

## try to write transactions that have no transactionID
write(tr, file = "demo_single", format = "single")
write(tr, file = "demo_basket", format = "basket")

## tidy up
unlink(c("demo_basket", "demo_single"))

## create a demo file using single format for the example
## column 1 contains the transaction ID and column 2 contains one item
data <- paste(
  "some comment goes here",
  "trans1 stuff \"item 1\"",
  "trans2 that 'item 1'",
  "trans2 is_not_important item2",
  sep = "\n"
)
# cat(data)
write(data, file = "demo_single")

## read demo data columns 1 and 3 + skip first line
tr <- read.transactions("demo_single",
  format = "single",
  cols = c(1, 3), skip = 1
)
# inspect(tr)
expect_equal(dim(tr), c(2L, 2L))

## use single with col headers
data <- paste(
  "trans_id filler item_id",
  "trans1 stuff \"item 1\"",
  "trans2 that 'item 1'",
  "trans2 is_not_important item2",
  sep = "\n"
)
# cat(data)
write(data, file = "demo_single")

## read demo data columns 1 and 3 + skip first line
tr <- read.transactions("demo_single",
  format = "single",
  cols = c("trans_id", "item_id"), header = TRUE
)
# inspect(tr)
expect_equal(dim(tr), c(2L, 2L))

## tidy up
unlink("demo_single")

## Write transactions
## Note: write basket looses transactionID
write(tr, file = "demo_write")
tr2 <- read.transactions("demo_write")
# inspect(tr)
# inspect(tr2)

## NOTE: write basket loses transactionID
transactionInfo(tr2) <- transactionInfo(tr)
itemsetInfo(tr2) <- itemsetInfo(tr)
expect_equal(tr, tr2)

write(tr, file = "demo_write", format = "single")
tr2 <- read.transactions("demo_write", format = "single", cols = c(1, 2))
# inspect(tr2)
expect_equal(tr, tr2)

unlink("demo_write")
