library("testthat")
library("arules")

context("itemCoding")

data("Adult")

list <- LIST(Adult[1:5], decode = FALSE)
list_decoded1 <- decode(list, itemLabels = itemLabels(Adult))
list_decoded2 <- LIST(Adult[1:5])

### no list element names for decode = FALSE
names(list_decoded2) <- NULL

expect_equal(list_decoded1, list_decoded2)


## Example 2: Manually create an itemMatrix 
data <- list(
  c("income=small", "age=Young"),
  c("income=large", "age=Middle-aged")
)

iM <- encode(data, itemLabels = Adult)


### non existing item are dropped with a warning now.
data2 <- list(
  c("income=small", "age=Young"),
  c("income=large", "not_an_item")
)

expect_warning(iM <- encode(data2, itemLabels = Adult))
expect_identical(size(iM), c(2L, 1L))

### test encoding
iM <- encode(list(c(1,2,3), c(4,5)), itemLabels(Adult))
expect_identical(size(iM), c(3L, 2L))

expect_error(encode(list(c(1,2,3), c(4,5.5)), itemLabels(Adult)))
expect_error(encode(list(c(1,2,3), c(4,5, nitems(Adult)+1L)), itemLabels(Adult)))


