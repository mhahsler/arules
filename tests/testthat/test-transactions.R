data <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("b", "e"),
  c("a", "d"),
  c("d", "e"),
  c("d", "f"),
  c("a", "b", "d", "e", "f", "g")
)
names(data) <- paste("Tr", c(1:8), sep = "")

##################################################
### test transactions

trans <- as(data, "transactions")
# trans
# summary(trans)
# inspect(trans[1:2])

expect_identical(size(trans), unname(sapply(data, length)))
expect_identical(data, as(trans, "list"))
expect_identical(transactionInfo(trans)$transactionID, names(data))
expect_identical(sort(itemInfo(trans)$labels), sort(unique(unique(unlist(data)))))

## test constructor
expect_identical(transactions(data), trans)

## combine
expect_equal(c(trans, trans), as(c(data, data), "transactions"))

m <- as(trans, "matrix")
# m
expect_identical(data, as(as(m, "transactions"), "list"))
expect_identical(dim(m), dim(trans))
expect_identical(nrow(m), length(trans))
expect_identical(dimnames(m), dimnames(trans))

expect_equal(c(trans, trans), as(rbind(m, m), "transactions"))

## combine with missing items (needs recoding)
expect_warning(expect_true(all(as(c(trans[, -2], trans[, -3]), "matrix")[1:8, "b"]) == FALSE))
expect_warning(expect_true(all(as(c(trans[, -2], trans[, -3]), "matrix")[9:15, "c"]) == FALSE))

l <- LIST(trans, decode = FALSE)
expect_identical(length(l), nrow(trans))
expect_identical(as(trans, "ngCMatrix")@i + 1L, unlist(l))

## test creating transactions in long format
a_df3 <- data.frame(
  TID = c(1, 1, 2, 2, 2, 3),
  item = factor(c("a", "b", "a", "b", "c", "b"))
)
a_df3
tr <- transactions(a_df3, format = "long", cols = c("TID", "item"))
tr
expect_equal(dim(tr), c(3L, 3L))
expect_equal(LIST(tr), list(`1` = c("a", "b"), `2` = c("a", "b", "c"), `3` = "b"))

lf <- toLongFormat(tr)
lf
expect_equal(unname(lf), unname(a_df3))

###########################################################################
### compare transactions with items b, c, d

t <- as(data, "transactions")[, 2:4]
t_comp <- as(m[, 2:4], "transactions")

## NOTE: rownames in itemInfo do not agree due to subsetting!
rownames(t@itemInfo) <- NULL
rownames(t_comp@itemInfo) <- NULL
expect_identical(t, t_comp)

expect_identical(as(t, "ngCMatrix"), as(t_comp, "ngCMatrix"))

## addComplement
data("Groceries")

## add a complement-items for "whole milk" and "other vegetables"
g2 <- addComplement(Groceries, c("whole milk", "other vegetables"))
g2 <- addComplement(g2, "coffee", "NO coffee")

expect_equal(nitems(g2), nitems(Groceries) + 3L)
expect_identical(
  as.logical(as(g2[, "!whole milk"], "matrix")),
  !as.logical(as(g2[, "whole milk"], "matrix"))
)


itemInfo(g2) <- itemInfo(g2)[, !colnames(itemInfo(g2)) %in% c("variables", "levels")]
expect_identical(g2[, 1:nitems(Groceries)], Groceries)
