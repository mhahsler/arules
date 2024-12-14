verb <- FALSE
options(digits = 2)

data <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("b", "e"),
  c("a", "c"),
  c("c", "e"),
  c("a", "b", "d", "e")
)
names(data) <- paste("Tr", c(1:7), sep = "")
trans <- transactions(data)

is_a_freq <- apriori(trans, parameter = list(target = "frequent"), control = list(verb = verb))
is_a_max <- apriori(trans, parameter = list(target = "max"), control = list(verb = verb))
is_a_closed <- apriori(trans, parameter = list(target = "closed"), control = list(verb = verb))
is_a_gen <- apriori(trans, parameter = list(target = "generator"), control = list(verb = verb))

is_e_freq <- eclat(trans, parameter = list(target = "frequent"), control = list(verb = verb))
is_e_max <- eclat(trans, parameter = list(target = "max"), control = list(verb = verb))
is_e_closed <- eclat(trans, parameter = list(target = "closed"), control = list(verb = verb))
is_e_gen <- eclat(trans, parameter = list(target = "generator"), control = list(verb = verb))

expect_true(setequal(is_a_freq, is_e_freq))
expect_true(setequal(is_a_max, is_e_max))
expect_true(setequal(is_a_closed, is_e_closed))
expect_true(setequal(is_a_gen, is_e_gen))

# inspect(is_a_freq)
# inspect(is_e_freq)
