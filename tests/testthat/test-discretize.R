options(digits=2)

nums1 <- sample(rep(0:10, time = 5))

nums2 <- 
    c(rep(1,7), 
	  rep(2,3),
	  rep(3,4),
	  rep(4,5),
	  rep(5,9),
	  rep(6,10),
	  rep(7,8),
	  rep(8,1),
	  rep(9,9),
	  rep(10,4)
  )


# interval
d <- discretize(nums1, method = "interval", breaks = 2)
expect_equal(attr(d, 'discretized:breaks'), c(0, 5, 10))

d <- discretize(nums2, method = "interval", breaks = 2)
expect_equal(attr(d, 'discretized:breaks'), c(1, 5.5, 10))
expect_equal(as.numeric(table(d)), c(28, 32))

d <- discretize(nums2, method = "interval", breaks = 9)
expect_equal(attr(d, 'discretized:breaks'), as.numeric(1:10))

# fixed
expect_error(discretize(nums2, method = "fixed", breaks = 1)) 
  ### needs at least 2 values for breaks
d <- discretize(nums2, method = "fixed", breaks = c(0,5,10))
expect_equal(length(levels(d)), 2L)
expect_equal(as.numeric(table(d)), c(19, 41))

# frequency
d <- discretize(nums1, method = "frequency", breaks = 2)
expect_equal(length(levels(d)), 2L)
expect_equal(as.numeric(table(d)), c(25, 30))

d <- discretize(nums1, method = "frequency", breaks = 11)
expect_equal(as.numeric(table(d)), rep(5, 11))

d <- discretize(nums2, method = "frequency", breaks = 2)
expect_equal(length(levels(d)), 2L)
expect_equal(as.numeric(table(d)), c(28, 32))

d <- discretize(nums2, method = "frequency", breaks = 6)
expect_equal(as.numeric(table(d)), c(10, 9, 9, 10, 9, 13))

# missing values
nums1[3:5] <- NA
d <- discretize(nums1, method = "interval")
expect_equal(sum(is.na(d)), 3L)
d <- discretize(nums1, method = "frequency")
expect_equal(sum(is.na(d)), 3L)
d <- discretize(nums1, method = "cluster")
expect_equal(sum(is.na(d)), 3L)

