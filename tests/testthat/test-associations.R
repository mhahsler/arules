  library("testthat")
  library("arules")
  
  context("associations")
  
  set.seed(20070611)
  
  m <- matrix(as.integer(runif(100000)>0.8), ncol=20)
  dimnames(m) <- list(NULL,paste("item",c(1:20),sep=""))
  t <- as(m, "transactions")
  #t
  #inspect(t[10])
  
  expect_identical(dim(t), dim(m))
  
  
  r <- apriori(t, parameter=list(supp=0.01,conf=0.1),control=list(verb=FALSE))
  #r
  #summary(r)
  #inspect(r)
  
  ss <- subset(r, subset=lift>1.4 & lhs %in% "item3")
  #inspect(ss)
  expect_identical(labels(lhs(ss)), "{item2,item3}")
  expect_true(quality(ss)$lift>1.4)
  
  
  f <- eclat(t,parameter=list(supp=0.01),control=list(verb=FALSE))
  #f
  #summary(f)
  #inspect(f)
  
  ss <- subset(f, subset = items %in% "item7")
  #inspect(ss)
  expect_identical(labels(ss), grep("item7", labels(ss), value = TRUE))
  
  
  ### create associations manually
  lmat <- matrix(rbind(c(1,1,0), c(0,0,1)), ncol=3)
  rmat <- matrix(rbind(c(1,0,0), c(0,1,0)), ncol=3)
  colnames(lmat) <- c("a", "b", "c")
  colnames(rmat) <- c("c", "a", "b")
  # Note: the column names do not agree!
  
  lhs <- as(lmat, "itemMatrix")
  rhs <- as(rmat, "itemMatrix")
  
  is <- new("itemsets", items=lhs, quality=data.frame(support=c(.1,.1)))
  #inspect(is)
  
  expect_equal(labels(is), c("{a,b}", "{c}"))
  
  qual <- data.frame(support=c(.5,.5), confidence=c(.5,.5), lift=c(2,1))
  
  ## warning because of the disagreeing labels
  expect_warning(
    r <- new("rules", lhs=lhs, rhs=rhs, quality=qual)
  )
  
  #inspect(r)
  
  context("subsetting")
  
  ## subsetting (also tests itemMatrix)
  # numeric
  take_r <- sample(nrow(t), 10) 
  take_c <- sample(ncol(t), 10) 
  expect_equal(dim(sub_n <- t[take_r, take_c]), c(10L, 10L))
  expect_equal(dim(t[take_r]), c(10L, ncol(t)))
  expect_equal(dim(t[take_r,]), c(10L, ncol(t)))
  expect_equal(dim(t[,take_c]), c(nrow(t), 10L))
  
  # logical
  take_rb <- rep(FALSE, nrow(t))
  take_rb[take_r] <- TRUE
  take_cb <- rep(FALSE, ncol(t))
  take_cb[take_c] <- TRUE
  expect_equal(dim(sub_b <- t[take_rb, take_cb]), c(10L, 10L))
  expect_equal(dim(t[take_rb,]), c(10L, ncol(t)))
  expect_equal(dim(t[take_rb]), c(10L, ncol(t)))
  expect_equal(dim(t[,take_cb]), c(nrow(t), 10L))
  
  # Note: transactions and itemLabels are mixed up in numeric subset! 
  expect_equal(sub_n[match(sub_b, sub_n), 
    match(itemLabels(sub_b), itemLabels(sub_n))], sub_b)
  
  # character
  take_cc <- itemLabels(t)[take_c]
  expect_equal(dim(t[,take_cc]), c(nrow(t), 10L))
  
  # NA
  expect_warning(expect_equal(dim(t[NA,NA]), c(0L,0L)))
  expect_warning(expect_equal(dim(t[NA]), c(0L,ncol(t))))
  expect_warning(expect_equal(dim(t[,NA]), c(nrow(t), 0L)))
  
  take_rn <- take_r
  take_rn[3:4] <- NA
  take_cn <- take_c
  take_cn[3:4] <- NA
  expect_warning(expect_equal(dim(t[take_rn,take_cn]), c(8L,8L)))
  
  take_rbn <- take_rb
  take_rbn[which(take_rbn)[3:4]] <- NA
  take_cbn <- take_cb
  take_cbn[which(take_cbn)[3:4]] <- NA
  expect_warning(expect_equal(dim(t[take_rbn,take_cbn]), c(8L,8L)))
  
  take_ccn <- take_cc
  take_ccn[3:4] <- NA
  expect_warning(expect_equal(dim(t[,take_cbn]), c(nrow(t),8L)))
  
  # rules
  r <- apriori(t, parameter=list(supp=0.01,conf=0.1),control=list(verb=FALSE))
  expect_warning(expect_equal(length(r[NA]), 0L))
  expect_warning(expect_equal(length(r[c(1L, NA_integer_)]), 1L))
  expect_warning(expect_equal(length(r[c(TRUE, NA, FALSE)]),
    sum(rep(c(TRUE,NA,FALSE), length.out=length(r)), na.rm = TRUE))) # recycle
  
  # itemsets
  f <- eclat(t,parameter=list(supp=0.01),control=list(verb=FALSE))
  expect_warning(expect_equal(length(f[NA]), 0L))
  expect_warning(expect_equal(length(f[c(1L, NA_integer_)]), 1L))
  expect_warning(expect_equal(length(f[c(TRUE, NA, FALSE)]),
    sum(rep(c(TRUE,NA,FALSE), length.out=length(f)), na.rm = TRUE))) # recycle
    
  # head
  expect_identical(r[1:5], head(r, n = 5))
  expect_identical(r[1:6], head(r))
  expect_identical(r[0], head(r, n = 0))
  expect_identical(r[1:(length(r)-10)], head(r, n = -10))
  expect_identical(r[1:length(r)], head(r, n = length(r)))
  expect_identical(r[1:length(r)], head(r, n = length(r)+100L))
  expect_identical(sort(r, by = "lift")[1:5], head(r, n = 5, by = "lift"))
  expect_identical(sort(r, by = "lift", decreasing = FALSE)[1:5], 
    head(r, n = 5, by = "lift", decreasing = FALSE))
  
  
  # match and %in%
  expect_identical(match(r[2:10], r), 2:10)
  expect_identical(r[2:10] %in% r, 2:10)
  expect_identical(match(f[2:10], f), 2:10)
  expect_identical(f[2:10] %in% f, 2:10)
  