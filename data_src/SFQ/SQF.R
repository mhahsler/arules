library("arules")

load("SFQ_clean.rda")


d <- dat[, c(
  grep("rf_", colnames(dat), value = TRUE),
  grep("cs_", colnames(dat), value = TRUE),
  grep("ac_", colnames(dat), value = TRUE),
  grep("pf_", colnames(dat), value = TRUE),
  "arstmade", "sumissue", "detailcm", "race",
  "pct",
  #"city", ### city and precinct are related
  "typeofid", "othpers"
)]

d$female <- dat$sex == "female"
#d$detailcm[!(d$arstmade | d$sumissue)] <- NA
d$weapon <- dat$pistol | dat$riflshot | dat$asltweap |
  dat$knifcuti | dat$machgun | dat$othrweap
d$no_uniform <- !dat$offunif

d$inside <- dat$inout == "inside"
d$trhsloc <- dat$trhsloc
d$trhsloc[dat$trhsloc == "neither"] <- NA

d$minor <- dat$age<18
d$height <- discretize(dat$height, method = "frequency", 3)

SQF <- as(d, "transactions")
SQF

save(SQF, file = "SQF.rda")

