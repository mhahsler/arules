################################################################
### read data 
   

read.Income <- function(file = "Income.data") {
  x <- read.table(file)

  names(x) <- c("income", "sex", "marital status", "age", "education",
      "occupation", "years in bay area", "dual incomes",
      "number in household", "number of children",
      "householder status", "type of home",
      "ethnic classification", "language in home")

  x$income <- ordered(x$income, levels = 1 : 9,
      labels = c("[0,10)", "[10,15)", "[15,20)",
	"[20,25)", "[25,30)", "[30,40)", "[40,50)",
	"[50,75)","75+"))
  x$sex <- factor(x$sex, levels = 1 : 2, labels = c("male", "female"))
  x$"marital status" <- factor(x$"marital status", levels = 1 : 5,
      labels = c("married", "cohabitation",
	"divorced", "widowed", "single"))
  x$age <- ordered(x$age, levels = 1 : 7,
      labels = c("14-17", "18-24", "25-34", "35-44",
	"45-54", "55-64", "65+"))
  x$education <- ordered(x$education, levels = 1 : 6,
      labels = c("grade <9", "grades 9-11",
	"high school graduate", "college (1-3 years)",
	"college graduate", "graduate study"))
  x$occupation <- factor(x$occupation, levels = 1 : 9,
      labels = c("professional/managerial", "sales",
	"laborer",
	"clerical/service", "homemaker",
	"student", "military", "retired",
	"unemployed"))
  x$"years in bay area" <- ordered(x$"years in bay area", levels = 1 : 5,
      labels = c("<1", "1-3", "4-6", "7-10", ">10"))
  x$"dual incomes" <- factor(x$"dual incomes", levels = 1 : 3,
      labels = c("not married", "yes", "no"))
  x$"number in household" <- ordered(x$"number in household", 
      levels = 1 : 9, labels = c(1 : 8, "9+"))
  
  ### on page 446 HTF state that number of children has 9 values!!!
  x$"number of children" <- ordered(x$"number of children", 
      levels = 0 : 9, labels = c(0 : 8, "9+"))
  x$"householder status" <- factor(x$"householder status", levels = 1 : 3,
      labels = c("own", "rent", "live with parents/family"))
  x$"type of home" <- factor(x$"type of home", levels = 1 : 5,
      labels = c("house", "condominium", "apartment",
	"mobile Home", "other"))
  x$"ethnic classification" <- factor(x$"ethnic classification", 
      levels = 1 : 8,
      labels = c("american indian", "asian", "black",
	"east indian", "hispanic", "pacific islander",
	"white", "other"))
  x$"language in home" <- factor(x$"language in home", levels = 1 : 3,
      labels = c("english", "spanish", "other"))

  x
}

IncomeESL <- read.Income()
save(IncomeESL, file = "IncomeESL.rda", compress = TRUE)



recode.Income <- function(x) { 

x <- x[complete.cases(x), ]
y <- x

cat("\nComplete cases:")
print(dim(y))

### Note: We cannot compute the median as in the book. We do not
### have the continuous data!

### get median values
cat("Median of ordered attributes:\n")
ordered_attributes <- which(sapply(x, is.ordered))
median_attribute <- sapply(ordered_attributes, function(i) { 
  median(as.numeric(x[[i]]), na.rm = TRUE) })
print(median_attribute)
median_level <- sapply(1 : length(median_attribute),  function(i) {
  levels(x[[names(median_attribute[i])]])[median_attribute[i]]})
print(median_level)


### recode income (<$40,000 and >=$40,000 stated on page 447)
y[["income"]] <- factor((as.numeric(x[["income"]]) > 6) +1,
	  levels = 1 : 2 , labels = c("$0-$40,000", "$40,000+"))

### recode age (14-34 and 35+)
y[["age"]] <- factor((as.numeric(x[["age"]]) > 3) +1,
    levels = 1 : 2 , labels = c("14-34", "35+"))

### recode education (>college (1-3 years))
y[["education"]] <- factor((as.numeric(x[["education"]]) > 4) +1,
    levels = 1 : 2 , labels = c("no college graduate", "college graduate"))

### recode years in bay area (>10)
y[["years in bay area"]] <- factor((as.numeric(x[["years in bay area"]]) > 4) +1,
    levels = 1 : 2 , labels = c("1-9", "10+"))

### recode number in household (1 used in example 1 on page 446)
### using the median of 3 would be inconsistent with the example
y[["number in household"]] <- factor((as.numeric(x[["number in household"]]) > 3) +1,
    levels = 1 : 2 , labels = c("1", "2+"))

### recode number of children (0 used in example 1 on page 446)
y[["number of children"]] <- factor((as.numeric(x[["number of children"]]) > 1) +0,
    levels = 0 : 1 , labels = c("0", "1+"))

y
}

library("arules")

Income_recoded <- recode.Income(IncomeESL)
Income <- as(Income_recoded, "transactions")

cat("\nNow we have:\n")
print(Income)

save(Income, file = "Income.rda", compress = TRUE)


#plot (cf. plot on page 445)
#Income_tidl <- as(Income ,"tidLists")
#barplot(size(Income_tidl)/length(Income), 
#	names.arg=labels(Income_tidl)$items)
