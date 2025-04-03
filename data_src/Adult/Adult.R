################################################################
### read data 
   

read.Adult <- function() {
  x <- read.table("adult.data", sep=",", strip.white = TRUE, na.strings = "?")
  ### 2nd part (1 line is a comment)
  y <- read.table("adult.test", sep=",", strip.white = TRUE, na.strings = "?", 
  	skip=1)

  x <- rbind(x, y) 

    
    #age: cont
    
    # workclass: cont
    
    # fnlwgt: cont
    
   
   # education
    x$V4 <- ordered(x$V4, #levels = 1 : 16,
                    #labels = c(
                    levels = c(
		    "Preschool", "1st-4th", "5th-6th", 
		    "7th-8th", "9th", "10th", "11th", "12th",
		    "HS-grad", "Prof-school", "Assoc-acdm", "Assoc-voc",
		    "Some-college", "Bachelors", "Masters", "Doctorate"
		   )) 
		    
   # education num: cont
   
   # marital-status
   x$V6 <- factor(x$V6)
   
   # occupation
   x$V7 <- factor(x$V7)
    
   # relationship 
   x$V8 <- factor(x$V8)

   # race
   x$V9 <- factor(x$V9)
    
   # sex 
   x$V10 <- factor(x$V10)
    
   # capital-gain: cont
    
   # capital-loss: cont
   
   # hours-per-week: cont

   # native-country
   x$V14 <- factor(x$V14)
   
   # income 
   #x$V15 <- ordered(x$V15, levels = c("<=50K", ">50K"))
   x$V15 <- ordered(x$V15, levels = c("<=50K", ">50K"), 
   	labels = c("small", "large"))
     
    names(x) <- c("age", "workclass", "fnlwgt", "education", "education-num",
                  "marital-status", "occupation", "relationship",
                  "race", "sex",
                  "capital-gain", "capital-loss",
                  "hours-per-week", "native-country", "income")
    
    
    x
}

AdultUCI <- read.Adult()
save(AdultUCI, file = "AdultUCI.rda", compress = TRUE)


### simple recoding
cut_median <- function(x, labels = c("low", "high")) {
  ordered(cut(x, quantile(x,  probs= c(0,0.5,1))), labels = labels)
}

recode.Adult_simple <- function(x) { 
  recode <- c("age", "education-num", "hours-per-week")

### cut at median
   x[["age"]] <- cut_median(x[["age"]])
   x[["education-num"]] <- cut_median(x[["education-num"]])
   x[["hours-per-week"]] <- cut_median(x[["hours-per-week"]])


#these variables are weird... 
   x[["capital-gain"]] <- ordered(as.numeric(x[["capital-gain"]]>0))
   x[["capital-loss"]] <- ordered(as.numeric(x[["capital-loss"]]>0))
x

}


### recode like Borgelt 
### see http://fuzzy.cs.uni-magdeburg.de/~borgelt/src/census
recode.Adult_manual <- function(x) { 
    
    x[["education-num"]] <- ordered(x[["education-num"]])
    
    ## kill fnlwgt
    x[["fnlwgt"]] <- NULL
  
### equals education
    x[["education-num"]] <- NULL

  x[["age"]] <- ordered(cut(x[["age"]], c(0,25,45,65,100)), 
  	labels = c("Young", "Middle-aged", "Senior", "Old"))
  
  x[["hours-per-week"]] <- ordered(cut(x[["hours-per-week"]], 
  	c(0,25,40,60,168)), 
	labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
  
  

### Borgelt originally killes capital-gain and capital-loss

  x[["capital-gain"]] <- ordered(cut(x[["capital-gain"]], 
  	c(-Inf,0,median(x[["capital-gain"]][x[["capital-gain"]]>0]),Inf)),
          labels = c("None", "Low", "High"))
  
  x[["capital-loss"]] <- ordered(cut(x[["capital-loss"]], 
  	  c(-Inf,0,median(x[["capital-loss"]][x[["capital-loss"]]>0]),Inf)),
          labels = c("None", "Low", "High"))


x
}


Adult_recoded <- recode.Adult_manual(AdultUCI)
save(Adult_recoded, file = "Adult_recoded.rda", compress = TRUE)

library("arules")

Adult <- as(Adult_recoded, "transactions")
save(Adult, file = "Adult.rda", compress = TRUE)



