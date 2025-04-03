library("arules")

### create categories
itemNames <- read.table("Groceries.labels", col.names=c("id","name","german"), 
	colClasses="character",sep="\t")
cat2 <- read.table("Groceries.cat2.labels", col.names=c("id","name","german"),
	colClasses="character",sep="\t")
cat1 <- read.table("Groceries.cat1.labels", col.names=c("id","name","german"),
	colClasses="character",sep="\t")
      
### create data sets
data <-  as.matrix(read.table("Groceries.data"))

### set dimnames
dimnames(data) <- list(NULL, itemNames[,2])

Groceries <- as(data, "transactions")
#itemLabels(Groceries) <- itemNames[,2]
ii <- itemInfo(Groceries)

id2 <- substr(itemNames[,"id"],1,2)
ii$level2 <- as.factor(cat2$name[match(id2, cat2$id)])

id1 <- substr(itemNames[,"id"],1,1)
ii$level1 <- as.factor(cat1$name[match(id1, cat1$id)])

itemInfo(Groceries) <- ii

save(Groceries, file="Groceries.rda", compress=TRUE)




