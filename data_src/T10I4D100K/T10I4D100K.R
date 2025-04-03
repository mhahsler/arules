library(arules)
T10I4D100k <- read.transactions("T10I4D100K.dat")

dim(T10I4D100k)
summary(T10I4D100k)
inspect(head(T10I4D100k), line = FALSE)

save(T10I4D100k, file="T10I4D100k.rda")


#itemFrequencyPlot(T10I4D100k, topN = 20)
#rules <- apriori(T10I4D100k, parameter = list(supp=0.005))