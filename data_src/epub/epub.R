require("arules")

## contains duplicate items
Epub <- read.transactions("epub.dat", format = "single", sep = ";",
                          cols = c(1,2), rm.duplicates = TRUE)

Epub_time <- read.table("epub.time", sep = ";", 
                        col.names= c("transactionID", "TimeStamp"))
Epub_time$TimeStamp <- as.POSIXct(strptime(as.character(Epub_time$TimeStamp),
                                format = "%a %b %d %H:%M:%S %Y", ))

transactionInfo(Epub) <-
    cbind(transactionInfo(Epub), 
          TimeStamp = Epub_time$TimeStamp[match(transactionInfo(Epub)$transactionID, 
          Epub_time$transactionID)])

Epub <- Epub[order(transactionInfo(Epub)$TimeStamp)]

Epub <- Epub[size(Epub)<70] ### remove robot

save(Epub, file = "Epub.rda", compress = TRUE)
