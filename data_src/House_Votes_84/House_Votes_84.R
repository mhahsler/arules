read.House_Votes_84 <- function(file = "house-votes-84.data") {
    x <- read.csv(file, header = FALSE, na.strings = "?")
    names(x) <- c("Class",
                  "handicapped-infants", 
                  "water-project-cost-sharing", 
                  "adoption-of-the-budget-resolution", 
                  "physician-fee-freeze", 
                  "el-salvador-aid", 
                  "religious-groups-in-schools", 
                  "anti-satellite-test-ban", 
                  "aid-to-nicaraguan-contras", 
                  "mx-missile", 
                  "immigration", 
                  "synfuels-corporation-cutback", 
                  "education-spending", 
                  "superfund-right-to-sue", 
                  "crime", 
                  "duty-free-exports", 
                  "export-administration-act-south-africa")
    x
}
                  
HouseVotes84UCI <- read.House_Votes_84()
save(HouseVotes84UCI, file = "HouseVotes84UCI.rda", compress = TRUE)

HouseVotes84 <- as(HouseVotes84UCI, "transactions")

