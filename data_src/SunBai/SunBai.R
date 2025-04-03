# Create the Sun&Bai data set


 #transactionID    weight       items
 #1           100 0.5176528 {A,B,C,D,E}
 #2           200 0.4362571     {C,F,G}
 #3           300 0.2321374       {A,B}
 #4           400 0.1476262         {A}
 #5           500 0.5440458   {C,F,G,H}
 #6           600 0.4123691     {A,G,H}



library(arules)
data <- list(
    c("A", "B", "C", "D", "E"),
    c("C", "F", "G"),
    c("A", "B"),
    c("A"),
    c("C", "F", "G", "H"),
    c("A", "G", "H")
)

SunBai <- as(data, "transactions")
transactionInfo(SunBai) <- data.frame(
  transactionID = (1:6)*100,
  weight = hits(SunBai), row.names = NULL
  )

summary(SunBai)

save(SunBai, file="SunBai.rda")
