library(arules)


Mushroom <- read.csv("agaricus-lepiota.data")
summary(Mushroom)


trans = list(
  Class = "edible=e,poisonous=p",
  CapShape = "bell=b,conical=c,convex=x,flat=f,knobbed=k,sunken=s",
  CapSurf = "fibrous=f,grooves=g,scaly=y,smooth=s",
  CapColor = "brown=n,buff=b,cinnamon=c,gray=g,green=r,pink=p,purple=u,red=e,white=w,yellow=y",
  Bruises = "bruises=t,no=f",
  Odor = "almond=a,anise=l,creosote=c,fishy=y,foul=f,musty=m,none=n,pungent=p,spicy=s",
  GillAttached = "attached=a,descending=d,free=f,notched=n",
  GillSpace = "close=c,crowded=w,distant=d",
  GillSize = "broad=b,narrow=n",
  GillColor = "black=k,brown=n,buff=b,chocolate=h,gray=g,green=r,orange=o,pink=p,purple=u,red=e,white=w,yellow=y",
  StalkShape = "enlarging=e,tapering=t",
  StalkRoot = "bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?",
  SurfaceAboveRing = "fibrous=f,scaly=y,silky=k,smooth=s",
  SurfaceBelowRing = "fibrous=f,scaly=y,silky=k,smooth=s",
  ColorAboveRing = "brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y",
  ColorBelowRing = "brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y",
  VeilType = "partial=p,universal=u",
  VeilColor = "brown=n,orange=o,white=w,yellow=y",
  RingNumber = "none=n,one=o,two=t",
  RingType = "cobwebby=c,evanescent=e,flaring=f,large=l,none=n,pendant=p,sheathing=s,zone=z",
  Spore = "black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y",
  Population = "black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y",
  Habitat = "grasses=g,leaves=l,meadows=m,paths=p,urban=u,waste=w,woods=d"
)


colnames(Mushroom) <- names(trans)
head(Mushroom)

## NAs
Mushroom[["StalkRoot"]][Mushroom[["StalkRoot"]] == "?"] <- NA
Mushroom[["StalkRoot"]] <- factor(Mushroom[["StalkRoot"]])

for(f in names(trans)) {
  Mushroom[[f]] <- as.factor(Mushroom[[f]])
  nl <- t(do.call("cbind", strsplit(unlist(strsplit(trans[[f]], ",")), "=")))[,2:1]
  l <- levels(Mushroom[[f]])
  m <- match(l, nl[,1])
  levels(Mushroom[[f]]) <- nl[m,2]
}

summary(Mushroom)

save(Mushroom, file="MushroomUCI.rda")

Mushroom <- as(Mushroom, "transactions")
inspect(head(Mushroom))
summary(Mushroom)
dim(Mushroom)

save(Mushroom, file="Mushroom.rda")

rules <- apriori(Mushroom, parameter=list(conf=.8, support=.1, maxlen = 3), appearance = list(rhs = c("Class=edible", "Class=poisonous"), default = "lhs"))
inspect(head(sort(rules, by="lift"), 20))

