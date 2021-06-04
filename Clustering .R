###HW 4###
###
###

pharma.df <- read.csv("Pharmaceuticals.csv")
row.names(pharma.df) <- pharma.df[,1]

pharma.df.norm <- sapply(pharma.df[,c(3,4,5,6,7,8,9,10,11)], scale)

row.names(pharma.df.norm) <- pharma.df[,1]
##a)
##
d.norm <- dist(pharma.df.norm, method = "euclidean")
d.norm
hc1 <- hclust(d.norm, method = "single")
hc1
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method = "complete")
plot(hc2, hang = -1, ann = FALSE)

## complete with 3 clusters
memb <- cutree(hc2, k = 3) # cut 3 clusters
centers <- aggregate( . ~ memb, data = pharma.df[,c(3,4,5,6,7,8,9,10,11)], FUN = mean)
centers

## Group by the other variables

pharma.df.2 <- pharma.df
pharma.df.2$Groups <- memb
library(dplyr)
pharma.df.2 %>%
  filter(Groups == "1") %>%
  select(Median_Recommendation, Location, Exchange)

pharma.df.2 %>%
  filter(Groups == "2") %>%
  select(Median_Recommendation, Location, Exchange)

pharma.df.2 %>%
  filter(Groups == "3") %>%
  select(Median_Recommendation, Location, Exchange)

##D word

