##Problem 1: DMBA 2.11

## a)
## I used the "dummies" package in R to make botht he Fuel_Type and Color categorical attributes into binary variables. 
## Rcode:
Toyota.data <- read.csv("ToyotaCorolla.csv", header = TRUE)
dim(Toyota.data)
install.packages('dummies')
library(dummies)
head(Toyota.data)

Toyota.data <- dummy.data.frame(Toyota.data, names = c("Fuel_Type","Color") , sep = ".")
head(Toyota.data)

## b) I will use the train and validation data to "train"and "validate"my model. I would use the training partition (generally the largest partition) to build my models. Then, the validation partition to assess the the predictive performance of each model that I build. Lastly, the test partition (holdout/evaluation partition) is used to re-evaluate the model (it applies to overcome lesses overfitting problems when we create many models to be used on the validation data). Lastly, I would use my final model to predict/classify on new data.
## R code:

set.seed(1)
train.rows <- sample(rownames(Toyota.data), dim(Toyota.data)[1]*0.5)
valid.rows <- sample(setdiff(rownames(Toyota.data), train.rows), 
                     dim(Toyota.data)[1]*0.3)
test.rows <- setdiff(rownames(Toyota.data), union(train.rows, valid.rows))

train.data <- Toyota.data[train.rows, ]
valid.data <- Toyota.data[valid.rows, ]
test.data <- Toyota.data[test.rows, ]

##Problem 2: DMBA 3.3
##a) The store with the highest average retail price is N17 6QA and the lowest is W4 3PH. 
Laptop.data <- read.csv("LaptopSalesJanuary2008.csv")
data.for.plot <- aggregate(Laptop.data$Retail.Price, by = list(Laptop.data$Store.Postcode), FUN = mean)
names(data.for.plot) <- c("Store.Postcode", "Mean.Retail.Price")
library(ggplot2)
ggplot(data.for.plot) + geom_bar(aes(x = Store.Postcode, y = Mean.Retail.Price), stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##b) Yes, there is a difference between both stores. First,  N17 6QA has a higher median than W4 3PH. In addition, the max value of N17 6QA is higher than W4's. Further, the min value of W4's is lower than N17's. Lastly, W4 3PH has more outliers than both on top and bottom than N17 6QA. 

ggplot(Laptop.data) + geom_boxplot(aes(x =as.factor(Store.Postcode), y = Retail.Price)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

par(mfrow = c(1, 2))
boxplot(Laptop.data$Retail.Price[Laptop.data$Store.Postcode=="N17 6QA"], xlab = "N17 6QA", ylab = "Retail.Price")
boxplot(Laptop.data$Retail.Price[Laptop.data$Store.Postcode=="W4 3PH"], xlab = "W4 3PH", ylab = "Retail.Price")

##Problem 3: DMBA 4.1
install.packages("tidyverse")
library("tidyverse")

Cereals.data <- read.csv("Cereals.csv")
head(Cereals.data)
dimnames(Cereals.data)
str(Cereals.data)

##a) The quantitative variables are calories, protein, fat, sodium, fiber, carbo, sugars, potass, weight, vitamins, cups and rating. The nominal are name, mfr and type. The ordinal is shelf variable. 
##

##b) 
Cereals.data.only.quant <- Cereals.data[,-c(1,2,3)]

data.frame(mean=sapply(Cereals.data.only.quant, mean), 
           sd=sapply(Cereals.data.only.quant, sd), 
           min=sapply(Cereals.data.only.quant, min), 
           max=sapply(Cereals.data.only.quant, max), 
           median=sapply(Cereals.data.only.quant, median), 
           length=sapply(Cereals.data.only.quant, length),
           miss.val=sapply(Cereals.data.only.quant, function(x) 
             sum(length(which(is.na(x))))))

data.frame(mean=sapply(Cereals.data.only.quant, mean, na.rm=TRUE), 
           sd=sapply(Cereals.data.only.quant, sd, na.rm=TRUE), 
           min=sapply(Cereals.data.only.quant, min, na.rm=TRUE), 
           max=sapply(Cereals.data.only.quant, max, na.rm=TRUE), 
           median=sapply(Cereals.data.only.quant, median, na.rm=TRUE))


##c)
##
library(ggplot2)

##R code (plots)
ggplot(Cereals.data, aes(x=calories)) + geom_histogram() + stat_bin(bins = 25)
ggplot(Cereals.data, aes(x=protein)) + geom_histogram() + stat_bin(bins = 15)
ggplot(Cereals.data, aes(x=fat)) + geom_histogram() + stat_bin(bins = 15)
ggplot(Cereals.data, aes(x=sodium)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=fiber)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=carbo)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=sugars)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=potass)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=weight)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=vitamins)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=cups)) + geom_histogram() + stat_bin(bins = 30)
ggplot(Cereals.data, aes(x=rating)) + geom_histogram() + stat_bin(bins = 30)

##c) i. Potass and Sodium are the ones with the largest variability, witht he higher standard deviation and the most difference between Min and Max. 
##
##c) ii. Looking at the histograms below, the ones that look more skewed are Potass (mostly between 0-100), fat (mostly between 0-2) and protein (most between 0-3).
##
##c) iii. Vitamins seem to be mostly at 25, very few at 0 and 100. Further, with fat, there is a couple that are at 5 seem to be out of the normal range (0-3); and the same happens with protein with some outliers at 6 (outside the most common range of 0-4).  

##d) The plot shows us that there is only 1 data point for hot cereal that is right at the quartile 1 point of the cold cereals. 

ggplot(Cereals.data) + geom_boxplot(aes(x =as.factor(type), y = calories)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##e) It seems that there is not much difference between 1 and 3, thus, we might not need to keep the two; something like - mid (2) vs tips (1&3) should be enough if we were to predict consumer rating from shelf height.
##R code
ggplot(Cereals.data) + geom_boxplot(aes(x =as.factor(shelf), y = rating)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

##f)

##R code
correlation <- cor(Cereals.data.only.quant, Cereals.data.only.quant, use = "na.or.complete" )

install.packages("ggplot2")
install.packages("GGally")
library(GGally)
ggcorrplot(correlation)
Correlation.Matrices <- ggpairs(Cereals.data.only.quant, columns = 1:13)

