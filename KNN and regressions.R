
### HW 2 - Igor Brenner Hernandez Neves

library(tidyverse)
library(ggplot2)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(ggplot2)


## 7.2 
## 


#split = createDataPartition(UniBank.data$ID, p=0.6, list = FALSE)
#UniBank.train = UniBank.data[split,]
#Unibank.valid = UniBank.data[-split,]


library(class)
library(caret)


Unibank.clean2 <- read.csv("UniversalBank.csv")
set.seed(1)
#Unibank.clean2 <- UniBank
#Unibank.clean2 <- as.data.frame(scale(Unibank.clean))
#Unibank.clean2 <- as.data.frame(Unibank.clean2)

train.index <- sample(row.names(Unibank.clean2), 0.6*dim(Unibank.clean2)[1])  
valid.index <- setdiff(row.names(Unibank.clean2), train.index)  
train.df <- Unibank.clean2[train.index, ]
valid.df <- Unibank.clean2[valid.index, ]
#train.df.ready <- subset(train.df, select = -c(ID,ZIP.Code) )
#valid.df.ready <- subset(valid.df, select = -c(ID,ZIP.Code) )
head(train.df)
str(train.df)
#
## a) Creating a new data frame with the variables
new.df <- data.frame("ID"= c(0), "Age" = c(40), "Experience" = c(10), "Income" = c(84), "ZIP.Code" = c(91000), "Family"= c(2), "CCAvg"= c(2), "Education" = c(2), "Mortgage"= c(0),"Personal.Loan"= c(0), "Securities.Account" = c(0), "CD.Account"= c(0), "Online" = c(1), "CreditCard"= c(1))
#new.df <- data.frame("Age" = c(40), "Experience" = c(10), "Income" = c(84), "Family"= c(2), "CCAvg"= c(2), "Education" = c(2), "Mortgage"= c(0), "Securities.Account" = c(0), "CD.Account"= c(0), "Online" = c(1), "CreditCard"= c(1))

#new.df.2 <- as.data.frame(scale(new.df))
#new.df.2 <- as.data.frame(new.df)
#train.norm.df <- as.data.frame(scale(train.df))
#valid.norm.df <- as.data.frame(scale(valid.df))
#Unibank.clean2.norm <- as.data.frame(scale(Unibank.clean2)) 
train.norm.df <- train.df
valid.norm.df <- valid.df
Unibank.clean2.norm <- Unibank.clean2
#new.norm.df <- as.data.frame(scale(new.df))
norm.values <- preProcess(train.df[, 2:9], method=c("center", "scale"))
train.norm.df[, 2:9] <- predict(norm.values, train.df[, 2:9])
valid.norm.df[, 2:9] <- predict(norm.values, valid.df[, 2:9])
Unibank.clean2.norm[, 2:9] <- predict(norm.values, Unibank.clean2.norm[, 2:9])
new.norm.df <- predict(norm.values, new.df)

library(FNN)
nn <- class::knn(train = train.norm.df[, -10],test = new.norm.df[, -10], cl = train.norm.df[,10], k=1)

row.names(train.df)[attr(nn, "nn.index")]
nn
head(train.norm.df)

## (b)
library(caret)

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1,14,1), accuracy = rep(0, 2))

head(valid.norm.df)

#Making personal loan a factor
valid.norm.df.factor <-  as.factor(valid.norm.df[, 10])

# computing knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, -10], valid.norm.df[, -10], 
                  cl = train.norm.df[, 10], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df.factor)$overall[1]
}
accuracy.df

####(c)###

##Re-do the knn prediction using k=4 and testing the validation data.
library(FNN)
knn.pred2 <- class::knn(train = train.norm.df[, -10],test = valid.norm.df[, -10], cl = train.norm.df[,10], k=4)
knn.pred2

##Creating a confusion matrix using the new prediction on the main colum 10 (personal loan) as factor of the validation data. 
confusionMatrix(knn.pred2, valid.norm.df.factor)


###(d)###
##creating a new data frame with the new variable and its parameters
new.df.2 <- data.frame("ID"= c(0), "Age" = c(40), "Experience" = c(10), "Income" = c(84), "ZIP.Code" = c(91000), "Family"= c(2), "CCAvg"= c(2), "Education" = c(2), "Mortgage"= c(0),"Personal.Loan"= c(0), "Securities.Account" = c(0), "CD.Account"= c(0), "Online" = c(1), "CreditCard"= c(1))

##Normalizing the new data frame
norm.values <- preProcess(train.df[, 2:9], method=c("center", "scale"))
new.norm.df.2 <- predict(norm.values, new.df.2)

##Predicting with the new k value

nn.2 <- class::knn(train = train.norm.df[, -10],test = new.norm.df.2[, -10], cl = train.norm.df[,10], k=4)

nn.2

###(e)###
library(class)
library(caret)
library(caret)

unibank <- read.csv("UniversalBank.csv")
set.seed(1)

ss <- sample(1:3,size=nrow(unibank),replace=TRUE,prob=c(0.5,0.3,0.2))
train.df <- unibank[ss==1,]
valid.df <- unibank[ss==2,]
test.df <- unibank[ss==3,]

train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df  <- test.df
norm.values <- preProcess(train.df[, 2:9], method=c("center", "scale"))
train.norm.df[, 2:9] <- predict(norm.values, train.df[, 2:9])
valid.norm.df[, 2:9] <- predict(norm.values, valid.df[, 2:9])
test.norm.df[, 2:9] <- predict(norm.values, test.norm.df[, 2:9])

## Creating predicition using train and validation datas
knn.pred1 <- class::knn(train = train.norm.df[, -10],test = valid.norm.df[, -10], cl = train.norm.df[,10], k=4)
knn.pred1

#Making personal loan a factor
valid.norm.df.factor <-  as.factor(valid.norm.df[, 10])

##Creating a confusion matrix using the new prediction on the main colum 10 (personal loan) as factor of the validation data. 
confusionMatrix(knn.pred1, valid.norm.df.factor)

# Creating predicition using train  with test data
knn.pred2 <- class::knn(train = train.norm.df[, -10],test = test.norm.df[, -10], cl = train.norm.df[,10], k=4)
knn.pred2

#Making personal loan a factor
test.norm.df.factor <-  as.factor(test.norm.df[, 10])

##Creating a confusion matrix using the new prediction on the main colum 10 (personal loan) as factor of the validation data. 
confusionMatrix(knn.pred2, test.norm.df.factor)


######8.1######

##(a)
unibank <- read.csv("UniversalBank.csv")
set.seed(1)
train.index <- sample(row.names(unibank), 0.6*dim(unibank)[1])  
valid.index <- setdiff(row.names(unibank), train.index)  
train.df <- unibank[train.index, ]
valid.df <- unibank[valid.index, ]

t1 <- table(train.df$Personal.Loan,train.df$Online,train.df$CreditCard, dnn = c("Has PLoan","is Online", "Has CC"))
t1

##(b)##

p.t1 <- prop.table(t1)
p.t1
p.t1[2,2,2]
p.t1[1,2,2]
##If conditional probability (not sure because of the english in the exercise -- very subtle)

p.t1[2,2,2]/(p.t1[2,2,2]+p.t1[1,2,2])


##(c)##
t2 <- table(train.df$Personal.Loan,train.df$Online, dnn = c("Has PLoan","is Online"))
t2
t3 <- table(train.df$Personal.Loan,train.df$CreditCard, dnn = c("Has PLoan", "Has CC"))
t3

t2
t3

##(d)##

#i)
t3
i <-t3[2,2]/(t3[2,2]+t3[2,1])
#0.282

#ii)
t2
ii <- t2[2,2]/(t2[2,2]+t2[2,1])
#0.618

#iii)
tloan <- table(train.df$Personal.Loan, dnn = c("Has PLoan"))
tloan
tloan[2]/(tloan[1]+tloan[2])
iii <- 0.097

##iv)
t3
iv <- t3[1,2]/(t3[1,2]+t3[1,1])
##0.290

##v)
t2
v <- t2[1,2]/(t2[1,2]+t2[1,1])
##0.595
t2[1,2]

##vi) 
tloan
tloan[1]/(tloan[1]+tloan[2])
vi <- 0.903

##e)

NaiveBayes = (i*ii*iii)/( i*ii*iii + iv*v*vi)

NaiveBayes

##g)

train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
train.df$Online <- as.factor(train.df$Online)
train.df$CreditCard <- as.factor(train.df$CreditCard)
nb <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = train.df)
nb

###############9.3#######################
library(caret) #need this for the preProcess() function
library(forecast) #for the accuracy() function, if you wish to use it...
install.packages("DiscriMiner")
library(DiscriMiner) #for discriminant analysis
install.packages("gains")
library(gains)
library(rpart)
library(rpart.plot)

Toyota <- read.csv("ToyotaCorolla.csv")
head(Toyota)
str(Toyota)

train.index <- sample(row.names(Toyota), 0.6*dim(Toyota)[1])  
valid.index <- setdiff(row.names(Toyota), train.index)  
train.df <- Toyota[train.index, ]
valid.df <- Toyota[valid.index, ]

Regression.tree <- rpart(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows + Sport_Model + Tow_Bar , data = train.df, 
                         control = rpart.control(maxdepth = 30),cp=0.001, method = "anova")

rpart.plot(Regression.tree)
varImp(Regression.tree)[order(varImp(Regression.tree)$Overall, decreasing = TRUE), ,
           drop = FALSE]

PredictTrain = predict(Regression.tree, newdata = train.df, type = "anova")
PredictValid = predict(Regression.tree, newdata = valid.df, type = "anova")
install.packages("Metrics")
library(Metrics)

RMSE.train <- rmse(actual = train.df$Price, # the actual values
     predicted = PredictTrain) # the predicted values

RMSE.valid <- rmse(actual = valid.df$Price, # the actual values
                   predicted = PredictValid) # the predicted values

RMSE.train
RMSE.valid

boxplot(PredictTrain ~ train.df$Price, 
        xlab = "Train Actual", ylab = "Train Prediction")

boxplot(PredictValid ~ valid.df$Price, 
        xlab = "Valid Actual", ylab = "Valid Prediction")

##iii) Word
##

##iv)
Toyota <- read.csv("ToyotaCorolla.csv")
head(Toyota)
str(Toyota)

train.index <- sample(row.names(Toyota), 0.6*dim(Toyota)[1])  
valid.index <- setdiff(row.names(Toyota), train.index)  
train.df <- Toyota[train.index, ]
valid.df <- Toyota[valid.index, ]

Regression.tree.2 <- rpart(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows + Sport_Model + Tow_Bar , data = train.df, method = "anova")

rpart.plot(Regression.tree.2)

PredictTrain.2 = predict(Regression.tree.2, newdata = train.df)
PredictValid.2 = predict(Regression.tree.2, newdata = valid.df)

library(Metrics)

RMSE.train.2 <- rmse(actual = train.df$Price, # the actual values
                   predicted = PredictTrain.2) # the predicted values

RMSE.valid.2 <- rmse(actual = valid.df$Price, # the actual values
                   predicted = PredictValid.2) # the predicted values

RMSE.train.2
RMSE.valid.2

###b)
Toyota.2 <- read.csv("ToyotaCorolla.csv")
head(Toyota)
str(Toyota)

#Creating bins
bins <- seq(min(Toyota$Price),max(Toyota$Price),(max(Toyota$Price)-min(Toyota$Price))/20)
bins
Toyota$Binned_Price <- .bincode(Toyota$Price, bins, include.lowest =
           TRUE)
##check if bin worked
max(Toyota$Binned_Price)

#Partitioning data
train.index <- sample(row.names(Toyota), 0.6*dim(Toyota)[1])  
valid.index <- setdiff(row.names(Toyota), train.index)  
train.df <- Toyota[train.index, ]
valid.df <- Toyota[valid.index, ]

Regression.tree.3 <- rpart(Binned_Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows + Sport_Model + Tow_Bar , data = train.df, method = "class")

rpart.plot(Regression.tree.3)

##i) 


##Create new record


new.record <- data.frame("Age_08_04"= c(77), "KM" = c(117000), "Fuel_Type" = c("Petrol"), "HP" = c(110), "Automatic" = c(0), "Doors"= c(5), "CCAvg"= c(2), "Quarterly_Tax" = c(100), "Mfr_Guarantee"= c(0),"Guarantee_Period"= c(3), "Airco" = c(1), "Automatic_airco"= c(0), "CD_Player" = c(0), "Powered_Windows"= c(0), "Sport_Model"= c(0), "Tow_Bar"= c(1), stringsAsFactors = TRUE)

str(new.record)

predict(Regression.tree.2, newdata = new.record)

bins[predict(Regression.tree.3, newdata =
               new.record, type = "class")]

##iii) word
#


################11.3#################
install.packages("neuralnet")
library(neuralnet)
toyota.df <- read.csv("ToyotaCorolla.csv")

##Preprocessing, making Fuel into 1/0 variable
toyota.df$Fuel_Type_CNG <- 1* (toyota.df$Fuel_Type == "CNG")
toyota.df$Fuel_Type_Diesel <- 1* (toyota.df$Fuel_Type == "Diesel") 

##Partitioning the data
set.seed(1)
train.index <- sample(row.names(toyota.df), 0.6*dim(toyota.df)[1])  
valid.index <- setdiff(row.names(toyota.df), train.index)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[valid.index, ]

##Scaling/normalizing the data
library(caret)
norm.object <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.object, train.df)
valid.norm.df <- predict(norm.object, valid.df)



##ii)

##2 nodes, 1 layer
nn <- neuralnet(Price ~ Age_08_04+KM+Fuel_Type_CNG+Fuel_Type_Diesel+HP+
                  Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+
                  Guarantee_Period+Airco+Automatic_airco+CD_Player+
                  Powered_Windows+Sport_Model+Tow_Bar,
                data = train.norm.df, linear.output = T,hidden = 2)
plot(nn)
nn.pred <- neuralnet::prediction(nn)
nn$result.matrix

validation.prediction <- compute(nn, valid.norm.df[, c("Age_08_04", 
                                                       "KM", 
                                                       "Fuel_Type_CNG",   
                                                       "Fuel_Type_Diesel",
                                                       "HP",     
                                                       "Automatic", 
                                                       "Doors",
                                                       "Quarterly_Tax", 
                                                       "Mfr_Guarantee",
                                                       "Guarantee_Period", 
                                                       "Airco", "Automatic_airco", 
                                                       "CD_Player",  
                                                       "Powered_Windows", 
                                                       "Sport_Model", 
                                                       "Tow_Bar")])

v.pred <- validation.prediction$net.result
v.pred.vector <- as.vector(v.pred)
library(forecast)
accuracy(v.pred.vector,valid.norm.df$Price)


## 5 nodes, 1 layer


nn.5 <- neuralnet(Price ~ Age_08_04+KM+Fuel_Type_CNG+Fuel_Type_Diesel+HP+
                  Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+
                  Guarantee_Period+Airco+Automatic_airco+CD_Player+
                  Powered_Windows+Sport_Model+Tow_Bar,
                data = train.norm.df, linear.output = T,hidden = 5)
plot(nn.5)

n.pred.5 <- neuralnet::prediction(nn.5)
nn.5$result.matrix
validation.prediction.5 <- compute(nn.5, valid.norm.df[, c("Age_08_04", 
                                                       "KM", 
                                                       "Fuel_Type_CNG",   
                                                       "Fuel_Type_Diesel",
                                                       "HP",     
                                                       "Automatic", 
                                                       "Doors",
                                                       "Quarterly_Tax", 
                                                       "Mfr_Guarantee",
                                                       "Guarantee_Period", 
                                                       "Airco", "Automatic_airco", 
                                                       "CD_Player",  
                                                       "Powered_Windows", 
                                                       "Sport_Model", 
                                                       "Tow_Bar")])

v.pred.5 <- validation.prediction.5$net.result
v.pred.vector.5 <- as.vector(v.pred.5)
library(forecast)
accuracy(v.pred.vector.5,valid.norm.df$Price)

## 5 nodes 2 layers
nn.55 <- neuralnet(Price ~ Age_08_04+KM+Fuel_Type_CNG+Fuel_Type_Diesel+HP+
                    Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+
                    Guarantee_Period+Airco+Automatic_airco+CD_Player+
                    Powered_Windows+Sport_Model+Tow_Bar,
                  data = train.norm.df, linear.output = T,hidden = c(5,5))
plot(nn.55)

n.pred.55 <- neuralnet::prediction(nn.55)
nn.55$result.matrix
validation.prediction.55 <- compute(nn.55, valid.norm.df[, c("Age_08_04", 
                                                           "KM", 
                                                           "Fuel_Type_CNG",   
                                                           "Fuel_Type_Diesel",
                                                           "HP",     
                                                           "Automatic", 
                                                           "Doors",
                                                           "Quarterly_Tax", 
                                                           "Mfr_Guarantee",
                                                           "Guarantee_Period", 
                                                           "Airco", "Automatic_airco", 
                                                           "CD_Player",  
                                                           "Powered_Windows", 
                                                           "Sport_Model", 
                                                           "Tow_Bar")])

v.pred.55 <- validation.prediction.55$net.result
v.pred.vector.55 <- as.vector(v.pred.55)
library(forecast)
accuracy(v.pred.vector.55,valid.norm.df$Price)

##i)

##2 nodes, 1 layer

train.prediction <- compute(nn, train.norm.df[, c("Age_08_04", 
                                                  "KM", 
                                                  "Fuel_Type_CNG",   
                                                  "Fuel_Type_Diesel",
                                                  "HP",     
                                                  "Automatic", 
                                                  "Doors",
                                                  "Quarterly_Tax", 
                                                  "Mfr_Guarantee",
                                                  "Guarantee_Period", 
                                                  "Airco", "Automatic_airco", 
                                                  "CD_Player",  
                                                  "Powered_Windows", 
                                                  "Sport_Model", 
                                                  "Tow_Bar")])

t.pred <- train.prediction$net.result
t.pred.vector <- as.vector(t.pred)
library(forecast)
accuracy(t.pred.vector,train.norm.df$Price)

## 5 nodes, 1 layer

train.prediction.5 <- compute(nn.5, train.norm.df[, c("Age_08_04", 
                                                  "KM", 
                                                  "Fuel_Type_CNG",   
                                                  "Fuel_Type_Diesel",
                                                  "HP",     
                                                  "Automatic", 
                                                  "Doors",
                                                  "Quarterly_Tax", 
                                                  "Mfr_Guarantee",
                                                  "Guarantee_Period", 
                                                  "Airco", "Automatic_airco", 
                                                  "CD_Player",  
                                                  "Powered_Windows", 
                                                  "Sport_Model", 
                                                  "Tow_Bar")])

t.pred.5 <- train.prediction.5$net.result
t.pred.vector.5 <- as.vector(t.pred.5)
library(forecast)
accuracy(t.pred.vector.5,train.norm.df$Price)

# 5 nodes 2 layers

train.prediction.55 <- compute(nn.55, train.norm.df[, c("Age_08_04", 
                                                      "KM", 
                                                      "Fuel_Type_CNG",   
                                                      "Fuel_Type_Diesel",
                                                      "HP",     
                                                      "Automatic", 
                                                      "Doors",
                                                      "Quarterly_Tax", 
                                                      "Mfr_Guarantee",
                                                      "Guarantee_Period", 
                                                      "Airco", "Automatic_airco", 
                                                      "CD_Player",  
                                                      "Powered_Windows", 
                                                      "Sport_Model", 
                                                      "Tow_Bar")])

t.pred.55 <- train.prediction.55$net.result
t.pred.vector.55 <- as.vector(t.pred.55)
library(forecast)
accuracy(t.pred.vector.55,train.norm.df$Price)

##iii) Word


