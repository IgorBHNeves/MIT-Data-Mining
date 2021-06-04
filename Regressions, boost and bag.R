
library(caret)
library(DiscriMiner)

##a)

universal.df <- read.csv("UniversalBank.csv")

##Take out zip code

universal.df$ZIP.Code <- NULL

## Subsetting

set.seed(1)

train.index <- sample(row.names(universal.df), 0.6 * nrow(universal.df))

valid.index <- setdiff(row.names(universal.df), train.index)

train.df <- universal.df[train.index, ]

valid.df <- universal.df[valid.index, ]

## balance class

set.seed(1)

train.oversample.index <- sample(x = row.names(train.df[train.df$Personal.Loan ==1, ]), size = sum(train.df$Personal.Loan == 0), replace = TRUE)

train.oversample.df <- train.df[train.oversample.index, ]

train.balance.df <- rbind(train.df[train.df$Personal.Loan ==0, ], train.oversample.df)

table(train.balance.df$Personal.Loan)

## Dircriminant analysis & statistics

da.reg <- linDA(train.balance.df[,2:12], train.balance.df[,13])
da.reg$functions
da.reg$scores
da.reg$confusion
confusionMatrix(da.reg$classification, as.factor(train.balance.df$Personal.Loan))

##Sumamry statistics
install.packages("purrr")
library(purrr)
install.packages("psych")
library(psych)
train.balance.df %>% split(.$Personal.Loan) %>% map(describe)
          

summary(train.balance.df)
##b)
da.reg <- linDA(train.balance.df[,2:12], train.balance.df[,13])

library(MASS)
# Fit the model
model <- lda( Personal.Loan ~ ., data = train.balance.df)
# Make predictions
predictions <- predict(model, valid.df, type = "response")
# Model accuracy
mean(predictions$class==valid.df$Personal.Loan)
##i) 0.897

##ii)

#Confusion matrix
lda.cm <- table(valid.df$Personal.Loan, predictions$class)
lda.cm

model
predictions$x

##iii) 

data.frame(Actual=valid.df$Personal.Loan, predictions$class, predictions$x)

##c) 

##Lift chart
pb <- predictions$posterior
pb <- as.data.frame(pb)
pred.LDA <- data.frame(valid.df$Personal.Loan, pb$`1`)
colnames(pred.LDA) <- c("target","score")
pred.LDA$target <- as.factor(pred.LDA$target)
lift.LDA <- lift(target ~ score, data = pred.LDA, cuts=10, class="1")
xyplot(lift.LDA, main="LDA - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

##d) 
## Regression
reg <-glm(Personal.Loan ~ ., 
         data = train.balance.df, family = "binomial")

summary(reg)
reg$fitted.values


Pred.reg <- predict(reg, valid.df, type = "response")
Pred.reg
threshold <- 0.5
predicted.classifications <- as.integer((Pred.reg >= threshold))
actual.classifications <- valid.df$Personal.Loan


reg.cm <- confusionMatrix(as.factor(predicted.classifications), as.factor(actual.classifications))
reg.cm


#Find the predicted probabilities, already computed for us (we don't need to manuall convert the linear predictors)
reg$fitted.values


############13.1#############

universal.df <- read.csv("UniversalBank.csv")

##Take out zip code

universal.df$ZIP.Code <- NULL

## Subsetting

set.seed(1)

train.index <- sample(row.names(universal.df), 0.6 * nrow(universal.df))

valid.index <- setdiff(row.names(universal.df), train.index)

train.df <- universal.df[train.index, ]

valid.df <- universal.df[valid.index, ]

##a) 

##Log regression
reg <-glm(Personal.Loan ~ ., 
          data = train.df, family = "binomial")

summary(reg)
Pred.reg <- predict(reg, valid.df, type = "response")
Pred.reg
threshold <- 0.5
predicted.classifications <- as.integer((Pred.reg >= threshold))
actual.classifications <- valid.df$Personal.Loan
reg.cm <- confusionMatrix(as.factor(predicted.classifications), as.factor(actual.classifications))
reg.cm


## knn

train.norm.df <- train.df
valid.norm.df <- valid.df
universal.norm.df <- universal.df
#new.norm.df <- as.data.frame(scale(new.df))
norm.values <- preProcess(train.df[, 2:12], method=c("center", "scale"))
train.norm.df[, 2:12] <- predict(norm.values, train.df[, 2:12])
valid.norm.df[, 2:12] <- predict(norm.values, valid.df[, 2:12])
universal.norm.df[, 2:12] <- predict(norm.values, universal.df[, 2:12])



##Do the knn prediction using k=3  and testing the validation data.
library(FNN)
knn.pred2 <- class::knn(train = train.norm.df[, -13],test = valid.norm.df[, -13], cl = train.norm.df[,13], k=3)
knn.pred2

valid.norm.df$Personal.Loan <- as.factor(valid.norm.df$Personal.Loan)
##Creating a confusion matrix using the new prediction on the main colum 10 (personal loan) as factor of the validation data. 
confusionMatrix(knn.pred2, valid.norm.df$Personal.Loan)

##Classification trees

library(caret) #need this for the preProcess() function
library(forecast) #for the accuracy() function, if you wish to use it...
install.packages("DiscriMiner")
library(DiscriMiner) #for discriminant analysis
install.packages("gains")
library(gains)
library(rpart)
library(rpart.plot)

Regression.tree <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
rpart.plot(Regression.tree)
PredictTrain = predict(Regression.tree, newdata = train.df, type="class")
PredictValid = predict(Regression.tree, newdata = valid.df, type="class")

valid.df$Personal.Loan <- as.factor(valid.df$Personal.Loan)
train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
confusionMatrix(PredictValid, valid.df$Personal.Loan) ##Classification Tree

##b)
ensemble.df <- data.frame(valid.df$Personal.Loan,predicted.classifications, knn.pred2,PredictValid)

colnames(ensemble.df) <- c("Actual","Pred.reg","Pred.knn","Pred.rt")

head(ensemble.df)

##c)
install.packages("adabag")
library(adabag)
library(rpart)
library(caret)

##Boost and bag
boost <- boosting(Personal.Loan ~ ., data = train.df, method = "class")
boost.pred <- predict(boost, valid.df, type = "class")
confusionMatrix(as.factor(boost.pred$class), valid.df$Personal.Loan)

bag <- bagging(Personal.Loan ~ ., data = train.df, method = "class")
bag.pred <- predict(bag, valid.df, type = "class")
confusionMatrix(as.factor(bag.pred$class), valid.df$Personal.Loan)

##adding to the data frame

ensemble.df.2 <- data.frame(valid.df$Personal.Loan,predicted.classifications, knn.pred2,PredictValid, boost.pred$class, bag.pred$class)

colnames(ensemble.df.2) <- c("Actual","Pred.reg","Pred.knn","Pred.rt", "Boost.pred", "Bag.pred")
ensemble.df.2

##d)

valid.df.2 <- universal.df[valid.index, ]

library(Metrics)
RMSE.valid.ct <- rmse(actual = valid.df.2$Personal.Loan, # the actual values
                     predicted = as.integer(PredictValid)) # the predicted values
RMSE.valid.ct

RMSE.valid.reg <- rmse(actual = valid.df.2$Personal.Loan, # the actual values
                      predicted = as.integer(predicted.classifications)) # the predicted values
RMSE.valid.reg
RMSE.valid.knn <- rmse(actual = valid.df.2$Personal.Loan, # the actual values
                       predicted = as.integer(knn.pred2)) # the predicted values
RMSE.valid.knn
RMSE.valid.boost <- rmse(actual = valid.df.2$Personal.Loan, # the actual values
                       predicted = as.integer(boost.pred$class)) # the predicted values
RMSE.valid.boost
RMSE.valid.bag <- rmse(actual = valid.df.2$Personal.Loan, # the actual values
                         predicted = as.integer(bag.pred$class)) # the predicted values
RMSE.valid.bag
