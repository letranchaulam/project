library('ggplot2') 
library('ggthemes') 
library('scales')
library('dplyr')
install.packages('mice', dependencies = TRUE)
library('mice')

titanic <- read.csv("train 2.csv", stringsAsFactors = F)
titanic <- titanic[,c(2:3, 5:8, 10,12)]
titanic$Embarked [titanic$Embarked == 'missing'] <- NA
titanic$Age [titanic$Age == "missing"] <- NA
view(titanic)
summary(titanic)
sum(is.na(titanic))
colMeans(is.na(titanic))*100

# Create a family size variable including the passenger themselves
titanic$Fsize <- titanic$SibSp + titanic$Parch + 1

# Discretize family size
titanic$FsizeD[titanic$Fsize == 1] <- 'singleton'
titanic$FsizeD[titanic$Fsize < 4 & titanic$Fsize > 1] <- 'small'
titanic$FsizeD[titanic$Fsize > 4] <- 'large'

#Eliminate NA value in embarked
titanic <- titanic[-c(62,830),]

# Make variables factors into factors
factor_vars <- c('Pclass','Sex','Embarked','FsizeD')
titanic$Age <- as.numeric(titanic$Age)

titanic[factor_vars] <- lapply(titanic[factor_vars], function(x) as.factor(x))

titanic[which(is.na(titanic$Age )), ]

# Perform mice imputation to handle missing data in age
set.seed(129)
mice_mod <- mice(titanic[, !names(titanic) %in% c('Name','Ticket','Cabin','Family','Survived')], method='rf') 
mice_output <- complete(mice_mod)
titanic$Age <- mice_output$Age
titanic[which(is.na(titanic$Age )), ]

#differentiate child and childre
titanic$Adulthood[titanic$Age < 18] <- 'Child'
titanic$Adulthood[titanic$Age >= 18] <- 'Adult'

summary(titanic)
head(titanic)
view(titanic)
str(titanic)

t(t(names(titanic)))
titanic <- titanic[ , -c(5:6, 10:11)]

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

library(fastDummies)
titanic <- dummy_cols(titanic,
                   select_columns = c("Pclass","Sex","Embarked"),
                   remove_first_dummy = FALSE,
                   remove_selected_columns = TRUE)

titanic <- dummy_cols(titanic,
                      select_columns = c("Survived"),
                      remove_first_dummy = TRUE,
                      remove_selected_columns = TRUE)



# check for highly skewed predictors
library(e1071)
skewness(titanic$Age)
skewness(titanic$Fare)
skewness(titanic$Fsize)

# apply a log transformation to highly skewed predictors
titanic$Fare <- log(titanic$Fare + 1)
titanic$Fsize <- log(titanic$Fsize + 1)

titanic$Survived_1 <- as.factor(titanic$Survived_1)

####### partition the data #######
set.seed(5)
train.index <- sample(rownames(titanic), nrow(titanic) * 0.25)
train.df <-titanic[train.index, ]
valid.index <- setdiff(rownames(titanic), train.index)
valid.df <- titanic[valid.index, ]

# convert all predictors to a 0-1 scale
titanic.train.norm <- train.df
titanic.valid.norm <- valid.df
cols <- colnames(train.df[, 1:3])
for (i in cols) {
  titanic.valid.norm[[i]] <- 
    (titanic.valid.norm[[i]] - min(train.df[[i]])) / (max(train.df[[i]]) - min(train.df[[i]]))
  titanic.train.norm[[i]] <- 
    (titanic.train.norm[[i]] - min(train.df[[i]])) / (max(train.df[[i]]) - min(train.df[[i]]))
}
summary(titanic.train.norm)
summary(titanic.valid.norm)

####### neural net with 1 hidden layer of 3 nodes #######
library(neuralnet)
titanic.nn.3 <- neuralnet(Survived_1 ~ .,           # categorical outcome ~ predictors 
                          data = titanic.train.norm,      # data for training model    
                          linear.output = FALSE,       # assume relationship is nonlinear
                          hidden = 3,
                          stepmax = 1e6)                  # a single hidden layer containing 3 nodes

# plot the neural net model
plot(titanic.nn.3, rep = "best")

predict.nn.3 <- predict(titanic.nn.3, titanic.valid.norm)
predicted.class.3 <- apply(predict.nn.3,         # in predict.cheese
                           1,                    # for each row
                           which.max) - 1        # return the column # with the max value and then subtract 1
library(caret)
confusionMatrix(as.factor(predicted.class.3), 
                titanic.valid.norm$Survived_1, 
                positive = "1")

####### neural net with 2 hidden layers of 2 nodes each #######
titanic.nn.2.2 <- neuralnet(Survived_1 ~ ., data = titanic.train.norm, linear.output = FALSE,
                            hidden = c(2,2),       # 2 hidden layers of 2 nodes each
                            stepmax = 1e6)       # increased maximum steps
plot(titanic.nn.2.2, rep = "best")

predict.nn.2.2 <- predict(titanic.nn.2.2, titanic.valid.norm)
predicted.class.2.2 <- apply(predict.nn.2.2,         # in predict.cheese
                             1,                      # for each row
                             which.max) - 1          # return the column # with the max value and then subtract 1

confusionMatrix(as.factor(predicted.class.2.2), 
                titanic.valid.norm$Survived_1, 
                positive = "1")


####### neural nets with one hidden layer of i nodes #######
results.df <- data.frame(n = seq(1, 10, 1), accuracy = rep(0, 10), sensitivity = rep(0,10),
                         specificity = rep(0,10), precision = rep(0,10))
start.time <- Sys.time()
for (i in 1:10) {
  titanic.nn <- neuralnet(Survived_1 ~ .,           # categorical outcome ~ predictors 
                            data = titanic.train.norm,      # data for training model    
                            linear.output = FALSE,       # assume relationship is nonlinear
                            hidden = i,
                            stepmax = 1e10)
  predict.nn <- predict(titanic.nn, titanic.valid.norm)
  predicted.class <- apply(predict.nn, 1, which.max) - 1
  results.df[i,2] <- confusionMatrix(as.factor(predicted.class), 
                                     (titanic.valid.norm$Survived_1), 
                                     positive = "1")$overall[1]
  results.df[i,3] <- confusionMatrix(as.factor(predicted.class), 
                                     (titanic.valid.norm$Survived_1), 
                                     positive = "1")$byClass[1]
  results.df[i,4] <- confusionMatrix(as.factor(predicted.class), 
                                     (titanic.valid.norm$Survived_1), 
                                     positive = "1")$byClass[2]
  results.df[i,5] <- confusionMatrix(as.factor(predicted.class), 
                                     (titanic.valid.norm$Survived_1), 
                                     positive = "1")$byClass[3]
}
results.df


####### neural nets with two hidden layers of i nodes #######
results.df <- data.frame(n = seq(1, 10, 1), accuracy = rep(0, 10), sensitivity = rep(0,10),
                         specificity = rep(0,10), precision = rep(0,10))
start.time <- Sys.time()
for (i in 1:10) {
  titanic.nn.2.2 <- neuralnet(Survived_1 ~ ., data = titanic.train.norm, linear.output = FALSE,
                              hidden = c(2,2),       # 2 hidden layers of 2 nodes each
                              stepmax = 1e10)
  predict.nn <- predict(titanic.nn, titanic.valid.norm)
  predicted.class <- apply(predict.nn, 1, which.max) - 1
  results.df[i,2] <- confusionMatrix(as.factor(predicted.class), 
                                     as.factor(titanic.valid.norm$Survived_1), 
                                     positive = "1")$overall[1]
  results.df[i,3] <- confusionMatrix(as.factor(predicted.class), 
                                     as.factor(titanic.valid.norm$Survived_1), 
                                     positive = "1")$byClass[1]
  results.df[i,4] <- confusionMatrix(as.factor(predicted.class), 
                                     as.factor(titanic.valid.norm$Survived_1), 
                                     positive = "1")$byClass[2]
  results.df[i,5] <- confusionMatrix(as.factor(predicted.class), 
                                     as.factor(titanic.valid.norm$Survived_1), 
                                     positive = "1")$byClass[3]
}
