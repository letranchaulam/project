library('ggplot2') 
library('ggthemes') 
library('scales')
library('dplyr')
library('mice')
library('randomForest') 
library(tidyverse)

full <- read.csv("train 2.csv", stringsAsFactors = F)
full <- full[,c(2:3, 5:8, 10,12)]
full$Embarked [full$Embarked == 'missing'] <- NA
full$Age [full$Age == "missing"] <- NA

view(full)
summary(full)
sum(is.na(full))
colMeans(is.na(full))*100

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 4 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize >= 4] <- 'large'

#Eliminate NA value in embarked
full <- full[-c(62,830),]

# Make variables factors into factors
factor_vars <- c('Pclass','Sex','Embarked','FsizeD')
full$Age <- as.numeric(full$Age)

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

full[which(is.na(full$Age )), ]

# Perform mice imputation to handle missing data in age
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c('Name','Ticket','Cabin','Family','Survived')], method='rf') 
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
full[which(is.na(full$Age )), ]

#differentiate child and childre
full$Adulthood[full$Age < 18] <- 'Child'
full$Adulthood[full$Age >= 18] <- 'Adult'

summary(full)
head(full)
view(full)
str(full)

library(fastDummies)
full <- dummy_cols(full,
                   select_columns = c("Pclass","Sex","Embarked", "FsizeD", "Adulthood"),
                   remove_first_dummy = TRUE,
                   remove_selected_columns = TRUE)
####### partition the data #######
set.seed(173)
train.rows <- sample(rownames(full), nrow(full) * 0.7)
train.df <-full[train.rows, ]
valid.rows <- setdiff(rownames(full), train.rows)
valid.df <- full[valid.rows, ]

summary(train.df)
str(train.df)

####### normalize data to 0-1 scale #######
train.norm <- train.df
valid.norm <- valid.df

summary(train.df)

cols <- colnames(train.df)
for (i in cols) {
  valid.norm[[i]] <- (valid.norm[[i]] - min(train.df[[i]])) / (max(train.df[[i]]) - min(train.df[[i]]))
  train.norm[[i]] <- (train.norm[[i]] - min(train.df[[i]])) / (max(train.df[[i]]) - min(train.df[[i]]))
}

####### k-NN for weather data with k=1 #######
library(FNN)
full.nn <- knn(train = train.norm[, -c(1)],
                  test = valid.norm[, -c(1)],
                  cl = train.norm$Survived,
                  k = 1)
library(caret)
confusionMatrix(full.nn, as.factor(valid.norm$Survived), positive = "1")

####### finding optimal k for weather data #######
accuracy.df <- data.frame(k = seq(1, 40, 1), accuracy = rep(0, 40))
for (i in 1:40) {
  knn.pred <- knn(train.norm[,-1], 
                  valid.norm[,-1], 
                  train.norm$Survived, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(valid.norm$Survived))$overall[1]
}
accuracy.df

accuracy.df[accuracy.df$accuracy == max(accuracy.df$accuracy), ]

full.nn.best <- knn(train.norm[,-1],
                       valid.norm[,-1], 
                       train.norm$Survived, 
                       k=16)
confusionMatrix(full.nn.best, 
                as.factor(valid.norm$Survived), 
                positive = "1")

