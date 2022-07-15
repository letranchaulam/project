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

set.seed(5)
train.index <- sample(rownames(full), nrow(full) * 0.15)
train.data <-full[train.index, ]
valid.index <- setdiff(rownames(full), train.index)
valid.data <- full[valid.index, ]

####### full logistic regression model #######
full.glm <- glm(Survived ~ ., family = "binomial", data = train.data)
options(scipen=999, digits=7)
summary(full.glm)
dim(summary(full.glm)$coefficients)[1]-1
AIC(full.glm)
BIC(full.glm)
deviance(full.glm)

# out-of-sample prediction
library(forecast)
pred.glm.valid <- predict(full.glm, newdata = valid.data, type = "response")

# create the confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(pred.glm.valid >= 0.5, "1", "0")), as.factor(valid.data$Survived), 
                positive = "1")

# ROC curve
par(mfrow = c(1,1))
library(pROC)
r <- roc(valid.data$Survived, pred.glm.valid)
plot.roc(r)
auc(r)

####### odds coefficients #######
options(scipen=5, digits=7)
data.frame(summary(full.glm)$coefficient, odds = exp(coef(full.glm)))


####### forward selection #######
full.glm.null <- glm(Survived ~ 1, data = train.data, family = "binomial")
full.fwd <- step(full.glm.null, scope = list(full.glm.null, upper = full.glm), direction = "forward")

summary(full.fwd)
dim(summary(full.fwd)$coefficients)[1]-1
AIC(full.fwd)
BIC(full.fwd)
deviance(full.fwd)
pred.fwd.valid <- predict(full.fwd, newdata = valid.data, type = "response")
r <- roc(valid.data$Survived, pred.fwd.valid)
par(mfcol = c(1,1))
plot.roc(r)
auc(r)
confusionMatrix(as.factor(ifelse(pred.fwd.valid >= 0.5, "1", "0")), as.factor(valid.data$Survived), 
                positive = "1")

####### backward elimination #######
full.back <- step(full.glm, direction = "backward")
summary(full.back)
dim(summary(full.back)$coefficients)[1]-1
AIC(full.back)
BIC(full.back)
deviance(full.back)
pred.back.valid <- predict(full.back, newdata = valid.data, type = "response")
par(mfcol = c(1,1))
r <- roc(valid.data$full, pred.back.valid)
plot.roc(r)
auc(r)
par(mfcol = c(1,2))
confusionMatrix(as.factor(ifelse(pred.back.valid >= 0.5, "1", "0")), as.factor(valid.data$Survived), 
                positive = "1")

####### stepwise regression #######
full.step <- step(full.glm.null, scope = list(full.glm.null, upper = full.glm), direction = "both")
summary(full.step)
dim(summary(full.step)$coefficients)[1]-1
AIC(full.step)
BIC(full.step)
deviance(full.step)
pred.step.valid <- predict(full.step, newdata = valid.data, type = "response")
r <- roc(valid.data$Survived, pred.step.valid)
par(mfcol = c(1,1))
plot.roc(r)
auc(r)
confusionMatrix(as.factor(ifelse(pred.step.valid >= 0.5, "1", "0")), as.factor(valid.data$Survived), 
                positive = "1")
