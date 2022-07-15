library('ggplot2') 
library('ggthemes') 
library('scales')
library('dplyr')
library('mice')
library('randomForest') 
library(tidyverse)
library(visdat)

full <- read.csv("train 2.csv", stringsAsFactors = F)
full <- full[,c(2:12)]
full$Cabin [full$Cabin == 'missing'] <- NA
full$Embarked [full$Embarked == 'missing'] <- NA
full$Age [full$Age == "missing"] <- NA
full$Age <- as.numeric(full$Age)

summary(full)
sum(is.na(full))
colMeans(is.na(full))*100
vis_miss(full)

full <- full[,c(1:2, 4:7, 9, 11)]
#Eliminate NA value in embarked
full <- full[-c(62,830),]

summary(full)

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 4 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize >= 4] <- 'large'

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

####### correlation matrix for numeric variables #######
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, full), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, full), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


####### histograms #####
par(mfrow = c(3,2))
hist(full$Pclass, xlab = "Pclass", main = "Histogram of Pclass", 
     col = "blue")
hist(full$Age, xlab = "Age", main = "Histogram of Age", 
     col = "blue")
hist(full$SibSp, xlab = "SibSp", main = "Histogram of SibSp", 
     col = "blue")
hist(full$Parch, xlab = "Parch", main = "Histogram of Parch", 
     col = "blue")
hist(full$Fare, xlab = "Fare", main = "Histogram of Fare", 
     col = "blue")
hist(full$Fsize, xlab = "Fsize", main = "Histogram of Fsize", 
     col = "blue")


# Make variables factors into factors
factor_vars <- c('Pclass','Sex','Embarked','Family','FsizeD', 'Adulthood')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

full[which(is.na(full$Age )), ]

####### Barplot #####

#Pclass
CountPclassSurv <- aggregate(full$Age, 
                               by = list(full$Pclass, full$Survived),
                               FUN = length,
                               drop = FALSE)
names(CountPclassSurv) <- c("Pclass", "Survived", "People")
par(mfcol = c(2,1))
barplot(height = CountPclassSurv$People[CountPclassSurv$Survived == "0"], 
        names.arg = CountPclassSurv$Pclass[CountPclassSurv$Survived == "0"], 
        col = "blue",
        las = 1,
        ylab = "People", xlab = "Ticket class", main = "Survived = No")
barplot(height = CountPclassSurv$People[CountPclassSurv$Survived == "1"], 
        names.arg = CountPclassSurv$Pclass[CountPclassSurv$Survived == "1"], 
        col = "blue",
        las = 1,
        ylab = "People", xlab = "Ticket class", main = "Survived = Yes")

#Sex
CountSexSurv <- aggregate(full$Age, 
                             by = list(full$Sex, full$Survived),
                             FUN = length,
                             drop = FALSE)
names(CountSexSurv) <- c("Sex", "Survived", "People")
par(mfcol = c(2,1))
barplot(height = CountSexSurv$People[CountSexSurv$Survived == "0"], 
        names.arg = CountSexSurv$Sex[CountSexSurv$Survived == "0"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = No")
barplot(height = CountSexSurv$People[CountSexSurv$Survived == "1"], 
        names.arg = CountSexSurv$Sex[CountSexSurv$Survived == "1"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = Yes")

#Embarked
CountEmbSurv <- aggregate(full$Age, 
                          by = list(full$Embarked, full$Survived),
                          FUN = length,
                          drop = FALSE)
names(CountEmbSurv) <- c("Embarked", "Survived", "People")
par(mfcol = c(2,1))
barplot(height = CountEmbSurv$People[CountEmbSurv$Survived == "0"], 
        names.arg = CountEmbSurv$Embarked[CountEmbSurv$Survived == "0"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = No")
barplot(height = CountEmbSurv$People[CountEmbSurv$Survived == "1"], 
        names.arg = CountEmbSurv$Embarked[CountEmbSurv$Survived == "1"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = Yes")



#Family
CountFamSurv <- aggregate(full$Age, 
                          by = list(full$Family, full$Survived),
                          FUN = length,
                          drop = FALSE)
names(CountFamSurv) <- c("Family", "Survived", "People")
par(mfcol = c(2,1))
barplot(height = CountFamSurv$People[CountFamSurv$Survived == "0"], 
        names.arg = CountFamSurv$Family[CountFamSurv$Survived == "0"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = No")
barplot(height = CountFamSurv$People[CountFamSurv$Survived == "1"], 
        names.arg = CountFamSurv$Family[CountFamSurv$Survived == "1"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = Yes")

#FsizeD
CountFDSurv <- aggregate(full$Age, 
                          by = list(full$FsizeD, full$Survived),
                          FUN = length,
                          drop = FALSE)
names(CountFDSurv) <- c("FsizeD", "Survived", "People")
par(mfcol = c(2,1))
barplot(height = CountFDSurv$People[CountFDSurv$Survived == "0"], 
        names.arg = CountFDSurv$FsizeD[CountFDSurv$Survived == "0"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = No")
barplot(height = CountFDSurv$People[CountFDSurv$Survived == "1"], 
        names.arg = CountFDSurv$FsizeD[CountFDSurv$Survived == "1"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = Yes")


#adulthood
CountAdultSurv <- aggregate(full$Age, 
                         by = list(full$Adulthood, full$Survived),
                         FUN = length,
                         drop = FALSE)
names(CountAdultSurv) <- c("Adulthood", "Survived", "People")
par(mfcol = c(2,1))
barplot(height = CountAdultSurv$People[CountAdultSurv$Survived == "0"], 
        names.arg = CountAdultSurv$Adulthood[CountAdultSurv$Survived == "0"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = No")
barplot(height = CountAdultSurv$People[CountAdultSurv$Survived == "1"], 
        names.arg = CountAdultSurv$Adulthood[CountAdultSurv$Survived == "1"], 
        col = "blue",
        las = 1,
        ylab = "People", main = "Survived = Yes")


####### Barplot #####
par(mfrow = c(1,1))
boxplot(full$Age ~ full$Survived, 
        ylab = "Age", xlab = "Survived")
boxplot(full$Fare ~ full$Survived, 
        ylab = "Fare", xlab = "Survived")

par(mfrow = c(1,3))
boxplot(full$SibSp ~ full$Survived, 
        ylab = "Number of Siblings or Spouses", xlab = "Survived")
boxplot(full$Parch ~ full$Survived, 
        ylab = "Number of Parents or children", xlab = "Survived")
boxplot(full$Fsize ~ full$Survived, 
        ylab = "Family Size", xlab = "Survived")

####### scatter plot matrix #####
library(GGally)
ggpairs(Filter(is.numeric, full))
