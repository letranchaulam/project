library('ggplot2') 
library('ggthemes') 
library('scales')
library('dplyr')
library('mice')
library('randomForest') 

train <- read.csv("train.csv", stringsAsFactors = F)
test  <- read.csv("test.csv", stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

str(full)

 

#passenger title is contained within the passenger name variable and we can use surname to represent families. 
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex 
table(full$Sex, full$Title)

full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize <= 4 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
#there's a survival penalty among singletons and large families, but a benefit for passengers in small families.

# Create a Deck variable.
full$Cabin[1:28]
strsplit(full$Cabin[2], NULL)[[1]]

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
print(full$Deck)

#find missing values in Fare and embarkment
full$Fare[full$Fare == "NA"] <- NA

full[which(is.na(full$Embarked )), ]
#62 and 830
full[which(is.na(full$Fare )), ]
#1044

# Passengers 62 and 830 are missing Embarkment
#infer their values for embarkment based on passenger class and fare.
# Get rid of our missing passenger IDs

embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)


# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

#fill in values with c
full$Embarked[c(62, 830)] <- 'C'

#1044 is class 3 embarked from s
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='grey', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


# Make variables factors into factors
# Show number of missing Age values
sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

mice_output <- complete(mice_mod)


full$Age <- mice_output$Age

full[which(is.na(full$Age )), ]

#  the relationship between age & survival & sex
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex) + 
  theme_few()

 

#differentiate child and childre
full$Adulthood[full$Age < 18] <- 'Child'
full$Adulthood[full$Age >= 18] <- 'Adult'


table(full$Adulthood, full$Survived)
#add mother variable in the pridiction
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

table(full$Mother, full$Survived)

full$Adulthood  <- factor(full$Adulthood)
full$Mother <- factor(full$Mother)


#split the data again

train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

train.df <- train[ , -c(1,4,9,11,14,18)]
train.df <- train.df[ , -c(11)]
## partitioning into training (70%) and validation (30%) 
train.rows <- sample(rownames(train.df), nrow(train.df)*0.7)
train.data <- train.df[train.rows, ]
valid.rows <- setdiff(rownames(train.df), train.rows)
valid.data <- train.df[valid.rows, ]
#full tree is not really useful
library(rpart)
library(rpart.plot)
full.tree <- rpart(factor(Survived) ~ ., data = train.data, cp = 0, minsplit = 2)
prp(full.tree, type = 1, extra = 1, varlen = -10, box.col = ifelse(full.tree$frame$var == "<leaf>", 'gray', 'white'))
length(full.tree$frame$var[full.tree$frame$var == "<leaf>"])
full.tree

#random forest
full.rf <- randomForest(factor(Survived) ~ ., data = train.df, ntree = 500,
                           mtry = 4, nodesize = 5, importance = TRUE)


# variable importance plot
varImpPlot(full.rf, type = 1)

full.rf.pred <- factor(predict(full.rf, valid.data))
library(caret)

#confusionMatrix  
valid.data$Survived <- factor(valid.data$Survived)

result <- confusionMatrix(data=full.rf.pred, reference =valid.data$Survived)
result






