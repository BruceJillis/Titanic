set.seed(42)

# read training data

# note the na.strings so that the 1 empty entry is picked up as an NA
train <- read.csv('data/train.csv', sep=',', na.strings=c(''))[1:10,]
# drop some columns
train <- subset(train, select = -c(Cabin, Ticket, Name, Fare))
# correct some column types
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Survived <- as.factor(train$Survived)
summary(train)

library(Hmisc)
train.imp = train

# the random values are not forced to be the same if there are multiple NAs
train.imp$Age <- impute(train$Age, 'random') 
# alt syntax
train.imp$Age <- impute(train$Age, fun=median)
train.imp$Age <- impute(train$Age, min)
train.imp$Age <- impute(train$Age, max)
train.imp$Age <- impute(train$Age, -10)
train.imp$Age <- impute(train$Age, mean)

# or using with
#   train.imp$Age <- with(train, impute(Age, mean))

train.imp$Embarked <- impute(train$Embarked, median)
# we can also add a new level and it will be added to the levels
train.imp$Embarked <- impute(train$Embarked, 'nowhere')

# checking for imputation
print(is.imputed(train$Embarked))
print(is.imputed(train.imp$Embarked))

summary(train.imp)
str(train.imp)

