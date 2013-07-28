set.seed(42)

# read training data

# note the na.strings so that the 1 empty entry is picked up as an NA
train <- read.csv('data/train.csv', sep=',', na.strings=c(''))
# drop some columns
train <- subset(train, select = -c(Cabin, Ticket, Name, Fare))
# correct some column types
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Survived <- as.factor(train$Survived)

library(Hmisc)
train.imp = train

train.imp$Age <- impute(train$Age, 'random')
train.imp$Age <- impute(train$Age, median)
train.imp$Age <- impute(train$Age, min)
train.imp$Age <- impute(train$Age, max)
train.imp$Age <- impute(train$Age, -10)
train.imp$Age <- impute(train$Age, mean)
# or using with
#   train.imp$Age <- with(train, impute(Age, mean))
train.imp$Embarked <- impute(train.imp$Embarked, median)

custom <- function(x) {
	m = is.na(x)
	return (rep(1:5, each=sum(m)))
}
custom(train$Age)
train.imp$Age <- impute(train$Age, custom(train$Age))


summary(train.imp)

