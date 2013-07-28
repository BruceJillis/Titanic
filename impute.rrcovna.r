library(ggcovna)
set.seed(42)

# read training data
train <- read.csv('data/train.csv', sep=',', na.strings=c(''))
# drop some columns
train <- subset(train, select = -c(Cabin, Ticket, Name, Fare))
# correct some column types
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

# add explicit levels argument to make R use 0 and 1 as the levels instead of 1 and 2
train$Survived <- as.factor(train$Survived, levels=c(0,1))

summary(train)
str(train)

# impute missing multivariate data using sequential algorithm
train.imp = impSeq(train)
summary(train.imp)
str(train.imp)
