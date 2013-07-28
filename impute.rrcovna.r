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
train.imp_seq = impSeq(train)
summary(train.imp_seq)
str(train.imp_seq)

# Impute missing multivariate data using robust sequential algorithm using explicit default alpha
# we need to get the x column of the returned result to obtain the imputed dataframe
train.imp_seq_rob = impSeqRob(train, alpha=0.9)$x
summary(train.imp_seq_rob)
str(train.imp_seq_rob)

