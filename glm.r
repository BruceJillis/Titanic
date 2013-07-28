set.seed(42)

train <- read.csv('data/train.csv', sep=',', na.strings=c(''))

# drop some rows
train$Cabin <- NULL
train$Ticket <- NULL
train$Name <- NULL

# mark some rows as nominal
train$Sex <- as.factor(train$Sex)

summary(train)

model <- glm(
	Survived ~ Pclass + Sex + Age + SibSp + Parch + Pclass:Sex + Pclass:Age + Age:Sex,
	data=train, 
	family="binomial"
)
summary(model)
confint(model)

test <- read.csv('data/test.csv', sep=',')

test$Cabin <- NULL
test$Ticket <- NULL
test$Name <- NULL

test$Sex <- as.factor(test$Sex)

test$Survived <- predict(model, newdata=test, type="response")
test$Survived = round(test$Survived)

write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)