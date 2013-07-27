set.seed(42)

train <- read.csv('data/train.csv', sep=',', na.strings=c(''))
train$Survived <- as.factor(train$Survived)

summary(train)

model <- glm(
	Survived ~ Pclass + Sex + Age + SibSp + Parch + Pclass:Sex + Pclass:Age + Age:Sex,
	data=clean, 
	family="binomial"
)
summary(model)
confint(model)

test <- read.csv('test-clean.csv', sep=';')
test$Survived <- predict(model, newdata=test, type="response")
test$Survived = round(test$Survived)

write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)