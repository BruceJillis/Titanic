set.seed(42)

clean <- read.csv('train-clean.csv', sep=';')
clean$Survived <- as.factor(clean$Survived)

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