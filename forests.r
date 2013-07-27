# # edit and fix the data
# fix(train)

# # accesss documentation
# ?read.csv
# help(read.csv)


# train <- read.csv('train.csv', sep=',')
# # print structure
# str(train)
# summary(train)

library(randomForest)
set.seed(42)

clean <- read.csv('train-clean.csv', sep=';')
clean$Survived <- as.factor(clean$Survived)

model <- randomForest(
	Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Pclass:Sex + Pclass:Age + Age:Sex, 
	data=clean,
	ntree=20000
)

test <- read.csv('test-clean.csv', sep=';')
test$Survived <- predict(model, newdata=test, type="response")

write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=F, quote=FALSE)