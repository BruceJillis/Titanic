set.seed(42)

train <- read.csv('data/train.csv', sep=',')
# select subset of the data to keep it managable
train <- train[1:20,c(1,2,6,10)]
summary(train)

random.imp <- function (a){
	# simple random imputation
	missing <- is.na(a)
	n.missing <- sum(missing)
	a.obs <- a[!missing]
	imputed <- a
	imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
	return imputed
}

train.imp <- random.imp(train)
print(train.imp)