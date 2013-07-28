# See for context: 
#   http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html

# Logistic Regression: One Numerical Predictor

library("MASS")
data(menarche)
str(menarche)
summary(menarche)

plot(Menarche / Total ~ Age, data=menarche)

glm.out = glm(
	cbind(Menarche, Total - Menarche) ~ Age,
	family=binomial(logit), 
	data=menarche
)

plot(Menarche / Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")

# Recall that the response variable is log odds, so the coefficient of "Age" can be interpreted as "for every one year increase in age the odds of having reached menarche increase by exp(1.632) = 5.11 times."
summary(glm.out)
# also available: glm.out$coef, glm.out$fitted, glm.out$resid, glm.out$effects, anova(glm.out)

# calculate p-value (NOTE. pchisq uses the cumulative distribution! so adjust accordingly)
p = 1 - pchisq(26.703, df=23) # should be ~ 0.269

# Logistic Regression: Multiple Numerical Predictors

