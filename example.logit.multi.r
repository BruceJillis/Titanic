# See for context: 
#   http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html

# Logistic Regression: Multiple Numerical Predictors

file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
# huh, you can turn it around as well.. could be handy
read.csv(file) -> gorilla
str(gorilla)

# correlation matrix
cor(gorilla) 

# or another way
with(gorilla, tapply(W, seen, mean))
with(gorilla, tapply(C, seen, mean))
with(gorilla, tapply(CW, seen, mean))

# ^ no significant relations to be found

glm.out = glm(seen ~ W * C * CW, family=binomial(logit), data=gorilla)
# check out regression coefficients with standard errors and a z-test
summary(glm.out)
  # None of the coefficients are significantly different from zero (but a few are close)
  # The deviance was reduced by 8.157 points on 7 degrees of freedom, for a p-value of
	1 - pchisq(65.438 - 57.281, df=7)
  # Overall, the model appears to have performed poorly, showing no significant reduction in deviance (no significant difference from the null model).

# The second print out shows the same overall reduction in deviance, from 65.438 to 57.281 on 7 degrees of freedom. 
anova(glm.out, test="Chisq")
# In this print out, however, the reduction in deviance is shown for each term, added sequentially first to last. 
# Of note is the 3-way interaction term, which produced a nearly significant reduction in deviance of 3.305 on 1 degree of freedom (p = 0.069).

# plot 
plot(glm.out$fitted)
abline(v=30.5,col="red")
abline(h=.3,col="green")
abline(h=.5,col="green")
text(15,.9,"seen = 0")
text(40,.9,"seen = 1")