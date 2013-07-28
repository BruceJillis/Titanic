ftable(UCBAdmissions, col.vars="Admit")

dimnames(UCBAdmissions)

margin.table(UCBAdmissions, c(2,1))

ucb.df = data.frame(gender=rep(c("Male","Female"),c(6,6)), dept=rep(LETTERS[1:6],2), yes=c(512,353,120,138,53,22,89,17,202,131,94,24), no=c(313,207,205,279,138,351,19,8,391,244,299,317))
ucb.df

mod.form = "cbind(yes,no) ~ gender * dept"
glm.out = glm(mod.form, family=binomial(logit), data=ucb.df)

# turn off significance stars (optional)
options(show.signif.stars=F)

anova(glm.out, test="Chisq")
# This is a saturated model, meaning we have used up all our degrees of freedom, and there is no residual deviance left over at the end. Saturated models always fit the data perfectly.

summary(glm.out)
# antilog of the genderMale coefficient
exp(glm.out$coeff['genderMale'])
# All else being equal, the odds of female being admitted were 2.86 times the odds of a male being admitted.
# odss -> the ratio of two probabilities
# 
#           p(one outcome)       p(success)    p
# odds = -------------------- = ----------- = ---, where q = 1 - p
#        p(the other outcome)    p(failure)    q
1/exp(glm.out$coeff['genderMale'])