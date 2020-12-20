
### Spam Logistic Regression
spammy<- read.csv("Spam.csv")
spammy[c(1,4000), c(16,56,58)]
spammyFit <- glm(spam ~ ., data=spammy, family='binomial')
summary(spammyFit)
1-spammyFit$deviance/spammyFit$null.deviance #R-squared

coef(spammyFit)["word_free"]
exp(1.542706)

coef(spammyFit)["word_george"]
exp(-5.779841)
1/exp(-5.779841)

# can use the model for predictions
predict(spammyFit, newdata=spammy[c(1,4000),]) #not probabilities
predict(spammyFit,newdata=spammy[c(1,4000),],type="response") #probabilities

# fit plot
boxplot(fitted(spammyFit)~spammy$spam, 
	xlab="", ylab=c("fitted probability of spam"), 
	col=c("navy","red"))

