
### Spam Logistic Regression

# plot the sigmoidal curve
png("sigmoidalCurve.png", width=4, height=4, units="in", res=720)
z <- seq(-6,6,length=100)
plot(z, exp(z)/(1+exp(z)), col="navy", type="l", lwd=1.5, bty="n", yaxt="n")
axis(2, at=c(0,0.5,1), las=1)
abline(h=0, lty=3, col="grey40")
abline(h=1, lty=3, col="grey40")
dev.off()

spammy<- read.csv("Spam.csv")
spammy[c(1,4000), c(16,56,58)]
spamFit <- glm(spam ~ ., data=spammy, family='binomial')
summary(spamFit)
1-spamFit$deviance/spamFit$null.deviance #R-squared

coef(spamFit)["word_free"]
exp(1.542706)

coef(spamFit)["word_george"]
exp(-5.779841)
1/exp(-5.779841)

# can use the model for predictions
predict(spamFit, newdata=spammy[c(1,4000),]) #not probabilities
predict(spamFit,newdata=spammy[c(1,4000),],type="response") #probabilities

# fit plot
png("spamFit.png", width=4.5, height=4.5, units="in", res=720)
plot(spamFit$fitted~factor(spammy$spam, levels=0:1, labels=c("true important", "true spam")),
	xlab="", ylab=c("fitted probability of spam"), col=c("blue","red"), bty="n")
dev.off()

predict(spamFit,newdata=spammy[c(1,4000),],type="response") #probabilities
