###### Regression

### Dominick's OJ Regression
oj<-read.csv("OJ.csv",strings=T) #strings=T makes character into factor
head(oj)

# create some colors for the brands then plot
brandcol <- c("green","red","gold")
par(mfrow=c(1,2))
plot(log(price) ~ brand, data=oj, col=brandcol)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand])

# simple log-log plus brand regression
fit<-glm(log(sales) ~ brand + log(price),data=oj)  
coef(fit)

# scatterplot with lines
beta <- coef(fit)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
	cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[4], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[2], b=beta[4], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[3], b=beta[4], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))

# exploring model matrix
oj[c(100,200,300),]
x <- model.matrix( ~ log(price) + brand, data=oj)
x[c(100,200,300),]

# add interaction
# note that '*' also adds the main effects automatically
fit2way <- glm(log(sales) ~ log(price)*brand, data=oj)
coef(fit2way)

# compare brand-specific log(price) slopes to our earlier elasticity (-3.1)
beta<-coef(fit2way)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
	cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[3], b=beta[2]+beta[5], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2]+beta[6], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))

# elasticities for 2 way interaction model
b <- coef(fit2way)
b["log(price)"]
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]


# and finally, consider 3-way interactions
fit3way <- glm(log(sales) ~ log(price)*brand*feat, data=oj)
coef(fit3way)

# making predictions
# create some data for prediction, using the data.frame function
# note the care in specifying brand factor (levels must match original data)
# we don't need all variables in oj; just those used as covariates in reg.
newdata=data.frame(price=rep(4,3), 
	brand=factor(c("tropicana","minute.maid","dominicks"),levels=levels(oj$brand)),
	feat=rep(1,3))
# predict
predict(fit3way, newdata=newdata)  ## predicted log units moved
exp(predict(fit3way, newdata=newdata)) ## predicted # of units moved

# for the elasticities table
b <- coef(fit3way)
b["log(price)"] 
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]
b["log(price)"] + b["log(price):feat"] 
b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"]
b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"]

# table explaining why ads confounded our brand elasticity estimates
oj$logmove<-log(oj$sales)
salestable <- tapply(exp(oj$logmove), oj[,c("feat","brand")], sum)
mosaicplot(salestable,col=brandcol)


# fit plots and R^2 
# (the 'bty="n"' option removes boxes around your plot)
plot(fit3way$fitted ~ oj$logmove, col=brandcol[oj$brand], bty="n")
abline(a=0,b=1)#  add a line with slope 1, intercept 0
legend("topleft",legend=levels(oj$brand),fill=brandcol, bty="n")
cor(fit3way$fitted,oj$logmove)^2

# Model fit statistics
summary(fit3way)
1-fit3way$deviance/fit3way$null.deviance #1 - 13975/30079




