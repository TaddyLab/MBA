####### Classification #######
#### ******* German Credit Data ******* ####

credit <- read.csv("credit.csv",strings=T)
credit[1,]
dim(credit)

## build a design matrix 
library(gamlr)
xcred <- sparse.model.matrix( Default ~ .^2, data=naref(credit))[,-1]
ycred <- credit$Default
# fit the model
credscore <- gamlr(xcred, ycred, family="binomial")
sum(coef(credscore)!=0) 
which.min(AICc(credscore))

## In sample probability estimates
pcred <- drop( predict(credscore, xcred, type="response") )

#png('credFit.png', width=4.5, height=4.5, units="in", res=720)
boxplot(pcred ~ ycred, xlab="Default", ylab="Probability of Default", col=c("pink","dodgerblue"))
#dev.off()

## what are our misclassification rates?
rule <- 1/5 # move this around to see how these change
yhat <- as.numeric(pcred>rule)
table(yhat, ycred)

source("roc.R")
#png('credROC.png', width=4.5, height=4.5, units="in", res=720)
roc(p=pcred, y=ycred, bty="n", main="in-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pcred<.2)[ycred==0]), 
	y=mean((pcred>.2)[ycred==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pcred<.5)[ycred==0]), 
	y=mean((pcred>.5)[ycred==1]), 
	cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
	legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
#dev.off()

# OOS ROC curve
# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(xcred[-test,], ycred[-test], family="binomial")
pcredoos <- predict(credhalf, xcred[test,], type="response")
ycredoos <- ycred[test]

## roc curve and fitted distributions
#png('credROCOOS.png', width=4.5, height=4.5, units="in", res=720)
roc(p=pcredoos, y=ycredoos, bty="n", main="out-of-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pcredoos<.2)[ycredoos==0]), 
	y=mean((pcredoos>.2)[ycredoos==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pcredoos<.5)[ycredoos==0]), 
	y=mean((pcredoos>.5)[ycredoos==1]), 
	cex=1.5, pch=20, col='blue') 
#dev.off()

## plot a mosaic
par(mai=c(.8,.8,.1,.1))
plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") ## surprise!
## the dangers of choice-based sampling!  






