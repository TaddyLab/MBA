###################################
### Classification
###################################

# KNN
library(MASS)
data(biopsy)
str(biopsy)

par(mfrow=c(3,3))
for(i in 2:10){
main<-paste("",colnames(biopsy)[i],"")
par(mai=c(0.3,0.3,0.4,0.3))
boxplot(biopsy[,i]~biopsy[,11],ylab="",main=main,col=c("blue","deeppink1"),
	xlab="")
}

library(class)
biops<-na.omit(biopsy) #remove rows with missing data
x <- scale(biops[,2:10]) # scale data
apply(x,2,sd)
y <- biops$class
tstSz<-50
test <- sample(1:nrow(x),tstSz)
library(class)
nearest1 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=1)
nearest3 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=3)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=5)
nearest25 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=25)
(res<-data.frame(y[test],nearest1,nearest3,nearest5,nearest25)[1:10,])
sum((as.numeric(y[test])-as.numeric(nearest1))==0)/tstSz
sum((as.numeric(y[test])-as.numeric(nearest3))==0)/tstSz
sum((as.ntableumeric(y[test])-as.numeric(nearest5))==0)/tstSz
sum((as.numeric(y[test])-as.numeric(nearest25))==0)/tstSz

sum((as.numeric(y[test])-as.numeric(nearest3))==1)
sum((as.numeric(y[test])-as.numeric(nearest3))==-1)


# run KNN many times to see how the results differ
k1<-c(); k3<-c(); k5<-c(); k25<-c();mxMin<-c()
for(i in 1:10000){
test <- sample(1:nrow(x),tstSz)
nearest1 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=1)
nearest3 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=3)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=5)
nearest25 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=25)
k1[i]<-sum((as.numeric(y[test])-as.numeric(nearest1))==0)/tstSz
k3[i]<-sum((as.numeric(y[test])-as.numeric(nearest3))==0)/tstSz
k5[i]<-sum((as.numeric(y[test])-as.numeric(nearest5))==0)/tstSz
k25[i]<-sum((as.numeric(y[test])-as.numeric(nearest5))==0)/tstSz
mxMin[i]<-max(k1[i],k3[i],k5[i],k25[i])-min(k1[i],k3[i],k5[i],k25[i])
}
summary(k1);summary(k3);summary(k5);summary(k25)
summary(mxMin)


## German Credit
credit <- read.csv("credit.csv",strings=T)
str(credit)

## build a design matrix 
library(gamlr)
source("naref.R") #functions to make NA ref level
credx <- sparse.model.matrix( Default ~ .^2, data=naref(credit))[,-1]
default <- credit$Default
credScore <- cv.gamlr(credx, default, family="binomial", verb=TRUE)

par(mfrow=c(1,2))
plot(credScore$gamlr)
plot(credScore)

sum(coef(credScore, s="min")!=0) # min
sum(coef(credScore)!=0) # 1se
sum(coef(credScore$gamlr)!=0) # AICc
sum(coef(credScore$gamlr,select=which.min(BIC(credScore$gamlr)))!=0) #BIC
sum(coef(credScore$gamlr,select=which.min(AIC(credScore$gamlr)))!=0) #AIC

# the OOS R^2
1 - credScore$cvm[credScore$seg.min]/credScore$cvm[1] #for CV-min

## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credScore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="Default", ylab="Probability of Default", col=c("blue","green"))

## what are our misclassification rates?
rule <- 1/5 # move this around to see how these change

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity

sum( (pred>rule)[default==1] )/sum(pred>rule) ## PPV
sum( (pred<rule)[default==0] )/sum(pred<rule) ## NPV

# confusion matrix
library(DescTools)
Rev(table(nearest3,y[test]),c(1,2))
           
# OOS ROC curve
# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]

## roc curve and fitted distributions
source("roc.R")

par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=default, bty="n", main="in-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
	y=mean((pred>.2)[default==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
	y=mean((pred>.5)[default==1]), 
	cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
	legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

roc(p=predoos, y=defaultoos, bty="n", main="out-of-sample")
## our 1/5 rule cutoff
points(x= 1-mean((predoos<.2)[defaultoos==0]), 
	y=mean((predoos>.2)[defaultoos==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[defaultoos==0]), 
	y=mean((predoos>.5)[defaultoos==1]), 
	cex=1.5, pch=20, col='blue')

## plot a mosaic
par(mai=c(.8,.8,.1,.1))
plot(factor(Default) ~ history, data=credit, col=c(8,2),xlab="History",ylab="Default") ## surprise!
## the dangers of choice-based sampling!  
 
## A lasso penalized multinomial regression model
library(Matrix)
dtmRevCH<-readMM("dtmRevCH.txt") #writing sparse matrices using readMM() doesn't write row and column names
colNames<-as.character(read.table("colnamesRevCH.txt")[,1]) 
rowNames<-as.character(read.table("rownamesRevCH.txt")[,1])
dtmRevCH@Dimnames[[1]]<-rowNames
dtmRevCH@Dimnames[[2]]<-colNames
dtmRevCH<-as(dtmRevCH,"dgCMatrix")
revCH<-read.csv("revCH.csv",strings=T)
stars<-as.factor(revCH$stars)

## running and plotting with glmnet
library(glmnet)
subset<-sample(1:nrow(dtmRevCH),10000)
x<-dtmRevCH[subset,]
y<-stars[subset]
start<-Sys.time()
yelpFit <- cv.glmnet(x, y, family="multinomial")
end<-Sys.time();end-start
plot(yelpFit)
win.graph()
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4)) ## note we can use xvar="lambda" to plot against log lambda
plot(yelpFit$glm, xvar="lambda") 


#log lambdas and cvm 
log(round(c(yelpFit$lambda.min,yelpFit$lambda.1se),3))
yelpFit$lambda
yelpFit$cvm[c(31,34)]


#get predictions
# drop takes it from n x k x 1 array to n x k matrix
probYelp <- drop(predict(yelpFit, x, type="response"))
round((probYelp)[1:5,],3) #probabilities for each class

# use R to compute max prob for class assignment
as.data.frame(apply(probYelp,1,which.max)[1:5],nrow=5)

# combine the 2
mat<-data.frame(round((probYelp)[1:5,],3),apply(probYelp,1,which.max)[1:5])
colnames(mat)[6]<-"predClass"
mat

### fit plots: plot p_yi distribution for each true yi
# get the probs for what actually happened
# note use of a matrix to index a matrix! 
# gives back the [i,j] entry of probYelp for each row of index matrix 
# so, here, that's the probability of true class for each observation
n <- nrow(x)
trueClassMembership <- probYelp[cbind(1:n, y)] 
## plot true class, with varwidth to have the box widths proportional to response proportion.
plot(trueClassMembership ~ y, col="blue", varwidth=TRUE,
	xlab="Stars", ylab="prob(true class)") 

## extract coefficients
B  <- coef(yelpFit, select="min")
B # it's a list of coefficients, 1 matrix per glass type.
## combine into a matrix
B <- do.call(cbind, B)
## annoyingly, column names are dropped
colnames(B) <- levels(y) # add them back
round(B,3)[1:20,]

##interpreting coefficients
exp(B["poor","1"]-B["poor","2"]) 
exp(B["poor","1"]-B["poor","5"]) 
 
## classification
## looking at 5 stars vs all others (using 0.6 rule)
prob5<- probYelp[,'5'] > .6
sum(prob5)
## you can also employ all of the binary classification ideas
## whenever you are making one-vs-all-others type comparisons
## e.g., ROC plot 5 stars vs 4, 3, 2, or 1 star

## plot the ROC curve for classification of y with p
source("roc.R")
roc(p=probYelp[,'5'], y=y=="5", main="5 stars ROC")
rule<-0.6
points(x= 1-mean((probYelp[,'5']<rule)[stars[subset]!="5"]), 
	y=mean((probYelp[,'5']>rule)[stars[subset]=="5"]), 
	cex=1.5, pch=20, col='blue') 
rule<-0.2
points(x= 1-mean((probYelp[,'5']<rule)[stars[subset]!="5"]), 
	y=mean((probYelp[,'5']>rule)[stars[subset]=="5"]), 
	cex=1.5, pch=20, col='red') 
legend("bottomright",fill=c("red","blue"),
	legend=c("p=0.2","p=0.6"),bty="n",title="cutoff")

####  Moving to distrom
library(distrom)
detectCores()
cl = makeCluster(4)
cl

xDMR<-dtmRevCH
yDMR<-stars

#DMR
start<-Sys.time()
yelpDMR <- dmr(cl, xDMR, yDMR, verb=TRUE,lmr=1e-3)
end<-Sys.time();end-start

#CV DMR
start<-Sys.time()
yelpCvDMR <- dmr(cl, xDMR, yDMR, verb=TRUE, cv=TRUE, lmr=1e-3)
end<-Sys.time();end-start


names(yelpDMR)
yelpDMR[["1"]]

## plot a set of paths
par(mfrow=c(2,3))
for(k in names(yelpDMR)) plot(yelpDMR[[k]], main=k) 
## plot CVs
par(mfrow=c(2,3))
for(k in names(yelpCvDMR)) plot(yelpCvDMR[[k]], main=k) 
 
## and grab the AICc selected coefficients
bDMR <- coef(yelpDMR)
round(bDMR[1:20,],2)
## you'll notice these look different than glmnet's.
## it's because of the variable lambda (diff lambda accross classes),
## and because we're using AICc rather than CV selection.
## the predictions are roughly close though.

# check predicted values for yelpMNR against yelpDMR
pDMR <- predict(bDMR,xDMR,type="response")
plot(probYelp,pDMR[subset,],col=rep(1:6,each=n),xlab="glmnet",ylab="dmr",bty="n")
legend("topleft", fill=1:6, legend=levels(stars), bty="n",h=TRUE)










