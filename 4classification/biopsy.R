####### Classification #######
####### Biopsy Data #######

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

## data prep
biopsyNM<-na.omit(biopsy) #remove rows with missing data
x <- scale(biopsyNM[,2:10]) # scale data
apply(x,2,sd)
class(biopsy)
class(x)
y <- biopsyNM$class
table(y)

set.seed(0)
test <- sample(1:nrow(x),400)
library(class)
K1 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=1)
K10 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=10)
K50 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=50)
K100 <- knn(train=x[-test,], test=x[test,], cl=y[-test], k=100)

res<-data.frame(y[test],K1,K10,K50,K100)
res[1:10,]
( apply(res[,-1], 2, function(c) mean(c==res[,1])) )

## confusion matrix for K=10
table(pred=K10,actual=y[test])
