
## Yelp Lasso
# Call in Yelp review DTM (document term matrix)
library(Matrix)
dtmRevCH<-readMM("dtmRevCH.txt") #writing sparse matrices using readMM() doesn't write row and column names
colNames<-as.character(read.table("colnamesRevCH.txt")[,1]) 
rowNames<-as.character(read.table("rownamesRevCH.txt")[,1])

dtmRevCH@Dimnames[[1]]<-rowNames
dtmRevCH@Dimnames[[2]]<-colNames
dtmRevCH<-as(dtmRevCH,"dgCMatrix")
dim(dtmRevCH)
dtmRevCH[1:5,1:3]
str(dtmRevCH)#make sure it looks good 


# Exploring how the data is stored in this dgCMatrix
dtmRevCH@x[13149303:13149307] # last few entries for non-zero data
dtmRevCH[309415:309425,4605:4609] # matches the last entries of the last column of the matrix

# call in raw reviews
revCH<-read.csv("revCH.csv")

yelpX<-dtmRevCH  #this is already a sparse matrix without intercept and containing all factor levels
stars<-as.data.frame(revCH$stars)
yelpLasso<-gamlr(yelpX,stars,standardize=FALSE,lmr=1e-5)
plot(yelpLasso)

cvYelp<-cv.gamlr(yelpX,stars,standardize=FALSE,standardize=FALSE)
plot(cvYelp)

cvYelpLmr<-cv.gamlr(yelpX,stars,lmr=1e-5)
plot(cvYelpLmr)

# get the segment numbers for 1se and CV-min rule
cvYelpLmr$seg.1se 
cvYelpLmr$seg.min
 
# check the OOS R-squared
1-cvYelpLmr$cvm[cvYelpLmr$seg.1se]/cvYelpLmr$cvm[1]
1-cvYelpLmr$cvm[cvYelpLmr$seg.min]/cvYelpLmr$cvm[1]

beta1SE<-coef(cvYelpLmr) #1SE rule
betaMin<-coef(cvYelpLmr, select="min") 
b1SEMat<-cbind(beta1SE@Dimnames[[1]][which(beta1SE!=0)],beta1SE[which(beta1SE!=0)])
bMinMat<-cbind(betaMin@Dimnames[[1]][which(betaMin!=0)],betaMin[which(betaMin!=0)])
b1SEMat[b1SEMat[,1]=="fantastic"]
b1SEMat[b1SEMat[,1]=="terrible"]
bMinMat[bMinMat[,1]=="fantastic"]
bMinMat[bMinMat[,1]=="terrible"]


## Information Criteria
AIC(scLasso) 
scBeta <- coef(scLasso)[-1,] 
sum(scBeta!=0) 

log(scLasso$lambda[which.min(AICc(scLasso))]) #log(lambda) for AICc
log(scLasso$lambda[which.min(AICc(scLasso))]/4) #log(lambda/4)
log(scLasso$lambda) #look for seg closest to -5.9 
log(scLasso$lambda[54]) #yep
probs <- drop(predict(scLasso, scX, select=54,type="response")) 

# Bootstrap for uncertainty quantification for Lasso estimates

# for SC Lasso
bHatSC<-coef(scLasso)
bParBoot<-sparseMatrix(dims=c(nrow(bHatSC),0),i={},j={})
B<-100
for(b in 1:B){
	yB<-rbinom(nrow(scX),1,probs)
	fitB<-gamlr(scX,yB,family="binomial")
	bParBoot<-cbind(bParBoot,coef(fitB))
  }

signal <-"SIG50"
fB <- exp(bHatSC[signal,]) #f(b) = b
critVal <- quantile(exp(bParBoot[signal,]), c(.95,.05))
2*fB - critVal  

# for Yelp Lasso
log(yelpLasso$lambda[which.min(AICc(yelpLasso))]) #log(lambda) for AICc
log(yelpLasso$lambda[which.min(AICc(yelpLasso))]/4) #log(lambda/4)
log(yelpLasso$lambda) #look for seg closest to -11.13 
log(yelpLasso$lambda[85]) #yep
preds <- drop(predict(yelpLasso, yelpX,select=85))

numPar<-yelpLasso$df[85] 
SSE<-sum((stars-preds)^2)
RMSE<-sqrt(SSE/(nrow(yelpX)-numPar))
bHat<-coef(yelpLasso)
bParBoot<-sparseMatrix(dims=c(nrow(bHat),0),i={},j={})
B<-10 
for(b in 1:B){
yB<-rnorm(nrow(yelpX),preds,RMSE)
fitB<-gamlr(yelpX,yB,standardize=FALSE,lmr=1e-5)
bParBoot<-cbind(bParBoot,coef(fitB))
  }

word<-"fantastic"
fB <- bHat[word,] #f(b) = b
critVal <- quantile(bParBoot[word,], c(.95,.05))
2*fB - critVal

word <- "terrible"
fB <- bHat[word,] #f(b) = b
critVal <- quantile(bParBoot[word,], c(.95,.05))
2*fB - critVal


# subsampling
n <- nrow(scX)
B <- 100
( m <- round(n/4) )

eSubs <- sparseMatrix(dims=c(nrow(bHatSC),0),
    i={},j={})
for(b in 1:B){
	subs <- sample.int(n, m)
	fitB <- gamlr(scX[subs,],scY[subs],family="binomial")
	eB <- exp(coef(fitB)) - exp(bHatSC)
	eSubs <- cbind(eSubs,  eB)
	print(b)
}

signal<-"SIG50"
thetaHat <- exp(coef(scLasso)[signal,])
critVals <- quantile(eSubs[signal,], c(.95,.05))
thetaHat - critVals*sqrt(m)/sqrt(n)

# subsampling for Yelp Lasso
n <- nrow(yelpX)
B <- 100
( m <- round(n/8) )
eSubs <- sparseMatrix(dims=c(nrow(bHat),0),i={},j={})

start<-Sys.time()
for(b in 1:B){
	subs <- sample.int(n, m)
	fitB <- gamlr(yelpX[subs,],stars[subs,], 
		free=1,standardize=FALSE,lmr=1e-4)
	print(log(fitB$lambda[which.min(AICc(fitB))]))
	eB <- (exp(coef(fitB)) - exp(bHat))
	eSubs <- cbind(eSubs,  eB)
	print(b)
}
end<-Sys.time()
end-start

word<-"fantastic"
thetaHat <- coef(yelpLasso)[word,]
critVals <- quantile(eSubs[word,], c(.95,.05))
thetaHat - critVals*sqrt(m)/sqrt(n)

word<-"terrible"
thetaHat <- coef(yelpLasso)[word,]
critVals <- quantile(eSubs[word,], c(.95,.05))
thetaHat - critVals*sqrt(m)/sqrt(n)

# plot errors for word "terrible"
which(eSubs@Dimnames[[1]]=="terrible")
hist(eSubs[4156,],col="blue",xlab="errors for terrible")

