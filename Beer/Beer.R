library(gamlr)
load("Beer.rda")
ls()

# data
str(sales)
head(upc)

# how many upcs?
length( upctab <- table(sales$upc) )
dim(upc)

# check data types
str(sales)

# create priceper12oz
sales$lpoz <- log(sales$price/upc[as.character(sales$upc),"oz"])

# calculate lags
# not necesary, but always good practice
sales <- sales[order(sales$store,sales$upc,sales$week),] 

sales$lag <- unlist(tapply(sales$units, list(sales$upc,sales$store), 
					function(x) c(NA,x[-length(x)])))
head(sales)
tail(sales)
sales <- sales[!is.na(sales$lag),]

# all together (results are garbage)
coef( margfit <- glm(log(units) ~ lpoz, data=sales) )

# panel regression
library(gamlr)
xfe <- sparse.model.matrix( ~ store + week + upc, data=sales)[,-1]

# if you try to fit with OLS its too big in dense format
# > olsfit <- glm( log(units) ~ lpoz + as.matrix(xfe), data=sales )
# Error: cannot allocate vector of size 7.8 Gb

fullfit <- gamlr(cbind(lpos=sales$lpoz,llag=log(sales$lag), xfe), 
			     log(sales$units), lambda.start=0)
coef(fullfit)[2:3,]

# grab a small subsample
# naive lasso
set.seed(888)
ss <- sample.int(nrow(sales),500)
naivefit <- gamlr(cbind(lpos=sales$lpoz,llag=log(sales$lag), xfe)[ss,], 
			      log(sales$units)[ss], free=1:2, standardize=FALSE, lmr=1e-5)
print( coef(naivefit)[2:3,] )

# ols fit
olsfit <- gamlr(cbind(lpos=sales$lpoz,llag=log(sales$lag), xfe)[ss,], 
			      log(sales$units)[ss], lambda.start=0)
print( coef(olsfit)[2:3,] )

# orthogonal ML
omlfit <- orthoML(x=xfe[ss,], 
				d=cbind(lpos=sales$lpoz,llag=log(sales$lag))[ss,], 
				y=log(sales$units)[ss],
				standardize=TRUE, nfold=5, lmr=1e-5)
summary(omlfit)

##############  heterogeneity

# parse the item description text as a bag o' words
library(tm)
title <- Corpus(VectorSource(as.character(upc$title)))
title <- DocumentTermMatrix(title)
title <- sparseMatrix(i=title$i,j=title$j,x=as.numeric(title$v>0), # convert from stm to Matrix format
              dims=dim(title),dimnames=list(rownames(upc),colnames(title)))

title[1:5,1:6]
title[287,title[287,]!=0]

# interact items and text with price
lpxu <- xu*wber$lp
colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
# create our interaction matrix
xhte <- cBind(BASELINE=1,descr[wber$UPC,])
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")

eachbeer <- xhte[match(rownames(upc),wber$UPC),]
rownames(eachbeer) <- rownames(upc)

# fullhte 
fullhte <- gamlr(x=cBind(d,controls), y=log(wber$MOVE), lambda.start=0)
#gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])
hist(gamfull, main="", xlab="elasticity", ,
			 col="darkgrey", freq=FALSE)

# mle with all upcs
mlehte <- gamlr(x=cBind(d,controls)[ss,], 
	y=log(wber$MOVE)[ss], lambda.start=0)
gammle <- drop(eachbeer%*%coef(mlehte)[2:(ncol(d)+1),])
hist(gammle, main="", xlab="elasticity", breaks=200, col="pink", xlim=c(-60,25), freq=FALSE)
sort(gammle)[1:4]

# naive fit
naivehte <- gamlr(x=cBind(d,controls)[ss,], 
				  y=log(wber$MOVE)[ss], 
				  free=1, standardize=FALSE)
gamnaive <- drop(eachbeer%*%coef(naivehte)[2:(ncol(d)+1),])
hist(gamnaive, main="", xlab="elasticity", col="lightyellow", freq=FALSE)

# double ML
dmlhte <- gamlr(x=xhte[ss,]*resids$dtil, y=resids$ytil, free=1, standardize=FALSE)
coef(dmlhte)[1:2]
range( gamdml <- drop(eachbeer%*%coef(dmlhte)[-1,]) )
hist(gamdml, main="", xlab="elasticity", col="lightblue", freq=FALSE)

ylim <- c(-8,1)
par(mai=c(.7,.7,.1,.1), mfrow=c(1,3))
plot(gamfull, gammle, pch=21, bg="pink", xlab="fulldata MLE", ylab="subsample MLE", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gammle))$r.squared))
plot(gamfull, gamnaive, pch=21, bg="lightyellow", xlab="fulldata MLE", ylab="subsample Naive ML", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gamnaive))$r.squared))
plot(gamfull, gamdml, pch=21, bg="lightblue", xlab="fulldata MLE", ylab="subsample Orthogonal ML", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gamdml))$r.squared))

B <- coef(dmlhte)[-(1:2),]
B <- B[B!=0]
head(sort(round(B,2)))
head(sort(round(B,2), decreasing=TRUE))

upc[which(gamfull>0),]