library(gamlr)
load("Beer.rda")
ls()

# data
head(sales)
head(upc)

# how many upcs?
length( upctab <- table(sales$upc) )
dim(upc)

# check data types
str(sales)

# create priceper12oz
sales$lpoz <- log(sales$price/upc[as.character(sales$upc),"oz"])

# calculate lags
sales <- sales[order(sales$store,sales$upc,sales$week),] 
sales$lag <- unlist(tapply(sales$units, list(sales$upc,sales$store), 
					function(x) c(NA,x[-length(x)])))
sales <- sales[!is.na(sales$lag),]
head(sales)
tail(sales)

## calculate fixed effects
library(gamlr)
x <- sparse.model.matrix( ~ store + week + upc, data=sales)[,-1]

# parse the item description text as a bag o' words
library(tm)
w <- Corpus(VectorSource(as.character(upc$title)))
w <- DocumentTermMatrix(w)
w <- sparseMatrix(i=w$i,j=w$j,x=as.numeric(w$v>0), # convert from stm to Matrix format
              dims=dim(w),dimnames=list(rownames(upc),colnames(w)))

w[1:5,1:6]
w[287,w[287,]!=0]
dim(w)
## match to the observations
w <- w[as.character(sales$upc),]
dim(w)


# all together (results are garbage)
coef( margfit <- glm(log(units) ~ lpoz, data=sales) )

# naive regression
naivefit <- gamlr(cbind(lpoz=sales$lpoz,llag=log(sales$lag), x), 
			     log(sales$units), free=1:2,, standardize=FALSE, lmr=1e-4)
coef(naivefit)[2:3,]

## LTE lasso
dfit <- gamlr(x,sales$lpoz, standardize=FALSE, lmr=1e-6)
dhat <- drop(predict(dfit, x))

png('beerTreatLasso.png', width=4, height=5, units="in", res=720)
plot(dfit)
dev.off()
png('beerScatterplot.png', width=4, height=5, units="in", res=720)
plot(dhat[1:1e5] ~ sales$lpoz[1:1e5], 
	col="purple", cex=.5, pch=21, bty="n", xlab="d (lpoz)", ylab="dhat")
dev.off()

# if you try to fit with OLS its too big in dense format
# > olsfit <- glm( log(units) ~ lpoz + as.matrix(xfe), data=sales )
# Error: cannot allocate vector of size 7.8 Gb

fullfit <- gamlr(cbind(lpos=sales$lpoz,llag=log(sales$lag), xfe), 
			     log(sales$units), lambda.start=0)
coef(fullfit)[2:3,]

# grab a small subsample
# naive lasso
set.seed(888)
ss <- sample.int(nrow(sales),5000)
naivefit <- gamlr(cbind(llag=log(sales$lag), lpos=sales$lpoz, xfe)[ss,], 
			      log(sales$units)[ss], free=1:2, standardize=FALSE, lmr=1e-5)
print( coef(naivefit)[2:3,] )

# ols fit
olsfit <- gamlr(cbind(llag=log(sales$lag), lpos=sales$lpoz, xfe)[ss,], 
			      log(sales$units)[ss], lambda.start=0)
print( coef(olsfit)[2:3,] )

# orthogonal ML
omlfit <- orthoML(x=xfe[ss,], 
				d=cbind(llag=log(sales$lag), lpos=sales$lpoz)[ss,], 
				y=log(sales$units)[ss],
				standardize=TRUE, nfold=5, lmr=1e-5)
summary(omlfit)

# residual fit
dfit <- gamlr(xfe[ss,], sales$lpoz[ss], standardize=FALSE, lmr=1e-3)
dtil <- drop(sales$lpoz - predict(dfit, xfe))
resfit <- gamlr(cbind(llag=log(sales$lag), lpos=dtil, xfe)[ss,], 
			      log(sales$units)[ss], standardize=FALSE, lmr=1e-3, free=1)
print( coef(resfit)[2:3,] )

##############  heterogeneity
## OLS just labs
u <- sparse.model.matrix( ~ upc, data=sales)[,-1]
llag <- log(sales$lag)
htefe <- gamlr(cbind(llag=llag, lpos=dtil, u*dtil, xfe), 
			      log(sales$units), standardize=FALSE, free=1, lmr=1e-6)
coef(htefe)[1:4,]
gammafe <- coef(htefe)[3] + drop(u%*%coef(htefe)[4:(ncol(u)+3),])
hist(gammafe, freq=FALSE, xlab="elastcity", main="UPC Fixed Effects")


# double ML
hteres <- gamlr(cbind(llag=llag, lpos=dtil, w*dtil, xfe), 
			      log(sales$units), standardize=FALSE, free=1, lmr=1e-6)
coef(hteres)[1:4,]
gammares <- coef(hteres)[3] + drop(w%*%coef(hteres)[4:(ncol(w)+3),])
hist(gammares, freq=FALSE, xlab="elastcity", main="UPC Text Effects")
#plot(gammares,gammafe)

B <- coef(hteres)[4:(ncol(w)+3),]
B <- B[B!=0]
head(sort(round(B,2)))
head(sort(round(B,2), decreasing=TRUE))

upc[rownames(upc)%in% as.character(sales$upc[which(gammares>0)]),]