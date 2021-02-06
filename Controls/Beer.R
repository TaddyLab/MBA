library(gamlr)
load("Beer.rda")
ls()

# data
head(sales)
head(upc)

# how many upcs?
dim(upc)

# check data types
str(sales)

# calculate lags
sales <- sales[order(sales$upc,sales$week),] 
sales$lag <- unlist(tapply(sales$units, sales$upc, 
							function(x) c(NA,x[-length(x)])))
sales <- sales[!is.na(sales$lag),]
head(sales)
tail(sales)

# create price per oz
sales$lpoz <- log(sales$price/upc[as.character(sales$upc),"oz"])

## calculate fixed effects
library(gamlr)
x <- sparse.model.matrix( ~ week + upc, data=sales)[,-1]
dim(x)

# parse the item description text as a bag o' words
library(tm)
w <- Corpus(VectorSource(as.character(upc$title)))
w <- DocumentTermMatrix(w)
w <- sparseMatrix(i=w$i,j=w$j,x=as.numeric(w$v>0), # convert from stm to Matrix format
              dims=dim(w),dimnames=list(rownames(upc),colnames(w)))

w[1:5,1:6]
w[242,w[242,]!=0]
dim(w)
## match to the observations
wupc <- w
w <- w[as.character(sales$upc),]
dim(w)


# all together (results are garbage)
coef( margfit <- glm(log(units) ~ lpoz, data=sales) )

# naive regression
naivefit <- gamlr(cbind(lpoz=sales$lpoz,llag=log(sales$lag), x), 
			     log(sales$units), free=1:2,, standardize=FALSE, lmr=1e-4)
coef(naivefit)[2:3,]

## LTE lasso
dfit <- gamlr(x,sales$lpoz, standardize=FALSE, lmr=1e-5)
dhat <- drop(predict(dfit, x))
resid <- sales$lpoz - dhat


png('beerTreatLasso.png', width=4, height=5, units="in", res=720)
plot(dfit)
dev.off()
png('beerScatterplot.png', width=4, height=5, units="in", res=720)
plot(dhat[1:1e5] ~ sales$lpoz[1:1e5], 
	col="purple", cex=.5, pch=21, bty="n", xlab="d (lpoz)", ylab="dhat")
dev.off()


beerATE <- gamlr(cbind(lpoz=resid,llag=log(sales$lag), x), 
			     log(sales$units), free=1:2, standardize=FALSE, lmr=1e-4)
coef(beerATE)[2:3,]

# fit with OLS 
beerOLS <- glm( log(units) ~ lpoz + log(lag) + as.matrix(x), data=sales )
summary(beerOLS)

# do the HTE fit.

nuw <- cbind(lpoz=resid, resid*w)
beerHTE <- gamlr(cbind(nuw, llag=log(sales$lag), x), 
			 log(sales$units), free="llag", standardize=FALSE, lmr=1e-3)

gam <- coef(beerHTE)[c("lpoz",colnames(w)),]
head(gam)

elastics <- drop( cbind(1,wupc) %*% gam )			
upc$elastics <- elastics
head(upc,1)
tail(upc,1)

png('beerElastics.png', width=5, height=5, units="in", res=720)
hist(elastics, freq=FALSE, xlab="elastcity", main="UPC elasticities")
dev.off()

upc[which.min(elastics),]
upc[which.max(elastics),]

