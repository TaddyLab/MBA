# Ames Housing Hedonic Price Regression
ames <- read.csv("AmesHousing.csv", strings=T)
dim(ames)
ames[1:3,c(1:5,79)]

png('amesPriceHist.png', width=4.5, height=4.5, units="in", res=720)
hist(ames$SalePrice, xlab="home sale price", freq=FALSE, col="khaki", main="")
dev.off()

png('amesScatter.png', width=4.5, height=4.5, units="in", res=720)
plot(ames$Lot.Area, ames$SalePrice, 
	xlab="Lot Area (SF)", log=("xy"), bty="n",
	ylab="Home Sale Price", col=rgb(1,.2,.2,.2), pch=20)
dev.off()

## convert year to factor
ames$Yr.Sold <- factor(ames$Yr.Sold)
ames$Mo.Sold <- factor(ames$Mo.Sold)

## convert to log lot area 
ames$Lot.Area <- log(ames$Lot.Area)

## the gamlr library: we will be heavy users
## syntax is the same as glmnet 
## it includes a number of useful functions for this book

####### Missing data stuff
sum(is.na(ames))
summary(ames$Pool.QC)
ames$Lot.Frontage[11:15]

## call naref to deal with missing data
library(gamlr)
amesImputed <- naref(ames, impute=TRUE) 
sum(is.na(amesImputed))
summary(amesImputed$Pool.QC)
amesImputed$Lot.Frontage.x[11:15]
amesImputed$Lot.Frontage.miss[11:15]

# example of zero imputation
mean(ames$Bsmt.Full.Bath==0, na.rm=TRUE)
ames$Bsmt.Full.Bath[1341:1344]
amesImputed$Bsmt.Full.Bath.x[1341:1344]
amesImputed$Bsmt.Full.Bath.miss[1341:1344]

## log sale price response 
yAmes <- log(ames$SalePrice)

## create the sparse model matrix

## fit the lasso pathycol <- which(names(amesImputed)=="SalePrice")
ycol <- which(names(amesImputed)=="SalePrice")
xAmes <- sparse.model.matrix( ~ ., data=amesImputed[,-ycol])[,-1]
dim(xAmes)

fitAmes <- gamlr(xAmes, yAmes, lmr=1e-4)

png('amesPath.png', width=4.5, height=4.5, units="in", res=720)
plot(fitAmes)
dev.off()

png('amesPathLMR.png', width=4.5, height=4.5, units="in", res=720)
plot(gamlr(xAmes, yAmes))
dev.off()

names(fitAmes)
length(fitAmes$lambda)
fitAmes$lambda[100]/fitAmes$lambda[1]
dim(fitAmes$beta)
fitAmes$beta[
	c("Overall.Qual","Lot.Area","Lot.Frontage.x","Lot.Frontage.miss"),
	c(1:2,99:100)]


# IC model selection
bAmes <- coef(fitAmes) ## the coefficients selected under AICc
head(bAmes)
## a few examples
bAmes <- bAmes[-1,] # drop intercept and remove sparse matrix formatting
sum(bAmes!=0)
head(sort(bAmes),4) ## big decreaser
tail(sort(bAmes),4) ## big increaser

## lot size, frontage, and above ground living area
bAmes[
 c("Overall.Qual","Lot.Area","Lot.Frontage.x","Lot.Frontage.miss")]

which.min(AICc(fitAmes))
fitAmes$lambda[62]

# other IC selections
(bicsel <- which.min(BIC(fitAmes)))
bAmesBIC <- coef(fitAmes, select=bicsel)[-1,] ## and BIC 
sum(bAmesBIC!=0)

(aicsel <- which.min(AIC(fitAmes)))
bAmesAIC <- coef(fitAmes, select=aicsel)[-1,] ## and AIC 
sum(bAmesAIC!=0)

# prediction
( yhat <- predict(fitAmes, xAmes[c(1,11),]) )
drop(yhat)
exp(drop(yhat))

### cross validation
set.seed(0)
cvfitAmes <- cv.gamlr(xAmes, yAmes, verb=TRUE, lmr=1e-4)


cvfitAmes$seg.min
cvfitAmes$lambda.min
log(cvfitAmes$lambda.min)

cvfitAmes$seg.1se
cvfitAmes$lambda.1se
log(cvfitAmes$lambda.1se)

## OOS R2 at lambda100
1 - cvfitAmes$cvm[100]/cvfitAmes$cvm[1]

## extract some coefficients
bAmesCV1se <- coef(cvfitAmes)[-1,] ## 1se rule; see ?cv.gamlr
bAmesCVmin <- coef(cvfitAmes, select="min")[-1,] ## min cv selection
sum(bAmesCV1se!=0)
sum(bAmesCVmin!=0)
cbind(bAmesCV1se,bAmesCVmin)[c("Lot.Area","Lot.Frontage.x"),]

## plot it
png('amesCV.png', width=4.5, height=4.5, units="in", res=720)
plot(cvfitAmes)
dev.off()

## log lambdas selected under various criteria
log(fitAmes$lambda[which.min(AICc(fitAmes))])
log(fitAmes$lambda[which.min(AIC(fitAmes))])
log(fitAmes$lambda[which.min(BIC(fitAmes))])
log(cvfitAmes$lambda.min)
log(cvfitAmes$lambda.1se)

## plot CV results and the various IC
ll <- log(fitAmes$lambda) ## the sequence of lambdas
n <- nrow(xAmes)

png('amesSelection.png', width=4.5, height=4.5, units="in", res=720)
plot(ll, AIC(fitAmes)/n, bty="n",
	xlab="log lambda", ylab="IC/n", type="l", lwd=2, col="orange")
abline(v=ll[which.min(BIC(fitAmes))], col="green", lty=2, lwd=2)
abline(v=ll[which.min(AICc(fitAmes))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fitAmes))], col="orange", lty=3, lwd=2)
lines(ll, BIC(fitAmes)/n, lwd=2, col="green")
lines(ll, AICc(fitAmes)/n, lwd=2, col="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
dev.off()

## all metrics, together in a path plot.
png('amesICCV.png', width=4.5, height=4.5, units="in", res=720)
plot(fitAmes, col="grey", select=FALSE)
abline(v=ll[which.min(AICc(fitAmes))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fitAmes))], col="orange", lty=3, lwd=2)
abline(v=ll[which.min(BIC(fitAmes))], col="green", lty=2, lwd=2)
abline(v=log(cvfitAmes$lambda.min), col="blue", lty=2, lwd=2)
abline(v=log(cvfitAmes$lambda.1se), col="red", lty=2, lwd=2)
legend("bottomright", bty="n", lwd=2, 
	col=c("black","orange","blue","green","red"),
	legend=c("AICc","AIC","CV.min","BIC","CV.1se"))
dev.off()

### uncertainty quantification
## grab a variable

xnew <- xAmes[c(1,11),]
yhat0 <- drop( predict(cvfitAmes, xnew, select="min") )
exp(yhat0)

B <- 100
yhatB <- matrix(nrow=2, ncol=B)

library(parallel)
cl <- makeCluster(detectCores())

for(b in 1:100){
	wb <- rexp(nrow(xAmes))
	fitb <- cv.gamlr(xAmes, yAmes, 
						 obsweight=wb, lmr=1e-4, cl=cl)
	yhatB[,b] <- drop(predict(fitb, xnew, select="min"))
	cat(b, " ")
}

## basic CI
apply(exp(yhatB),1,quantile,probs=c(.025,.975))
## bias corrected CI
apply(2*exp(yhat0)-exp(yhatB),1,quantile,probs=c(.025,.975))
