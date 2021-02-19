# Ames Housing Hedonic Price Regression
ames <- read.csv("AmesHousing.csv", strings=T, row.names=1)
dim(ames)
ames[1:3,1:5]

png('amesPriceHist.png', width=4.5, height=4.5, units="in", res=720)
hist(ames$SalePrice, xlab="home sale price", freq=FALSE, col="khaki", main="")
dev.off()

png('amesScatter.png', width=4.5, height=4.5, units="in", res=720)
plot(ames$Lot.Area, ames$SalePrice, 
	xlab="Lot Area (SF)", 
	ylab="Home Sale Price", col=rgb(1,.2,.2,.2), pch=20)
dev.off()

## log sale price response 
y <- log(ames$SalePrice)

## convert year to factor
ames$Yr.Sold <- factor(ames$Yr.Sold)
ames$Mo.Sold <- factor(ames$Mo.Sold)

## create log lot area and frontage 
ames$Lot.Frontage <- log(ames$Lot.Frontage)
ames$Lot.Area <- log(ames$Lot.Area)

## the gamlr library: we will be heavy users
## syntax is the same as glmnet 
## it includes a number of useful functions for this book
library(gamlr)

## replace missing values and set NA as reference for factors
sum(is.na(ames))
# drop the unlogged lot area and frontage, Sale Price, and ids
amesDF <- ames[,-c(1:2,81)]
amesDF <- naref(amesDF, impute=TRUE)
sum(is.na(amesDF))

## create the sparse model matrix
x <- sparse.model.matrix( ~ ., data=amesDF)
dim(x)

## fit the lasso path
fit <- gamlr(x, y, lmr=1e-4)

B <- coef(fit) ## the coefficients selected under AICc
## a few examples
B <- B[-1,] # drop intercept and remove sparse matrix formatting
sum(B!=0)
head(sort(B),4) ## big decreaser
tail(sort(B),4) ## big increaser

## lot size, frontage, and above ground living area
B[c("Lot.Area", "Lot.Frontage.x", "Lot.Frontage.miss")]

# other IC selections
Bbic <- coef(fit, select=which.min(BIC(fit)))[-1,] ## and BIC instead
Baic <- coef(fit, select=which.min(AIC(fit)))[-1,] ## AIC instead
sum(B!=0)
sum(Bbic!=0)
sum(Baic!=0)

set.seed(0)
cvfit <- cv.gamlr(x, y, verb=TRUE, lmr=1e-4)
beta1se <- coef(cvfit)[-1,] ## 1se rule; see ?cv.gamlr
betamin <- coef(cvfit, select="min")[-1,] ## min cv selection
cbind(beta1se,betamin)[c("Lot.Area","Lot.Frontage.x"),]
sum(beta1se!=0)
sum(betamin!=0)

## plot them together  (note cvfit$gamlr and fit$)
png('amesPath.png', width=5, height=5, units="in", res=720)
plot(fit)
dev.off()
png('amesCV.png', width=5, height=5, units="in", res=720)
plot(cvfit)
dev.off()

## log lambdas selected under various criteria
log(fit$lambda[which.min(AICc(fit))])
log(fit$lambda[which.min(AIC(fit))])
log(fit$lambda[which.min(BIC(fit))])
log(cvfit$lambda.min)
log(cvfit$lambda.1se)

## plot CV results and the various IC
ll <- log(fit$lambda) ## the sequence of lambdas
n <- nrow(x)

png('amesSelection.png', width=5, height=5, units="in", res=720)
plot(ll, AIC(fit)/n, bty="n",
	xlab="log lambda", ylab="IC/n", type="l", lwd=2, col="orange")
abline(v=ll[which.min(BIC(fit))], col="green", lty=2, lwd=2)
abline(v=ll[which.min(AICc(fit))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fit))], col="orange", lty=3, lwd=2)
lines(ll, BIC(fit)/n, lwd=2, col="green")
lines(ll, AICc(fit)/n, lwd=2, col="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
dev.off()

## all metrics, together in a path plot.
png('amesICCV.png', width=5, height=5, units="in", res=720)
plot(fit, col="grey", select=FALSE)
abline(v=ll[which.min(AICc(fit))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fit))], col="orange", lty=3, lwd=2)
abline(v=ll[which.min(BIC(fit))], col="green", lty=2, lwd=2)
abline(v=log(cvfit$lambda.min), col="blue", lty=2, lwd=2)
abline(v=log(cvfit$lambda.1se), col="red", lty=2, lwd=2)
legend("bottomright", bty="n", lwd=2, 
	col=c("black","orange","blue","green","red"),
	legend=c("AICc","AIC","CV.min","BIC","CV.1se"))
dev.off()

### uncertainty quantification
## grab a variable
cvgetB <- function(vname){
	fitb <- cv.gamlr(x, y, obsweight=rexp(nrow(x)), lmr=1e-4)
	coef(fitb, select="min")[vname,]
}
cvgetB("Lot.Area")
( cvt0 <- coef(cvfit, select="min")["Lot.Area",] )

## run the CV bootstrap
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("gamlr","cv.gamlr","x", "y"))

cvt <- parSapply(cl, rep("Lot.Area",100), cvgetB)
## basic CI
quantile(cvt, c(.025,.975))
## bias corrected CI
quantile(2*cvt0 - cvt, c(.025,.975))
