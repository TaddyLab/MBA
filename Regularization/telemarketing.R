## bank tlmrkmarketing
tlmrk <- read.csv("telemarketing.csv", strings=T)
dim(tlmrk)
tlmrk[1,]

## log sale price response 
yTD <- tlmrk$subscribe
mean(yTD)

## the gamlr library: we will be heavy users
## syntax is the same as glmnet 
## it includes a number of useful functions for this book
sum(is.na(tlmrk))
library(gamlr)
tlmrkX <- naref(tlmrk[,-15])
levels(tlmrkX$job)

## sparse model matrix
xTD <- sparse.model.matrix(~.^2 + I(durmin^2), data=tlmrkX)
dim(xTD)

## fit the lasso path
fitTD <- gamlr(xTD, yTD, family="binomial")
png('tlmrkPath.png', width=4.5, height=4.5, units="in", res=720)
plot(fitTD)
dev.off()

## coefficients
fitTD$beta[c("durmin","I(durmin^2)"),c(1:2,99:100)]


B <- coef(fitTD) ## the coefficients selected under AICc
## a few examples
B <- B[-1,] # drop intercept and remove sparse matrix formatting
sum(B!=0)
head(sort(B),4) ## big decreasers
tail(sort(B),4) ## big increasers

## nonzero coefficients on durmin
Bnz <- B[B!=0]
Bnz[grep("durmin",names(Bnz))]

grid <- seq(0,max(tlmrk$durmin),length=200)
png('tlmrkDurMin.png', width=5, height=5, units="in", res=720)
plot(grid, exp(B["durmin"]*grid+B["I(durmin^2)"]*grid^2),
	type="l", bty="n", xlab="call duration in minutes", ylab="multiplier on odds of success")
dev.off()

png('tlmrkFitted.png', width=5, height=5, units="in", res=720)
boxplot(predict(fitTD,xTD,type="response") ~ yTD, ylab="y.hat", col="khaki")
dev.off()

# other IC selections
Bbic <- coef(fitTD, select=which.min(BIC(fitTD)))[-1,] ## and BIC instead
Baic <- coef(fitTD, select=which.min(AIC(fitTD)))[-1,] ## AIC instead
sum(B!=0)
sum(Bbic!=0)
sum(Baic!=0)

set.seed(0)
cvfitTD <- cv.gamlr(xTD, yTD, verb=TRUE)
beta1se <- coef(cvfitTD)[-1,] ## 1se rule; see ?cv.gamlr
betamin <- coef(cvfitTD, select="min")[-1,] ## min cv selection
cbind(beta1se,betamin)[c("durmin","I(durmin^2)"),]
sum(beta1se!=0)
sum(betamin!=0)

## plot the CV path
png('tlmrkCV.png', width=5, height=5, units="in", res=720)
plot(cvfitTD)
dev.off()

## log lambdas selected under various criteria
log(fitTD$lambda[which.min(AICc(fitTD))])
log(fitTD$lambda[which.min(AIC(fitTD))])
log(fitTD$lambda[which.min(BIC(fitTD))])
log(cvfitTD$lambda.min)
log(cvfitTD$lambda.1se)

## plot CV results and the various IC
ll <- log(fitTD$lambda) ## the sequence of lambdas
n <- nrow(xTD)

png('tlmrkSelection.png', width=5, height=5, units="in", res=720)
plot(ll, AIC(fitTD)/n, bty="n",
	xlab="log lambda", ylab="IC/n", type="l", lwd=2, col="orange")
abline(v=ll[which.min(BIC(fitTD))], col="green", lty=2, lwd=2)
abline(v=ll[which.min(AICc(fitTD))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fitTD))], col="orange", lty=2, lwd=2)
lines(ll, BIC(fitTD)/n, lwd=2, col="green")
lines(ll, AICc(fitTD)/n, lwd=2, col="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
dev.off()

## all metrics, together in a path plot.
png('tlmrkICCV.png', width=5, height=5, units="in", res=720)
plot(fitTD, col="grey", select=FALSE)
abline(v=ll[which.min(AICc(fitTD))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fitTD))], col="orange", lty=2, lwd=2)
abline(v=ll[which.min(BIC(fitTD))], col="green", lty=2, lwd=2)
abline(v=log(cvfitTD$lambda.min), col="blue", lty=2, lwd=2)
abline(v=log(cvfitTD$lambda.1se), col="red", lty=2, lwd=2)
legend("bottomright", bty="n", lwd=2, 
	col=c("black","orange","blue","green","red"),
	legend=c("AICc","AIC","CV.min","BIC","CV.1se"))
dev.off()

### uncertainty quantification
## simulation function
p0 <- drop( predict(fitTD,xTD, type="response", select=100) )
getBoot <- function(b){
	yb <- rbinom(nrow(xTD),size=1,prob=p0)
	fitTDb <- gamlr(xTD, yb, family="binomial")
	beta <- coef(fitTDb)[-1,]
	list(beta=beta[c("durmin","I(durmin^2)")], nz=sum(beta!=0))
}
getBoot(b)
( t0 <- coef(fitTD)[c("durmin","I(durmin^2)"),] )
( nz0 <- sum(B!=0) )

## run the bootstrap
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("gamlr","cv.gamlr","xTD", "p0"))
t <- parLapply(cl, 1:100, getBoot)
t[[1]]

dmy <- sapply(t, function(tb){ 
		tb$beta["durmin"]*grid+tb$beta["I(durmin^2)"]*grid^2})
png('tlmrkBboot.png', width=5, height=5, units="in", res=720)
matplot(grid, dmy, col=8, type="l", bty="n", 
 xlab="call duration in minutes", ylab="log multiplier on odds of success")
lines(grid, B["durmin"]*grid+B["I(durmin^2)"]*grid^2, col="navy")
dev.off()

tnz <- sapply(t, function(tb){tb$nz})
png('tlmrkNZboot.png', width=5, height=5, units="in", res=720)
hist(tnz, xlab="AICc selected nonzero coefficients", 
	freq=FALSE, col="grey80", main="")
abline(v=nz0, lwd=2, col=2)
dev.off()
