## bank telemarketing
tele <- read.csv("telemarketing.csv", strings=T)
dim(tele)
tele[1,]

## log sale price response 
y <- tele$subscribe

## the gamlr library: we will be heavy users
## syntax is the same as glmnet 
## it includes a number of useful functions for this book
library(gamlr)
## set NA as reference
teledf <- naref(tele[,-15])
levels(teledf$job)

## sparse model matrix
x <- sparse.model.matrix(~.^2 + I(durmin^2), data=teledf)
dim(x)

## fit the lasso path
fit <- gamlr(x, y, family="binomial")

B <- coef(fit) ## the coefficients selected under AICc
## a few examples
B <- B[-1,] # drop intercept and remove sparse matrix formatting
sum(B!=0)
head(sort(B),4) ## big decreasers
tail(sort(B),4) ## big increasers

## nonzero coefficients on durmin
Bnz <- B[B!=0]
Bnz[grep("durmin",names(Bnz))]

grid <- seq(0,max(tele$durmin),length=200)
png('teleDurMin.png', width=5, height=5, units="in", res=720)
plot(grid, exp(B["durmin"]*grid+B["I(durmin^2)"]*grid^2),
	type="l", bty="n", xlab="call duration in minutes", ylab="multiplier on odds of success")
dev.off()

png('teleFitted.png', width=5, height=5, units="in", res=720)
boxplot(predict(fit,x,type="response") ~ y, ylab="y.hat", col="khaki")
dev.off()

# other IC selections
Bbic <- coef(fit, select=which.min(BIC(fit)))[-1,] ## and BIC instead
Baic <- coef(fit, select=which.min(AIC(fit)))[-1,] ## AIC instead
sum(B!=0)
sum(Bbic!=0)
sum(Baic!=0)

set.seed(0)
cvfit <- cv.gamlr(x, y, verb=TRUE)
beta1se <- coef(cvfit)[-1,] ## 1se rule; see ?cv.gamlr
betamin <- coef(cvfit, select="min")[-1,] ## min cv selection
cbind(beta1se,betamin)[c("durmin","I(durmin^2)"),]
sum(beta1se!=0)
sum(betamin!=0)

## plot them together  (note cvfit$gamlr and fit$)
png('telePath.png', width=5, height=5, units="in", res=720)
plot(fit)
dev.off()
png('teleCV.png', width=5, height=5, units="in", res=720)
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

png('teleSelection.png', width=5, height=5, units="in", res=720)
plot(ll, AIC(fit)/n, bty="n",
	xlab="log lambda", ylab="IC/n", type="l", lwd=2, col="orange")
abline(v=ll[which.min(BIC(fit))], col="green", lty=2, lwd=2)
abline(v=ll[which.min(AICc(fit))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fit))], col="orange", lty=2, lwd=2)
lines(ll, BIC(fit)/n, lwd=2, col="green")
lines(ll, AICc(fit)/n, lwd=2, col="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
dev.off()

## all metrics, together in a path plot.
png('teleICCV.png', width=5, height=5, units="in", res=720)
plot(fit, col="grey", select=FALSE)
abline(v=ll[which.min(AICc(fit))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fit))], col="orange", lty=2, lwd=2)
abline(v=ll[which.min(BIC(fit))], col="green", lty=2, lwd=2)
abline(v=log(cvfit$lambda.min), col="blue", lty=2, lwd=2)
abline(v=log(cvfit$lambda.1se), col="red", lty=2, lwd=2)
legend("bottomright", bty="n", lwd=2, 
	col=c("black","orange","blue","green","red"),
	legend=c("AICc","AIC","CV.min","BIC","CV.1se"))
dev.off()

### uncertainty quantification
## simulation function
p0 <- drop( predict(fit,x, type="response") )
getBoot <- function(b){
	yb <- rbinom(nrow(x),size=1,prob=p0)
	fitb <- gamlr(x, yb, family="binomial")
	beta <- coef(fitb)[-1,]
	list(beta=beta[c("durmin","I(durmin^2)")], nz=sum(beta!=0))
}
getBoot(b)
( t0 <- coef(fit)[c("durmin","I(durmin^2)"),] )
( nz0 <- sum(B!=0) )

## run the bootstrap
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("gamlr","cv.gamlr","x", "p0"))
t <- parLapply(cl, 1:100, getBoot)
t[[1]]

dmy <- sapply(t, function(tb){ 
		tb$beta["durmin"]*grid+tb$beta["I(durmin^2)"]*grid^2})
png('teleBboot.png', width=5, height=5, units="in", res=720)
matplot(grid, dmy, col=8, type="l", bty="n", 
 xlab="call duration in minutes", ylab="log multiplier on odds of success")
lines(grid, B["durmin"]*grid+B["I(durmin^2)"]*grid^2, col="navy")
dev.off()

tnz <- sapply(t, function(tb){tb$nz})
png('teleNZboot.png', width=5, height=5, units="in", res=720)
hist(tnz, xlab="AICc selected nonzero coefficients", 
	freq=FALSE, col="grey80", main="")
abline(v=nz0, lwd=2, col=2)
dev.off()
