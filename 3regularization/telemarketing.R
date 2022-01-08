###### REGULARIZATION #######
###### bank telemarketing #######

tlmrk <- read.csv("telemarketing.csv", strings=T)
dim(tlmrk)
tlmrk[1,]

## pull out log sale price response 
yTD <- tlmrk$subscribe
mean(yTD)

## the gamlr library: we will be heavy users
## syntax is the same as glmnet 
## it includes a number of useful functions for this book
sum(is.na(tlmrk)) # no NAs 
library(gamlr)
# even though we don't have NAs, run naref to make NA ref level
tlmrkX <- naref(tlmrk[,-15]) # removed column 15 (the response)
levels(tlmrkX$job)

## sparse model matrix
xTD <- sparse.model.matrix(~.^2 + I(durmin^2), data=tlmrkX)
dim(xTD)

## fit the lasso path
fitTD <- gamlr(xTD, yTD, family="binomial")
plot(fitTD)
#png('tlmrkFitted.png', width=5, height=5, units="in", res=720)
boxplot(predict(fitTD,xTD,type="response") ~ yTD, ylab="y.hat", col="khaki")
#dev.off()



## coefficients
fitTD$beta[c("durmin","I(durmin^2)"),c(1:2,99:100)]

## IC selection
AIC(fitTD)
which.min(AIC(fitTD))
sum(fitTD$beta[,92]!=0)
fitTD$lambda[92]
fitTD$lambda[which.min(AIC(fitTD))] 

bTD <- coef(fitTD)[-1,] ## the coefficients selected under AICc
sum(bTD!=0)
bTD[c("durmin","I(durmin^2)")]
head(sort(bTD),2) ## big decreasers
tail(sort(bTD),2) ## big increasers

# other IC selections
bTDbic <- coef(fitTD, select=which.min(BIC(fitTD)))[-1,] ## BIC
sum(bTDbic!=0)
bTDaic <- coef(fitTD, select=which.min(AIC(fitTD)))[-1,] ## AIC
sum(bTDaic!=0)

# Cross-Validation
library(parallel)
cl <- makeCluster(detectCores())
set.seed(0)
cvfitTD <- cv.gamlr(xTD, yTD, family="binomial", cl=cl)


betamin <- coef(cvfitTD, select="min")[-1,] # cv min rule returns 88 coefficients
sum(betamin!=0)
beta1se <- coef(cvfitTD, select="1se")[-1,] # cv 1se rule returns 18 coefficients
sum(beta1se!=0)

# make predictions
yTD[c(1,100)]
drop(predict(cvfitTD, xTD[c(1,100),], select="min", type="response"))
drop(predict(cvfitTD, xTD[c(1,100),], select="1se", type="response"))

## plot the CV path
plot(cvfitTD)

## plot CV results and the various IC
ll <- log(fitTD$lambda) ## the sequence of lambdas
n <- nrow(xTD)

#png('tlmrkSelection.png', width=4.5, height=4.5, units="in", res=720)
plot(ll, AIC(fitTD)/n, bty="n",
	xlab="log lambda", ylab="IC/n", type="l", lwd=2, col="orange")
abline(v=ll[which.min(BIC(fitTD))], col="green", lty=2, lwd=2)
abline(v=ll[which.min(AICc(fitTD))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fitTD))], col="orange", lty=2, lwd=2)
lines(ll, BIC(fitTD)/n, lwd=2, col="green")
lines(ll, AICc(fitTD)/n, lwd=2, col="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))
#dev.off()

## all metrics, together in a path plot.
plot(fitTD, col="grey", select=FALSE)
abline(v=ll[which.min(AICc(fitTD))], col="black", lty=2, lwd=2)
abline(v=ll[which.min(AIC(fitTD))], col="orange", lty=2, lwd=2)
abline(v=ll[which.min(BIC(fitTD))], col="green", lty=2, lwd=2)
abline(v=log(cvfitTD$lambda.min), col="blue", lty=2, lwd=2)
abline(v=log(cvfitTD$lambda.1se), col="red", lty=2, lwd=2)
legend("bottomright", bty="n", lwd=2, 
	col=c("black","orange","blue","green","red"),
	legend=c("AICc","AIC","CV.min","BIC","CV.1se"))


### uncertainty quantification
## simulation function
p0 <- drop( predict(fitTD ,xTD, type="response", select=100) )
getBoot <- function(b){
	yb <- rbinom(nrow(xTD),size=1,prob=p0)
	fitTDb <- gamlr(xTD, yb, family="binomial")
	coef(fitTDb)[c("durmin","I(durmin^2)"),]
}

getBoot(1)
bTD[c("durmin","I(durmin^2)")]

## run the bootstrap
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("gamlr", "xTD", "p0"))
betaB <- parSapply(cl, 1:100, getBoot)

plot(t(betaB), pch=20, bty="n")


grid <- seq(0,max(tlmrk$durmin),length=200)
dmy <- apply(betaB, 2, function(b){ b[1]*grid+b[2]*grid^2 } )
#png('tlmrkBootCurve.png', width=4.5, height=4.5, units="in", res=720)
matplot(grid, dmy, col=8, type="l", bty="n", 
 xlab="call duration in minutes", ylab="log multiplier on odds of success")
lines(grid, bTD["durmin"]*grid+bTD["I(durmin^2)"]*grid^2, col="navy", lwd=1.5)
#dev.off()
