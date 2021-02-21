
SC <- read.csv("semiconductor.csv")

## full model
full <- glm(fail ~ ., data=SC, family=binomial)

## deviance and R2
full$deviance
full$null.deviance
1 - full$deviance/full$null.deviance

## grab p-values
pvals <- summary(full)$coef[-1,4] #-1 to drop the intercept
## plot them: it looks like we have some signal here
png('scPval.png', width=5, height=5, units="in", res=720)
hist(pvals, xlab="p-value", main="", col="lightblue")
dev.off()

## running the BH FDR procedure
## At 10% FDR, we get 25 `signif'
pvals <- sort(pvals[!is.na(pvals)])
J <- length(pvals)
k <- rank(pvals, ties.method="min")
q=0.1
( alpha <- max(pvals[ pvals<= (q*k/(J+1)) ]) )

png('scFDR.png', width=5, height=5, units="in", res=720)
plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
   ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
lines(1:J, q*(1:J)/(J+1))
dev.off()


## Re-run a cut regression using only these 25
signif <- which(pvals < 0.0122)
cut <- glm(fail ~ ., data=SC[,c("fail", names(signif))], family="binomial")
1 - cut$deviance/cut$null.deviance # new in-sample R2

## Out of sample prediction experiment
# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
set.seed(1)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
foldid[1:20]
# use a for loop to run the experiment
fulldev <- cutdev <- nulldev <- rep(NA,K) 
for(k in 1:K){ 
	train <- which(foldid!=k) # train on all but fold `k'
		
	## fit the two regressions
	cuts <- c("fail",names(signif))
	rfull <- glm(fail~., data=SC, subset=train, family=binomial)
	rcut <- glm(fail~., data=SC[,cuts], subset=train, family=binomial)

	## get predictions: type=response so we have probabilities
	pfull <- predict(rfull, newdata=SC[-train,], type="response")
	pcut <- predict(rcut, newdata=SC[-train,], type="response")

	## calculate OOS deviances
	y <- SC$fail[-train]
	ybar <- mean(y)
	fulldev[k] <- -2*sum( y*log(pfull) + (1-y)*log(1-pfull) ) 
	cutdev[k] <- -2*sum( y*log(pcut) + (1-y)*log(1-pcut) ) 
	nulldev[k] <- -2*sum( y*log(ybar) + (1-y)*log(1-ybar) ) 

	## print progress
	cat(k, " ")
}

## resulting deviance
round(fulldev)
round(cutdev)
R2 <- data.frame(
	full = 1 - fulldev/nulldev,
	cut = 1 - cutdev/nulldev )
colMeans(R2) # WOW!  Full model really sucks.

## plot it in plum
png('scOOS.png', width=5, height=5, units="in", res=720)
boxplot(R2, col="plum", ylab="R2", xlab="model", bty="n")
dev.off()


## A forward stepwise procedure
# null model
null <- glm(fail~1, data=SC)
# forward stepwise: it takes a long time!
system.time(fwd <- step(null, scope=formula(full), dir="forward"))
length(coef(fwd)) # chooses around 70 coef

#### lasso 
library(gamlr) 
fitSC <- gamlr(x=SC[,-1], y=SC[,1], family="binomial")
#png('scLasso.png', width=4.5, height=4.5, units="in", res=720)
plot(fitSC)
#dev.off()