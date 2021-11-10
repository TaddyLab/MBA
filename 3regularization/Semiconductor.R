###### REGULARIZATION #######
###### Semiconductor example #######

SC <- read.csv("semiconductor.csv")
full <- glm(fail ~ ., data=SC, family="binomial")

full$deviance
full$null.deviance
1 - full$deviance/full$null.deviance

pvals <- summary(full)$coef[-1,4] # -1 drops intercept
pvals <- sort(pvals[!is.na(pvals)])
J <- length(pvals)
k <- rank(pvals, ties.method="min")
q=0.1
( alpha <- max(pvals[ pvals<= (q*k/(J+1)) ]) )
sum(pvals<=alpha)

signif <- which(pvals < 0.0122)
cut <- glm(fail ~ ., data=SC[,c("fail", names(signif))],
    family="binomial")
1 - cut$deviance/cut$null.deviance # new in-sample R2

# histogram of p-values and BH algorithm figure
par(mfrow=c(1,2))
hist(pvals, xlab="P-value", main="", col="blue")
fdr_cut <- function(pvals, q=0.1){
  pvals <- sort(pvals[!is.na(pvals)])
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
  
  plot(pvals, log="xy", xlab="order", 
       main=sprintf("FDR of %g",q),
       ylab="p-value", bty="n", 
       col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha)
}

fdr_cut(pVals)

## OOS experiment for full and cut models
# create folds
n <- nrow(SC) # the number of observations
K <- 10 # the number of 'folds'
# create a vector of fold memberships (random order)
set.seed(1) # set seed to get same "random" results
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
foldid[1:20]

## run the OOS experiment
fulldev <- cutdev <- nulldev <- rep(NA,K)
for(k in 1:K){
    train <- which(foldid!=k) # train on all but fold 'k'
    # fit the two regressions
    cuts <- c("fail",names(signif))
    rfull <- glm(fail~., data=SC, subset=train, family="binomial")
    rcut <- glm(fail~., data=SC[,cuts], subset=train, family="binomial")
    # predict (type=response for probabilities)
    pfull <- predict(rfull, newdata=SC[-train,], type="response")
    pcut <- predict(rcut, newdata=SC[-train,], type="response")
    # calculate OOS deviances
    y <- SC$fail[-train]
    ybar <- mean(y)
    fulldev[k] <- -2*sum(y*log(pfull)+(1-y)*log(1-pfull))
    cutdev[k] <- -2*sum(y*log(pcut)+(1-y)*log(1-pcut))
    nulldev[k] <- -2*sum(y*log(ybar)+(1-y)*log(1-ybar))
    # print progress
    cat(k, "")
  }      

round(fulldev)
round(cutdev)
R2 <- data.frame(full = 1 - fulldev/nulldev, cut = 1 - cutdev/nulldev )
colMeans(R2)   

## Forward stepwise regression
null <- glm(fail~1, data=SC)
system.time(fwd <- step(null, scope=formula(full), dir="forward") )
length(coef(fwd)) # chooses around 70 coef

## Path estimation in gamlr
# Semiconductor Lasso
library(gamlr)
# create the numeric design matrix.  
scX <- sparse.model.matrix(FAIL ~ ., data=SC)[,-1] # do -1 to drop intercept!
# we included the y (FAIL) so that it woldn't include it with the remaining columns as x's
# here, we could have also just done x <- as.matrix(SC[,-1]).
# but sparse.model.matrix is a good way of doing things if you have factors.
scY <- SC$FAIL # pull out `y' too just for convenience
scLasso <- gamlr(scX, scY, family="binomial")
plot(scLasso) 
scLasso

dim(scLasso$beta) 
sum(scLasso$beta[,1]!=0)
sum(scLasso$beta[,100]!=0)

cvScLasso<-cv.gamlr(scX,scY,family="binomial")
plot(cvScLasso)

cvScLasso$cvm[100] #mean at 100th lambda 
cvScLasso$cvs[100] #SE at 100th lambda
cvScLasso$seg.1se #which lambda for 1se rule
cvScLasso$seg.min #which lambda for cv-min rule
cvScLasso$lambda.1se #lambda # lambda value for 1se rule
log(cvScLasso$lambda.1se)
cvScLasso$lambda.min #lambda # lambda value for CV-min rule
log(cvScLasso$lambda.min)

beta1SE<-coef(cvScLasso) #coefficients using 1se rule
cbind(beta1SE@Dimnames[[1]][which(beta1SE!=0)],beta1SE[which(beta1SE!=0)])

betaMin<-coef(cvScLasso, select="min") # coefficients for CV-min rule
cbind(betaMin@Dimnames[[1]][which(betaMin!=0)],betaMin[which(betaMin!=0)])

1-cvScLasso$cvm[cvScLasso$seg.1se]/cvScLasso$cvm[1] #OOS R2 for 1se
1-cvScLasso$cvm[cvScLasso$seg.min]/cvScLasso$cvm[1] #OOS R2 for CV-min rule

plot(cvScLasso, bty="n")
lines(log(scLasso$lambda),AICc(scLasso)/n, col="green", lwd=2)
lines(log(scLasso$lambda),BIC(scLasso)/n, col="maroon", lwd=2)
legend("top", fill=c("blue","green","maroon"),
	legend=c("CV","AICc","BIC"), bty="n")


## Lasso path estimation
library(gamlr)
fitSC <- gamlr(x=SC[,-1], y=SC[,1], family="binomial")
plot(fitSC)


