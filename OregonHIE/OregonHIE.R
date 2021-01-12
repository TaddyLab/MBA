######################################
#### Causal Inference - Experiments
######################################

################## OHIE
# Get the data from nber.org/oregon/4.data.html

# person_id  is key
# treatment is in Description file, and is random conditional on the numhh (number of names in lottery)
# in 2008 new spots opened for medicaid, which was previously closed to new enroll
# we are interested in health insurance effect on increased utilization (hence costs)
# (impact on health is a longer term outcome of interest)

ohie <- read.csv("OHIEresults.csv")

#### ATE - basic diffs in mean
table(ohie[,c("selected","numhh")])

getATE <- function(data, ind, v="doc_num"){
	ybar <- tapply(data[ind,v], data[ind,"selected"], mean)
	return( ybar['1'] - ybar['0'] )
}
getATE(ohie,1:nrow(ohie))

ybar <- tapply(ohie[,"doc_num"], ohie[,"selected"], mean)
( ATE = ybar['1'] - ybar['0'] )

library(parallel)
library(boot)
( bootATE <- boot(ohie, getATE, 1000, v="doc_num", parallel="snow", ncpus=detectCores() ) )


nSel <- table(ohie$selected)
yVar <- tapply(ohie$doc_num, ohie$selected, var)
seATE = sqrt(sum(yVar/nSel))
ATE + c(-2,2)*seATE

# weighting to adjust sample to represent population
nSelW <- tapply(ohie$weight, ohie$selected, sum)
yBarW <- tapply(ohie$weight*ohie$doc_num, ohie$selected, sum)/nSelW
(ATEweighted <-  yBarW['1'] - yBarW['0'])

# covariate imbalance - larger households higher chance to be selected
table(ohie[,c("selected","numhh")])

# linear fit to account for household size
fitLin <- glm(doc_any ~ selected + numhh, data=ohie)
summary(fitLin)

x <- scale(model.matrix( ~ numhh, data=ohie)[,-1], scale=FALSE)
colMeans(x)
fitLinAdj <- glm(doc_any ~ selected*x, data=ohie)
summary(fitLinAdj)

# collapse according to household
yHH <- tapply(ohie$doc_any, ohie$household_id, mean)
IDs <- match(names(yHH), ohie$household_id) 
selectedHH <- ohie$selected[IDs]
xHH <- x[IDs,]
summary(glm(yHH ~ selectedHH*xHH))

# compare person level and household level estimate
0.064230  +  c(-2,2)*0.006460  #person
0.063291  +  c(-2,2)*0.006838  #household

# bootstrap standard errors
library(boot)
n <- nrow(ohie)
hhWho <- split(1:n, ohie$household_id) # rows grouped by HH
bootFit <- function(hhlist, boothh) {
    bootSamp <- unlist(hhwho[boothh])   
    coef(glm(doc_any ~ selected*x,data = ohie,subset=bootSamp))[2]
}
bs <- boot(names(hhWho), bootFit, 99)

sd(bs$t)
quantile(bs$t, c(.025,.975))

library(AER)
sqrt(vcovCL(fitLinAdj, cluster = P$household_id)[2,2])

fitLogit <- glm(doc_any ~ selected*numhh, data=P, family="binomial")
summary(fitLogit)

predCats <- data.frame(selected=c(1,1,1,0,0,0),
     numhh=c('1','2','3+','1','2','3+'))
predY <- predict(fitLogit, newdata=predCats, type='response')
(pDiff <- predY[1:3] - predY[4:6])
(muNumHH <- table(P$numhh)/nrow(P))
pDiff%*%muNumHH


################## paidsearch
sem <- read.csv("paidsearch.csv")
sem$dma <- factor(sem$dma)

## quick summary: total revenue by date and treatment/controls
totalRev <- tapply(sem$revenue, sem[,c("date","search.stays.on")], sum)
## for plotting, we'll convert the row `dates' to R Date class
## see http://www.statmethods.net/input/dates.html for format codes.
date <- as.Date(rownames(totalRev), format="%d-%b-%y")
## order everything by date
totalRev <- totalRev[order(date),]
date <- sort(date)

## plot the revenues by group
plot(date, totalRev[,'0'], type="l", bty="n", col=2,
	ylim=range(totalRev), log="y", xlab="", ylab="revenue",lwd=2)
lines(date, totalRev[,'1'], type="l",lwd=2)
legend("right",col=c(1,2), lwd=2, bty="n",
	legend=c("control (search stays on)", "treatment (search goes off)"))
abline(v=as.Date("2012-05-22"), lty=2)
## and the difference between groups
plot(asdate, log(totalrev[,'1'])-log(totalrev[,'0']), 
	type="l", bty="n", col=3,lwd=2, xlab="", 
	ylab="log(rev_control) - log(rev_treat)")
abline(v=as.Date("2012-05-22"), lty=2,lwd=2)

#### plot of difference in logs
diffLogs<-log(totalRev[,'1']/totalRev[,'0'])
plot(date,diffLogs,type="l",lwd=2,col="green",
	ylab="log(revenue control) - log(revenue treatment)")
abline(v=as.Date("2012-05-22"), lty=2)

######  Actual Analysis #######
## we'll make use of the data.table package
library(data.table)
sem <- as.data.table(sem)
semAvg <- sem[,list(d=mean(1-search.stays.on), y=mean(log(revenue))), 
			by=c("dma","treatment_period")]
setnames(semAvg, "treatment_period", "t") #rename "treatment_period" to "t"
semAvg <- as.data.frame(semAvg)
head(semAvg)


library(AER)

semReg <- glm(y ~ d*t, data=semAvg)
coef(semReg)
sqrt(vcovCL(semReg, cluster=semAvg$dma)['d:t','d:t'])


dmaReg <- glm(y ~ dma + d*t, data=semAvg)
summary(dmaReg)$coef["d:t",]

## diff-in-diff means "difference in differences"
## we get the same thing just viewing this as a sample of n_dma differences.
r <- tapply(semAvg$y, semAvg$dma, function(y) y[2]-y[1])
d <- semAvg[match(names(r),semAvg$dma),"d"]
rBar <- tapply(r,d,mean)
rBarVar <- tapply(r, d, function(r) var(r)/length(r))
rBar[2]-rBar[1]
sqrt(sum(rBarVar))

######################################
#####  RD Analysis
######################################

D <- read.csv("RD.csv")
head(D)

par(mai=c(.8,.8,.3,.3))
boxplot(score ~ treat, data=D, horizontal=TRUE,col="darkgreen", 
	xlab="rank score minus reserve",
 	ylab="treatment (Ad in Main)")
abline(v=0, col=8, lty=3)


# a neighborhood
delta <- 3
above <- which(D$score > 0 & D$score <delta)
below <- which(D$score < 0 & D$score >-delta)

# constant model
muA <- mean(D$y[above])
muB <- mean(D$y[below])
(te <- muA - muB)
varA <- var(D$y[above])
varB <- var(D$y[below])
sdTE <- sqrt(varA/length(above) + varB/length(below))
te + c(-2,2)*sdTE

# local linear regression

delta <- 3
window <- which(D$score > -delta & D$score < delta)
summary(linFit <- glm(y ~ treat*score, data=D, subset=window))

# uncertainty quatification
library(AER)
seaTE <- sqrt(vcovHC(linFit)["treat","treat"])
coef(linFit)["treat"] + c(-2,2)*seaTE


fitA <- loess(y ~ score, data=D[above,], degree=1)
fitB <- loess(y ~ score, data=D[below,], degree=1)
rr <- seq(0.001,delta-0.001,length=100)
predA <- predict(fitA,rr) 
predB <- predict(fitB,-rr)


nr <- 10
rGrid <- seq(0, delta, length=nr)
dGrid <- data.frame(score=c(-rev(rGrid),rGrid), 
					treat=rep(c(0,1),each=nr))
linPred <- predict(linFit, newdata=dGrid)

par(mfrow=c(1,2), omi=c(.5,0,0,0), mai=c(.3,.9,.4,.2))
plot(y ~ score, data=D, subset=sample(c(above,below),10000), 
		cex=.3, col=8, bty="n", xlab="", main="data sample")
plot(rr, predA, xlab="", col="grey50", ylab="y",lwd=2, 
	main="RD analysis",
	ylim=range(c(predA,predB)), xlim=c(-delta,delta), type="l", bty="n")
legend("right", bty="n", lwd=2, lty=c(1,2,1), col=c("grey50","red","blue"), 
	legend=c("loess","constant","linear"))
lines(-rr, predB, col="grey50", lwd=2)
lines(dGrid$score[1:nr], linPred[1:nr], lwd=1.5, col=4)
lines(dGrid$score[nr+1:nr], linPred[nr+1:nr], lwd=1.5, col=4)
lines(dGrid$score[1:nr], rep(muB,nr), lwd=1.5, lty=2, col=2)
lines(dGrid$score[nr+1:nr], rep(muA,nr), lwd=1.5, lty=2, col=2)
lines(c(0,0),coef(linFit)[1] + c(0,coef(linFit)[2]), lwd=1.5, col=2, lty=3)
mtext(side=1, "rank score", outer=TRUE, line=1)

deltaSeq <- seq(.1,5,length=50)
ateD <- seaD <- rep(0,length(deltaSeq))
for(i in 1:length(deltaSeq)){
	print(i)
	fith <- lm(y ~ treat*score, data=D, 
		subset=which(abs(D$score) < deltaSeq[i]))
	ateD[i] <- coef(fith)[2]
	seaD[i] <- sqrt(vcovHC(fith)[2,2])
}

up <- ateD+2*seaD
down <- ateD-2*seaD


par(mai=c(.8,.8,.3,.3))

plot(deltaSeq, ateD, type="l", ylim=range(c(up,down)), 
	xlab="delta", ylab="ATE estimate", bty="n")
polygon(c(hh,rev(hh)), c(up,rev(down)), col=8, border=FALSE)
lines(deltaSeq, ateD, col="blue", lwd=2)

##################################

#endogeneity Airline demand pricing simulation
yFun <- function(e,p){
     y = 2 + 10*e-3*p + rnorm(length(e),0,.1)
     y[y<0] <- 0
     return(y) }

e <- rgamma(100,1,1)

z <- rgamma(100,1,1)
pObserved <- e + z
pCounterfactual <- rgamma(100,2,1)

yObserved <- yFun(e, pObserved)
yCounterfactual <- yFun(e, pCounterfactual)

plot(pObserved, yObserved, xlim=c(0,6), ylim=c(0,29), pch=21, bg=8,
     xlab="", ylab="", 
     bty="n", main="observed")
# Add OLS line
abline(lm(yObserved ~ pObserved), col="orange", lwd=2)

# Plot generated data
plot(pCounterfactual, yCounterfactual, xlim=c(0,6), ylim=c(0,29), pch=21, bg=8,
     xlab="", ylab="", 
     bty="n", main="counterfactual")

# Add the OLS line to the plot
abline(lm(yCounterfactual ~ pCounterfactual), col="orange", lwd=2)

#### 2SLS
pReg <- lm(pObserved ~ z)
pHat <- predict(pReg, data.frame(z=z))
lin2SLS <- lm(yObserved ~ pHat)
summary(lin2SLS)

summary(lm(y_observed ~ p_observed)) #compare with OLS

######################################
#### OHIE 2SLS
######################################
stage1 <- lm( medicaid ~ selected + numhh, data=P)
pHat <- predict(stage1, newdata=P)
stage2 <- lm( doc_any ~ pHat + numhh, data=P,
     x=TRUE #returns the model matrix used in regression
)
summary(stage2)


#get correct standard errors

# SEs using sandwich with complicated meat
library(Matrix)
resids <- P$doc_any - predict( stage2,
     newdata=data.frame(numhh=P$numhh, pHat=P$medicaid))
meat <- Diagonal(x=resids^2)
bread <- stage2$x%*%solve(t(stage2$x)%*%stage2$x)
sandwich <- t(bread)%*%meat%*%bread
print( segam <- sqrt(sandwich[2,2]) )

# 95% interval
coef(stage2)["pHat"] + c(-2,2)*segam

# SEs using AER package 
library(AER)
aerIV <- ivreg( doc_any  ~ medicaid + numhh | selected + numhh, data=P)
summary(aerIV)

# closer to sandwich result
sqrt(vcovHC(aerIV)[2,2])

# cluster by household
(seClust <- sqrt(vcovCL(aerIV, cluster = P$household_id)[2,2]) )

coef(aerIV)["medicaid"] + c(-2,2)*seClust

