################## paidsearch

ps <- read.csv("paidsearch.csv") 
## wrangling
library(data.table)
ebay <- as.data.table(ps)
ebay <- ebay[,list(ssm.turns.off=mean(1-search.stays.on), 
					revenue=mean(revenue)), 
			by=c("dma","treatment_period")]
setnames(ebay, "treatment_period", "post.treat") 
ebay <- as.data.frame(ebay)
head(ebay)

## run the DiD analysis
did <- glm(log(revenue) ~ ssm.turns.off*post.treat, data=ebay)
coef(did)

library(sandwich)
library(lmtest)
coeftest(did, vcov=vcovCL(did, cluster=ebay$dma))


## same thing by comparing a sample of n_dma differences.
r <- tapply(log(ebay$revenue), ebay$dma, function(y) y[2]-y[1])
d <- ebay[match(names(r),ebay$dma),"ssm.turns.off"]
rBar <- tapply(r,d,mean)
rBar[2]-rBar[1]

rBarVar <- tapply(r, d, function(r) var(r)/length(r))
sqrt(sum(rBarVar))

## plots
## quick summary: total revenue by date and treatment/controls
totalrev <- tapply(ps$revenue, ps[,c("date","search.stays.on")], sum)
## for plotting, we'll convert the row `dates' to R Date class
## see http://www.statmethods.net/input/dates.html for format codes.
asdate <- as.Date(rownames(totalrev), format="%d-%b-%y")
## order everything by date
totalrev <- totalrev[order(asdate),]
asdate <- sort(asdate)

## revenue over time
plot(asdate, totalrev[,'0'], type="l", bty="n", col=2, bty="n",
	ylim=range(totalrev), log="y", xlab="", ylab="revenue",lwd=2)
lines(asdate, totalrev[,'1'], type="l",lwd=2)
legend("right",col=c(1,2), lwd=2, bty="n",
	legend=c("control (search stays on)", "treatment (search goes off)"))
abline(v=as.Date("2012-05-22"), lty=2)

## the log difference
plot(asdate, log(totalrev[,'1'])-log(totalrev[,'0']), 
	type="l", bty="n", col=3,lwd=2, xlab="", 
	ylab="log(rev_control) - log(rev_treat)")
abline(v=as.Date("2012-05-22"), lty=2,lwd=2)

### synthetic controls analysis
## a bunch of wrankling to get the data in the right format
library(tidyr)
psfat <- ps[,-(3:4)] %>% spread(dma,revenue)
psfat$date <- as.Date(psfat$date, format="%d-%b-%y")
psfat <- psfat[order(psfat$date),]

# 
t <- as.numeric(psfat$date >= as.Date("2012-05-22"))
may22 <- which(psfat$date == as.Date("2012-05-22"))

row.names(psfat) <- as.character(psfat$date)
psfat <- psfat[,-1]
dfat <- ebay[match(colnames(psfat),ebay$dma),"ssm.turns.off"]
pstreat <- rowMeans(psfat[,dfat==1])
psctrl <- psfat[,dfat==0]
psY <- log(cbind(pstreat,psctrl))

library(gamlr)
synthc <- function(Y, treated, when, ...){
	Y0t <- Y[1:(when-1),]
	fit <- gamlr( Y0t[,-treated], Y0t[,treated], ...)
	plot(fit)
	y0hat <- predict(fit, Y[,-treated])[,1]
	ate <- mean( (Y[,treated] - y0hat)[when:nrow(Y)] )
	return(list(w=coef(fit)[,1], y0hat=y0hat, ate=ate ) )
}

sc <- synthc(psY, 1, may22)
plot(asdate, psY[,1], type="l", lwd=2, xlab="", ylab="log Average Daily Revenue")
lines(asdate, sc$y0hat, col="orange", lwd=2)
abline(v=as.Date("2012-05-22"), lty=2,lwd=2)

library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("psY", "gamlr", "synthc"))

getATE <- function(j){ synthc(psY, j, 52)$ate }
ate <- parSapply(cl, 1:ncol(psY), getATE)
mean(abs(ate[-1]) > abs(ate[1]))

