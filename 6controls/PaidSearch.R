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
## a bunch of wrangling to get the data in the right format
ps <- read.csv("paidsearch.csv") 

library(tidyr)
ebayD <- ps[,-(3:4)] %>% spread(dma,revenue)
ebayD$date <- as.Date(ebayD$date, format="%d-%b-%y")
ebayD <- ebayD[order(ebayD$date),]
row.names(ebayD) <- as.character(ebayD$date)
ebayD <- t(ebayD[,-1])

## May 22 is t=52, so tstar=51.
tstar <- 51
ebayD[1:4,51:53]
dim(ebayD)

# Create a single average 'treatment' DMA series.
d <- ebay$ssm.turns.off[match(rownames(ebayD),ebay$dma)]
Y <- colMeans(ebayD[d==1,])
ebayD <- log(rbind(Y,ebayD[d!=1,]))
ebayD[1:4,51:53]

# take the same function from basque.R
library(gamlr)
synthc <- function(D, tstar, treated, ...){
	y <- D[treated,]
	x <- t(D[-treated,]) 
	fit <- gamlr( x[1:tstar,], y[1:tstar], ...)
	y0hat <- predict(fit, x)[,1]
	gamhat <- y - y0hat
	ate <- mean( gamhat[-(1:tstar)] )
	return(list(y0hat=y0hat, gamhat=gamhat, ate=ate, fit=fit ) )
}

# estimate the treatment effect
ebaySC <- synthc(ebayD, tstar=51, 1)
ebaySC$ate

# plots
png('ebaySCfit.png', width=4, height=4, units="in", res=720)
plot(ebaySC$fit)
dev.off()

png('ebaySCpred.png', width=4, height=4, units="in", res=720)
asdate <- as.Date(colnames(ebayD))
plot(asdate, ebayD[1,], type="l", ylim=c(11.2,12), bty="n", xlab="", ylab="log Average Daily Revenue")
lines(asdate, ebaySC$y0hat, col="orange")
abline(v=as.Date("2012-05-22"), lty=2)
legend("topright", bty="n", legend=c("observed","synthetic"), 
	lwd=2, col=c(col=c("black","orange")) )

dev.off()

library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("ebayD", "gamlr", "synthc"))
getATE <- function(j){ synthc(ebayD, 52, j)$ate }
ate <- parSapply(cl, 1:nrow(ebayD), getATE)
mean(abs(ate[-1]) > abs(ate[1]))

