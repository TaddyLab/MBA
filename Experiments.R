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

