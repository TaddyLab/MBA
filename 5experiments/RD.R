####### Causal Inference - Experiments #######
####### RD #######

dat <- read.csv("RD.csv")
head(dat)

par(mai=c(.8,.8,.3,.3))
boxplot(score ~ treat, data=dat, horizontal=TRUE, 
	xlab="rank score minus reserve",
 	ylab="treatment (Ad in Main)")
abline(v=0, col=8, lty=3)

delta <- 3
window <- which(dat$score > -delta & dat$score < delta)
summary(linfit <- glm(y ~ treat*score, data=dat, subset=window))

library(sandwich)
library(lmtest)
coeftest(linfit, vcov=vcovHC(linfit))

# difference in means
summary( meandiff <- glm( y ~ treat, data=dat, subset=window) )

## plots
#png('RDanalysisData.png', width=4, height=4, units="in", res=720)
plot(y ~ score, data=dat, subset=sample(c(above,below),10000), 
		cex=.3, col=8, bty="n", xlab="r", main="")
#dev.off()

mub <- coef(meandiff)[1]
mua <- coef(meandiff)[1] + coef(meandiff)[2]
#png('RDanalysisZoomed.png', width=4, height=4, units="in", res=720)
plot(rr, preda, xlab="r", col="grey50", ylab="y",lwd=2, 
	main="",
	ylim=range(c(preda,predb)), xlim=c(-w,w), type="l", bty="n")
legend("right", bty="n", lwd=2, lty=c(1,2,1), col=c("grey50","red","blue"), 
	legend=c("loess","constant","linear"))
lines(-rr, predb, col="grey50", lwd=2)
lines(dgrid$score[1:nr], linpred[1:nr], lwd=1.5, col=4)
lines(dgrid$score[nr+1:nr], linpred[nr+1:nr], lwd=1.5, col=4)
lines(dgrid$score[1:nr], rep(mub,nr), lwd=1.5, lty=2, col=2)
lines(dgrid$score[nr+1:nr], rep(mua,nr), lwd=1.5, lty=2, col=2)
lines(c(0,0),coef(linfit)[1] + c(0,coef(linfit)[2]), lwd=1.5, col=4, lty=3)
#dev.off()


hh <- seq(.1,5,length=50)
ateh <- seah <- rep(0,length(hh))
for(i in 1:length(hh)){
	print(i)
	fith <- lm(y ~ treat*score, data=dat, 
		subset=which(abs(dat$score) < hh[i]))
	ateh[i] <- coef(fith)[2]
	seah[i] <- sqrt(vcovHC(fith)[2,2])
}

up <- ateh+2*seah
down <- ateh-2*seah


par(mai=c(.8,.8,.3,.3))

plot(hh, ateh, type="l", ylim=range(c(up,down)), 
	xlab="window size", ylab="ATE estimate", bty="n")
polygon(c(hh,rev(hh)), c(up,rev(down)), col=8, border=FALSE)
lines(hh, ateh, col="blue", lwd=2)

