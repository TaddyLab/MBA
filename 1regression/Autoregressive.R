####### Regression #######

###airline passenger data

#read in the data and add date column
air<-read.csv("airline.csv")
air[c(1,70,144),]
air$date<-paste("19",air$Year,"-",air$Month,"-01", sep="") 
air$date[c(1,70,144)]

air$date<-as.Date(air$date)
air$date[c(1,70,144)]
class(air$date)

as.numeric(air$date[c(1,70,144)])
as.numeric(as.Date("1970-01-01")) 

#plot it
#png("airline.png", width=4, height=4, units="in", res=720)
plot(air$date,air$Passengers,type="l",xlab="Date", bty="n",
     ylab="Monthly Passengers",col="maroon",lwd=2) 
#dev.off()

#png("logairline.png", width=4, height=4, units="in", res=720)
plot(air$date,log(air$Passengers),xlab="Date", bty="n",
     ylab="Log Monthly Passengers",type="l",col="navy",lwd=2)
#dev.off()

## Simple linear regression
air$t <- 1:nrow(air)
fitAirSLR <- glm(log(Passengers)~t, data=air)
coef(fitAirSLR)
exp(coef(fitAirSLR)["t"])

## add seasonal monthly means
air$Month <- factor(air$Month)
levels(air$Month)
fitAirMonth <- glm(log(Passengers) ~ t + Month, data=air)
round(coef(fitAirMonth),2)

###plot series and fitted values for seasonal means model
#png("airlinefit.png", width=6, height=4, units="in", res=720)
plot(air$date,log(air$Passengers),type="l",xlab="Date", bty="n",
     ylab="Log Passengers",col="navy",lwd=1.5) 
lines(air$date, fitAirMonth$fitted, col=2, lwd=1.5, lty=2)
legend("topleft", col=c("navy","red"), lwd=1.5, lty=c(1,2), legend=c("data","fitted"),bty="n")
#dev.off()

#png("decomposition.png", width=7, height=4, units="in", res=720)
par(mfrow=c(1,2))
plot(air$date, predict(fitAirMonth, 
					newdata=data.frame(t=air$t, Month="1")),
	type="l",xlab="Date", bty="n",
    ylab="Linear trend", col="red",lwd=1.5)
plot(air$date, predict(fitAirMonth, 
					newdata=data.frame(t=72, Month=air$Month)),
	type="l",xlab="Date", bty="n",
    ylab="Seasonal trend", col="blue",lwd=1.5)
#dev.off()

## plot residuals to see autocorrelation
#png("airlineresid.png", width=4, height=4, units="in", res=720)
plot(air$date, fitAirMonth$residuals,
	type="l",xlab="Date", bty="n",
    ylab="Residuals", col="black",lwd=1.5)
#dev.off()

## plot residuals against lagged residuals 
#  there is a clear correlation of residuals with the past residual
#png("airlagplot.png", width=4, height=4, units="in", res=720)
plot(head(fitAirMonth$residuals,-1), tail(fitAirMonth$residuals,-1),
	xlab="lagged Residual", bty="n",
    ylab="Residual",pch=20, col="grey20")
#dev.off()

## plot ACF
plot(acf(fitAirMonth$residuals))

### AR(1)
air$lag1 <- NA
air$lag1[-1] <- air$Passengers[-nrow(air)]
air[1:3,]

fitAirAR1 <- glm(log(Passengers) ~ log(lag1) + t + Month, data=air)
coef(fitAirAR1)["log(lag1)"]

## plot AR(1) residuals to see they look much more random
#png("airAR1resid.png", width=4, height=4, units="in", res=720)
plot(air$date[-1], fitAirAR1$residuals,
	type="l",xlab="Date", bty="n",
    ylab="Residuals", col="black",lwd=1.5)
#dev.off()

## plot ACF
#png("airAR1acf.png", width=4, height=4, units="in", res=720)
plot(acf(fitAirAR1$residuals),
	xlab="lag", bty="n",main="", ylab="ACF")
#dev.off()

### DJA
dja <- read.csv("dja.csv")[,1]
n<-length(dja)
coef(ARdj <- glm(dja[-1] ~ dja[-n]))

# prices to returns
returns <- (dja[-1]-dja[-n])/dja[-n]
coef(glm(returns[-1] ~ returns[-(n-1)]))



#png("dowjones.png", width=4, height=4, units="in", res=720)
plot(dja, type="l",
	xlab="day", ylab="DJA", bty="n",main="", col="blue")
#dev.off()
#png("dowjonesReturns.png", width=4, height=4, units="in", res=720)
plot(returns, type="l",
	xlab="day", ylab="DJA returns", bty="n",main="", col="green")
#dev.off()

