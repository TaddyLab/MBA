####### CHAPTER 3 - Space and Time #######

###airline passenger data

#read in the data and add date column
air<-read.csv("airline.csv")
air$date<-paste(air$Month,"-01-19",air$Year,sep="") 
air$date<-as.Date(air$date,"%m-%d-%Y")
str(air) 

#plot it
plot(air$date,air$Passengers,type="l",xlab="Date",
     ylab="Monthly Passengers",col="maroon",lwd=2) 
#log(passengers)
plot(air$date,log(air$Passengers),xlab="Date",
     ylab="Monthly Passengers",type="l",col="navy",lwd=2)

#Simple linear regression
fitSLR<-glm(log(air$Passengers)~air$date)
coef(fitSLR)

##can fit using index variable instead of date
air$index<-1:length(air$Passengers)
air$index[1:10]
fitSLRindex<-glm(log(air$Passengers)~air$index)
coef(fitSLRindex)

#predict passengers using 75th time period
predict.lm(fitSLR,newdata=data.frame(date=as.Date("1955-03-01"))) # 75th date
# 5.5668651340779238 
predict.lm(fitSLR2,newdata=data.frame(index=75)) #index
# 5.5672969128861105 
# They are a little off partly due to rounding
# and partly due to the fact that dates account for differing days in a month

### Plot using date and compare to using index
plot(air$date,log(air$Passengers),xlab="Date",ylab="Monthly
     Passengers",type="l",col="navy",lwd=2)
abline(fitSLR)
plot(air$index,log(air$Passengers),xlab="Date Index",ylab="Monthly
     Passengers",type="l",col="navy",lwd=2)
abline(fitSLRindex)

### add seasonal monthly means
fitSeasonal<-glm(log(air$Passengers)~air$index+factor(air$Month))
round(coef(fitSeasonal),4)

###plot series and fitted values for seasonal means model
plot(air$date, log(air$Passengers), xlab="Date",
	ylab="log passengers", type="l", col="navy", lty=1, lwd=2)
lines(air$date, fitted(fitSeasonal), col="maroon", lwd=2, lty=2)
legend("topleft", legend=c("true","fit"), bty="n", lwd=3, col=c("navy","maroon"))


### AR(1)
lag <- head(log(air$Passengers),-1) #all except last entry
passengersL <- air$Passengers[-1] #all except first entry
monthL <- air$Month[-1]
indexL <- air$index[-1]
fitAR1<-glm(log(passengersL)~ indexL+factor(monthL)+lag)
summary(fitAR1)

### DJA
dja<-read.csv("dja.csv")
n<-length(dja$DJA)
summary(ARdj <- glm(dja$DJA[2:n] ~ dja$DJA[1:(n-1)]))

# prices to returns
returns <- (dja$DJA[2:n]-dja$DJA[1:(n-1)])/dja$DJA[1:(n-1)]
summary(glm(returns[2:n] ~ returns[1:(n-1)]))

