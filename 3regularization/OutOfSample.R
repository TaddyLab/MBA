## the introductory overfit example
set.seed(865) # this allows you to get the same "random" data
x<-seq(-3,6,0.5) # define x's
data<-(1.5 + (x-1)^2) + rnorm(length(x),0,2.5) #a quadratic function with random noise added
data

fitL<-glm(data~x) #model with a straight line
fitQ<-glm(data~x+I(x^2)) #model with a quadratic
fit18<-glm(data~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+ #18th degree polynomial
	I(x^8)+I(x^9)+I(x^10)+I(x^11)+I(x^12)+I(x^13)+I(x^14)+
	I(x^15)+I(x^16)+I(x^17)+I(x^18))
par(mfrow=c(1,3)) #set graph window to 1 row of 3 graphs
plot(x,data,ylab="y",pch=21,col="darkgreen",bg="darkgreen",bty="n")
abline(fitL,lwd=2) #add regression line
plot(x,data,ylab="y",pch=21,col="darkgreen",bg="darkgreen",bty="n")
lines(x,fitted(fitQ),lwd=2) #add fitted model
plot(x,data,ylab="y",pch=21,col="darkgreen",bg="darkgreen",bty="n")
lines(x,fitted(fit18),lwd=2)


1-fitL$deviance/fitL$null.deviance  #R2 linear
1-fitQ$deviance/fitQ$null.deviance  #R2 quadratic
1-fit18$deviance/fit18$null.deviance  #R2 poly 18

# the code below simulates new data for an OOS experiment
r2OOSSLR<-c(); r2OOSQuad<-c(); r2OOSPoly<-c()
for (i in 1:10000){
	dataNew<-(1.5 + (x-1)^2) + rnorm(length(x),0,2.5)
 
	dev<-sum((dataNew-fitted(fitL))^2)
	dev.null<-sum((dataNew-mean(dataNew))^2)
	r2L<-1-dev/dev.null
	r2OOSSLR <- c(r2OOSSLR,r2L)
 
	dev<-sum((dataNew-fitted(fitQ))^2)
	dev.null<-sum((dataNew-mean(dataNew))^2)
	r2Q<-1-dev/dev.null
	r2OOSQuad <- c(r2OOSQuad,r2Q)

	dev<-sum((dataNew-fitted(fit18))^2)
	dev.null<-sum((dataNew-mean(dataNew))^2)
	r2P<-1-dev/dev.null
	r2OOSPoly <- c(r2OOSPoly,r2P)
  }

boxplot(r2OOSSLR,r2OOSQuad,r2OOSPoly,
     names=c("SLR","Quadratic","18th Polynomial"),
     col=c("blue","green","yellow"),
     main="R-square OOS",cex.main=1.5)

summary(r2OOSSLR)
summary(r2OOSQuad)
summary(r2OOSPoly)

length(r2OOSSLR[r2OOSSLR<0])


