# Import the data file on cars for sale into R
Cars <- read.csv("SoCalCars.csv", stringsAsFactors = TRUE)

# We'll now take a subset we feel is representative of used cars we want to test
# This sample includes used cars  with prices at or below $100,000, and mileage greater than or equal to 10,000 miles
# but less than 150,000 miles.
Cars <- subset(Cars,
        Cars$type != "New" & Cars$mileage >= 10000 & 
        Cars$mileage <= 150000 & Cars$price < 100000 & Cars$year >= 1970)[,-1]
# note that since the imported dataset has column names, it will be treated as a dataframe
# View the dimensions of the Cars dataset with dim()
dim(Cars) # output will be number of rows then number of columns

# View the unconditional mean of the price of car listings
mean(Cars$price)
# Compute the variance of the mean price for cars listed for sale, we use na.rm=TRUE here as well
var(Cars$price)/nrow(Cars)
# and the standard error (square root of the variance)
sd(Cars$price)/sqrt(nrow(Cars))

## Generates figure 3.1, two histograms for price of cars
# The hist() function calls the histogram
#png('SoCalCarsHistRaw.png', width=4, height=4, units="in", res=720)
hist(Cars$price, 
     freq=FALSE,
     main="", 
     xlab="Price", 
     col=8, 
     border="grey90"
)
#dev.off()
#png('SoCalCarsHistLog.png', width=4, height=4, units="in", res=720)
hist(log(Cars$price), 
     freq=FALSE, 
     main="", 
     col=8,
     border="grey90",
     xlab="log(Price)"
)
#dev.off()

# If we consider the Cars dataset to be a sample of the cars for sale in 
# Southern California, then the sampling distribution's mean should be
xbar <- mean(Cars$price)
# The sampling distribution's standard deviation should be
xbse <- sd(Cars$price)/sqrt(nrow(Cars))
# Note that nrow() computes the number of rows/observations for a given dataframe

## Plot the sampling distribution
# First, create a vector of values to be fed in the sampling distrubtion pdf
calc_values <- seq(xbar-4*xbse,xbar+4*xbse,length=100)
# Note the values chosen above are based on the distribution of prices in the data
# Second, feed the values created above into the sampling distribution pdf created 
# with dnorm function
samp_pdf_values <- dnorm(calc_values, xbar, xbse)
# Third, plot the calc_values versus the sample_pdf_values for a view of the sample distribution
#pdf("SoCalCarsMeanDensity.pdf", width=4, height=4)
plot(calc_values, 
     samp_pdf_values, 
     type="l", 
     lwd=1.5,
     xlab="average used car price", 
     ylab="density"
)
#dev.off()

## hypothesis testing

# model for CPO price premium
summary( glm(price ~ certified, data=Cars) )
# 95% confidence interval for overall car average price
19674.96 + 1.96*297.3*c(-1,1) 
# ~95% confidence interval for CPO premium 
14331 + c(-2,2)*1363.6

### prediction
# conditional expectation CI
certreg <- glm(price ~ certified, data=Cars)
( certpred <- predict(certreg, data.frame(certified=1), se.fit=TRUE) )
certpred$fit + c(-2,2)*certpred$se.fit

# prediction interval
predvar <- certpred$se.fit^2 + certpred$residual.scale^2
sqrt(predvar)
certpred$fit + c(-2,2)*sqrt(predvar)

### carsreg
# larger car regression model.  First we relevel the reference levels
Cars$make <- relevel(Cars$make, "Ford")
Cars$body <- relevel(Cars$body, "Sedan")
Cars$city <- relevel(Cars$city, "Costa Mesa")


carsreg <- glm(log(price) ~ log(mileage) + make + 
       year + certified + body + city, data=Cars)
summary(carsreg)

# Extract p-values for each regressor from the regression ouput
pvals <- summary(carsreg)$coef[-1,"Pr(>|t|)"]
length(pvals)
nullps <- runif(116)

#png('SoCalCarsPvals.png', width=5, height=5, units="in", res=720)
hist(pvals, col=8, breaks=10, xlab="p-values", main="Cars Regression", freq=FALSE)
#dev.off()

#png('SoCalCarsNullPvals.png', width=5, height=5, units="in", res=720)
hist(nullps, col=8, breaks=10, xlab="p-values", main="Null Distribution", ylim=c(0,7), freq=FALSE)
#dev.off()


# Function to get significance cut-off alpha from FDR q
fdr_cut <- function(pvals, q){
        pvals <- pvals[!is.na(pvals)]
        N <- length(pvals)
        k <- rank(pvals, ties.method="min")
        max(pvals[ pvals<= (q*k/N) ])
}
# @ 1/10% FDR
cutoff10 <- fdr_cut(pvals,q=.1)
print(cutoff10)
print(sum(pvals<=cutoff10))

# produce the order statistic plots and  the FDR control plot
sig <- factor(pvals<=cutoff10)
#png('SoCalCarsOrderedPvals.png', width=4, height=4, units="in", res=720)
plot(sort(pvals), pch=21, cex=.5, col="gray20",
    bty="n", xlab="rank", ylab="p-values")
points(sort(nullps), pch=24, cex=.5, col="blue")
legend("topleft", legend=c("null","observed"), pch=c(24,21), 
       col=c("blue", "gray20"), bty="n")
#dev.off()

#png('SoCalCarsFDR.png', width=4, height=4, units="in", res=720)
plot(sort(pvals),
     col=c("grey60","red")[sig[order(pvals)]], 
     # above colors the significant p-values red
     pch=20, bty="n", xlab="rank", ylab="p-values" )
## Add the line of significance for q=0.1
q <- 0.1
abline(a=0, b=0.1/length(pvals), lwd=1.5)
#dev.off()

nullpval <- runif(length(pvals))
plot(sort(nullpval))


### bootstrapping

## basic bootstrap for average price
set.seed(1)
muhats <- c() # empty set of estimates 
for(b in 1:1000){ # we'll draw B estimates
    # resample with-replacement and estimate mu.hat
    samp_b <- sample.int(nrow(Cars), replace=TRUE)
    muhat_b <- mean(Cars$price[samp_b])
    muhats <- c(muhats, muhat_b) # append to the set
}
sd(muhats)

#pdf("SoCalCarsBootPrice.pdf", width=4, height=4)
hist(muhats, main="", xlab="average used car price", freq=FALSE,
     col=8, border="grey90", breaks=20,
     xlim=c(xbar-4*xbse,xbar+4*xbse), 
     ylim=c(0, max(samp_pdf_values)) )
lines(calc_values, 
     samp_pdf_values, 
     type="l", 
     col="blue"
)
#dev.off()


### making decisions with the bootstrap

## p-value calculation (note the absolute value on z)
( z <- (mean(Cars$price) - 20000)/292 )
2*pnorm(-abs(z))

### mileage elasticity of price
# glm analysis results
betaStats <- summary(carsreg)$coef["log(mileage)",]
round(betaStats, 5)

# function 
getBeta <- function(data, obs){
    fit <- glm(log(price) ~ log(mileage) + make + year 
               + certified + body + city, data=data[obs,])
    return(fit$coef["log(mileage)"])
}
getBeta(Cars, 1:nrow(Cars))

#########  side box on bootstrapping and parallel
library(parallel)
library(boot)
detectCores()
system.time( boot(Cars, getBeta, 2000) )
system.time( boot(Cars, getBeta, 2000, parallel="snow", ncpus=8 ) )
## this one will not give you any speedup on Windows
system.time( boot(Cars, getBeta, 2000, parallel="multicore", ncpus=8) )
############

## test statistics
library(parallel)
library(boot)
( betaBoot <- boot(Cars, getBeta, 2000, 
                 parallel="snow", ncpus=detectCores()) )


(bhat <- betaStats["Estimate"])
betaBoot$t0
sd(betaBoot$t)
mean(betaBoot$t) - bhat

## standard errors
sd(betaBoot$t)
betaStats["Std. Error"]

## recalculating test stats and p values
bhat/sd(betaBoot$t)
2*pnorm(-abs(bhat/sd(betaBoot$t)))

## HC  robust standard errors 
library(sandwich)
library(lmtest)
VHC = vcovHC(carsreg, "HC0")
hcstats <- coeftest(carsreg, vcov = VHC)
round(hcstats["log(mileage)",], 5)

## plot them all
#pdf("SoCalCarsElasticity.pdf", width=4, height=4)
hist(betaBoot$t, freq=FALSE, ylim=c(0,25), border="grey90",
     main="", xlab="price-mileage elasticity")
bhatse <- betaStats["Std. Error"]
grid <- seq(bhat-6*bhatse,bhat+6*bhatse,length=100)
lines(grid, dnorm(grid, bhat, bhatse), col="navy", lwd=1.5)
lines(grid, dnorm(grid, bhat, hcstats["log(mileage)","Std. Error"]), 
      col="orange", lwd=1.5)
legend("topright", legend=c("Basic","Bootstrap","HC"), 
       col=c("navy","grey","orange"), pch=15, bty="n")
#dev.off()

### clustered standard errors. 
CarsByDealer <- split(Cars, Cars$dealer)
length(CarsByDealer)
getBetaBlock <- function(data, ids){
    data <- do.call("rbind",data[ids])
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data)
    return(fit$coef["log(mileage)"])
}
getBetaBlock(CarsByDealer, 1:length(CarsByDealer))

( betaBootB <- boot(CarsByDealer, getBetaBlock, 
                          2000, parallel="snow", ncpus=detectCores()) )

Vblock <- vcovCL(carsreg, cluster=Cars$dealer)
clstats <- coeftest(carsreg, vcov = Vblock)
round(clstats["log(mileage)",], 5)
##############

### confidence intervals
# using theoretical standard errors
bhat + c(-1,1)*1.96*sd(betaBootB$t)

## distribution of bootstrapped statistics
quantile(betaBootB$t,c(0.025, 0.975))

## look at the distribution of errors
betaErrors <- betaBootB$t - bhat
quantile(bhat - betaErrors,c(.025,.975))

## bias example: e^b
Cars[1000,]
( pred <- predict(carsreg, Cars[c(1000),]) )
( phat <- exp(pred) )
getPrice <- function(data, ids, xpred){
    data <- do.call("rbind", data[ids])
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data)
    return(exp(predict(fit,xpred)))
}
getPrice(CarsByDealer, 1:length(CarsByDealer), Cars[1000,])

# if you change this seed, you might get errors in
# the random chance that one of your boostrap samples includes no 
# cars from the same city as our Caravan (Montclair)
set.seed(101)  
( priceBoot <- boot(CarsByDealer, getPrice, xpred=Cars[1000,], 
                    2000, parallel="snow", ncpus=detectCores()) )


priceErrors <- priceBoot$t - phat
mean(priceErrors)
phat - mean(priceErrors)

# bias corrected CI
quantile(phat-priceErrors, c(.025,.975))
# CI on original parameter sample
quantile(priceBoot$t, c(.025,.975))

# same things with the getCI function
getCI <- function(bo, p=c(.025,.975)) quantile(2*bo$t0 - bo$t, p)
getCI(priceBoot)

### parametric bootstrap
CarsSim <- function(data, mle){
    n <- nrow(data)
    Ey <- predict(mle, data)
    data$price <- exp( rnorm(n, Ey, summary(mle)$dispersion)  )
    return(data)
}
simcars <- CarsSim(Cars, carsreg)
simcars[1,]


getBetaPar <- function(data){
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data)
    return(fit$coef["log(mileage)"])
}
( parboot <- boot(Cars, getBetaPar, 2000, 
    sim = "parametric", ran.gen = CarsSim, mle=carsreg,
    parallel="snow", ncpus=detectCores()) )

# again, but with nonconstant variance
CarsSimNCV <- function(data, mle){
    n <- nrow(data)
    Ey <- predict(mle, data)
    # replace the dispersion with abs(residuals)
    data$price <- exp( rnorm(n, Ey, abs(mle$residuals) ))
    return(data)
}
( parbootNCV <- boot(Cars, getBetaPar, 2000, 
    sim = "parametric", ran.gen = CarsSimNCV, mle=carsreg,
    parallel="snow", ncpus=detectCores()) )

#####
## Bayesian estimation of generalized linear models
# You can install with install.packages("arm")
library(arm)

# Bayesian GLM
carsregBayes <- bayesglm(log(price) ~ log(mileage) + make + 
                           year + certified + body + city, data=Cars)
summary(carsregBayes)

# Simulate posterior distribution for our Bayesian example
posterior <- coef(sim(carsregBayes))

bhatse <- betaStats["Std. Error"]
grid <- seq(bhat-6*bhatse,bhat+6*bhatse,length=100)

#pdf("SoCalCarsBayes.pdf", width=4, height=4)
plot(grid, dnorm(grid, bhat, bhatse), xlim=range(grid), ylim=c(0,30), type="l",
    xlab="price-mileage elasticty", ylab="density", main="", bty="n", lwd=1.5, col="navy")
lines(density(betaBoot$t), col="grey50",  lwd=1.5)
lines(density(posterior[,"log(mileage)"]), col=2,  lwd=1.5)
legend("topright", legend=c("Basic","Bootstrap","Bayesian"), 
       col=c("navy","grey50","red"), pch=15, bty="n")
#dev.off()
