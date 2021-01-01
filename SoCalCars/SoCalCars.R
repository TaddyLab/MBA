
# Import the data file on cars for sale into R
Cars <- read.csv("SoCalCars.csv", stringsAsFactors = TRUE)

# We'll now take a sample we feel is representative of used cars we want to test
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
pdf('SoCalCarsHist.pdf', width=8, height=4)
par(mfrow=c(1,2))
hist(Cars$price, 
     freq=FALSE,
     main="", 
     xlab="Price", 
     col=8, 
     border="grey90"
)
hist(log(Cars$price), 
     freq=FALSE, 
     main="", 
     col=8,
     border="grey90",
     xlab="log(Price)"
)
dev.off()

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
pdf("SoCalCarsMeanDensity.pdf", width=4, height=4)
plot(calc_values, 
     samp_pdf_values, 
     type="l", 
     lwd=1.5,
     xlab="average used car price", 
     ylab="density"
)
dev.off()

## hypothesis testing

# model for CPO price premium
summary( glm(price ~ certified, data=Cars) )
# 95% confidence interval for overall car average price
19674.96 + 1.96*297.3*c(-1,1) 
# ~95% confidence interval for CPO premium 
14331 + c(-2,2)*1363.6

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

pdf('SoCalCarsPvals.pdf', width=10, height=5)
par(mfrow=c(1,2))
hist(pvals, col=8, breaks=10, xlab="p-values", main="Cars Regression", freq=FALSE)
hist(nullps, col=8, breaks=10, xlab="p-values", main="Null Distribution", ylim=c(0,7), freq=FALSE)
dev.off()


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
pdf('SoCalCarsFDR.pdf', width=8, height=4)
par(mfrow=c(1,2))
plot(sort(pvals), pch=21, cex=.5, col="gray20",
    bty="n", xlab="rank", ylab="p-values")
points(sort(nullps), pch=24, cex=.5, col="blue")
legend("topleft", legend=c("null","observed"), pch=c(24,21), 
       col=c("blue", "gray20"), bty="n")
plot(sort(pvals),
     col=c("grey60","red")[sig[order(pvals)]], 
     # above colors the significant p-values red
     pch=20, bty="n", xlab="rank", ylab="p-values" )

## Add the line of significance for q=0.1
q <- 0.1
abline(a=0, b=0.1/length(pvals), lwd=1.5)
dev.off()


nullpval <- runif(length(pvals))
plot(sort(nullpval))

### prediction

# conditional expectation CI
Cars[c(1000),c("make","model","year","mileage")]
(caravan <- predict(carsreg, Cars[c(1000),], se.fit=TRUE))
caravan$fit + c(-2,2)*caravan$se.fit
exp(caravan$fit + c(-2,2)*caravan$se.fit)
# prediction interval
predvar <- caravan$se.fit^2 + caravan$residual.scale^2
sqrt(predvar)
caravan$fit + c(-2,2)*sqrt(predvar)
exp(caravan$fit+ c(-2,2)*sqrt(predvar))

### bootstrapping

## basic bootstrap for average price
B <- 1000
muhats <- c() # empty set of estimates 
for(b in 1:B){ # we'll draw B estimates
    # resample with-replacement and estimate mu.hat
    samp_b <- sample.int(nrow(Cars), replace=TRUE)
    muhat_b <- mean(Cars$price[samp_b])
    muhats <- c(muhats, muhat_b) # append to the set
}
sd(muhats)

pdf("SoCalCarsBootPrice.pdf", width=4, height=4)
hist(muhats, main="", xlab="average used car price", freq=FALSE,
     col=8, border="grey90", breaks=20,
     xlim=c(xbar-4*xbse,xbar+4*xbse), 
     ylim=c(0, max(samp_pdf_values)) )
lines(calc_values, 
     samp_pdf_values, 
     type="l", 
     col="blue"
)
dev.off()

### making decisions with the bootstrap

## 
( z <- (mean(Cars$price) - 20000)/298 )
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

#########
library(parallel)
library(boot)
detectCores()
system.time( boot(Cars, getBeta, 2000) )
system.time( boot(Cars, getBeta, 2000, parallel="multicore", ncpus=8) )
system.time( boot(Cars, getBeta, 2000, parallel="snow", ncpus=8 ) )
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

bhat/sd(betaBoot$t)
2*pnorm(-abs(bhat/sd(betaBoot$t)))

## HC standard errors 
#boxplot(carsreg$residuals ~ Cars$dealer)
library(sandwich)
library(lmtest)
hcstats <- coeftest(carsreg, vcov = vcovHC(carsreg, "HC0"))
round(hcstats["log(mileage)",], 5)

## plot them all
pdf("SoCalCarsElasticity.pdf", width=4, height=4)
hist(betaBoot$t, freq=FALSE, ylim=c(0,25), border="grey90",
     main="", xlab="price-mileage elasticity")
bhatse <- betaStats["Std. Error"]
grid <- seq(bhat-6*bhatse,bhat+6*bhatse,length=100)
lines(grid, dnorm(grid, bhat, bhatse), col="navy", lwd=1.5)
lines(grid, dnorm(grid, bhat, hcstats["log(mileage)","Std. Error"]), 
      col="orange", lwd=1.5)
legend("topright", legend=c("Basic","Bootstrap","HC"), 
       col=c("grey","navy","orange"), pch=15, bty="n")
dev.off()

## clustered standard errors. 
CarsByDealer <- split(Cars, Cars$dealer)
length(CarsByDealer)
getBetaBlock <- function(data, ids){
    data <- do.call("rbind",data[ids])
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data)
    return(fit$coef["log(mileage)"])
}
getBetaBlock(CarsByDealer, 1:length(CarsByDealer))

( betaBootBlock <- boot(CarsByDealer, getBetaBlocked, 
                          2000, parallel="snow", ncpus=8) )

clstats <- coeftest(carsreg, vcov = vcovCL(carsreg, cluster=Cars$dealer))
round(clstats["log(mileage)",], 5)

SampMean <- function(data, obs) mean(data[obs])
PriceByDealer <- split(Cars$price, Cars$dealer)
BlockSampMean <- function(data, ids) mean(unlist(data[ids]))
boot(Cars$price, SampMean, 1000)
boot(PriceByDealer, BlockSampMean, 1000)

### confidence interval
betaErrors <- betaBoot$t - bhat
quantile(bhat - betaErrors,c(.025,.975))
quantile(betaBoot$t,c(.025,.975))

## bias example: e^b
ebhat <- exp(bhat)
ebhatBoot <- exp(betaBoot$t)
ebhatErrors <- ebhatBoot - ebhat
mean(ebhatErrors)
quantile(ebhat - ebhatErrors,c(.025,.975))
quantile(ebhatBoot,c(.025,.975))


### parametric bootstrap
CarsRegFun <- function(data){
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data)
    return(fit$coef["log(mileage)"])
}
CarsDataGen <- function(data, fit){
    n <- nrow(data)
    Ey <- predict(fit, data)
    data$price <- exp( rnorm(n, Ey, summary(fit)$dispersion)  )
    return(data)
}
CarsDataGenNCV <- function(data, mle){
    Ey <- predict(mle, data)
    data$price <- exp( rnorm(nrow(data), Ey, abs(mle$residuals) ) )
    return(data)
}

boot(Cars, CarsRegFun, 1000, sim = "parametric", 
     ran.gen = CarsDataGen, mle=carsreg,
     parallel="snow", ncpus=8)

boot(Cars, CarsRegFun, 1000, sim = "parametric", 
     ran.gen = CarsDataGenNCV, mle=carsreg,
     parallel="snow", ncpus=8)

MileBoot

#############################


# Import the data file on cars for sale into R
Cars.Listings <- read.csv("./datasets/CA_cars.csv", header=TRUE)

# We'll now take a sample we feel is representative of used cars we want to test
# This sample includes used cars  with prices at or below $100,000, and mileage greater than or equal to 10,000 miles
# but less than 150,000 miles.
Cars.Listings <- subset(Cars.Listings,
                                Cars.Listings$type != "New" &
                                Cars.Listings$mileage >= 10000 &
                                Cars.Listings$mileage <= 150000 &
                                Cars.Listings$price <= 100000 )

summary(Cars.Listings)


# This subsample examines Mercedes-Benz from our original dataset
Cars.Listings.Make <- subset(Cars.Listings, make == "Mercedes-Benz")

# This line removes unwanted factor levels
#Cars.Listings.Volkswagen$Model <- factor(Cars.Listings.Volkswagen$Model)

## Bayesian estimation of generalized linear models
# You can install with install.packages("arm")
library(arm)

# Bayesian GLM
bayes.linereg.make <- bayesglm(log(price) ~ log(mileage) + year + certified + rating + body, data=Cars.Listings.Make)
summary(bayes.linereg.make)

# Compare 95 percent confidence interval for the GLM example
# Standard linear model
carsubtest <- glm(log(price) ~ log(mileage) + year + certified + rating + body, data=Cars.Listings.Make)
summary(carsubtest)
confint(carsubtest)

# Simulate posterior distribution for our Bayesian example
post.sims <- coef(sim(bayes.linereg.make))
posterior.Mileage <- post.sims[,2]
posterior.year <- post.sims[,3]

# Display the 95 percent confidence interveral for the Mileage example
quantile(posterior.Mileage, c(0.025, 0.975))

# Display a histogram of the posterior distribution for Mileage
hist(posterior.Mileage,
     freq=FALSE, 
     main="", 
     col=8,
     border="grey90",
     xlab="posterior.Mileage")
lines(density(posterior.Mileage), col="royalblue", lwd=1.5)
dev.copy(png,'posteriormileage.png') # Figure 3.11
dev.off() 
