
# Import the data file on cars for sale into R
Cars <- read.csv("SoCalCars.csv", stringsAsFactors = TRUE)

# We'll now take a sample we feel is representative of used cars we want to test
# This sample includes used cars  with prices at or below $100,000, and mileage greater than or equal to 10,000 miles
# but less than 150,000 miles.
Cars <- subset(Cars,
        Cars$type != "New" & Cars$mileage >= 10000 & 
        Cars$mileage <= 150000 & Cars$price <= 100000 )[,-1]
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
19674.96 + 1.96*297.298*c(-1,1) 
# ~95% confidence interval for CPO premium 
14330.5 + c(-2,2)*1363.6

# larger car regression model.  First we relevel the reference levels
Cars$make <- relevel(Cars$make, "Ford")
Cars$body <- relevel(Cars$body, "Sedan")

carsreg <- glm(log(price) ~ log(mileage) + make + 
       year + certified + body + city, data=Cars)
summary(carsreg)

# Extract p-values for each regressor from the regression ouput
pvals <- summary(carsreg)$coef[-1,"Pr(>|t|)"]
length(pvals)
nullps <- runif(115)

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
pdf('SoCalCarsFDR.pdf', width=8, height=4)
par(mfrow=c(1,2))
plot(sort(pvals), pch=21, cex=.5,
    bty="n", xlab="rank", ylab="p-values")
points(sort(nullps), pch=24, cex=.5)
legend("topleft", legend=c("null","observed"), pch=c(24,21), bty="n")
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

Cars[c(1001),c("make","model","year","mileage")]
predict(carsreg, Cars[c(1001),], se.fit=TRUE)
9.67232 + c(-2,2)*0.04083165
exp(9.67232 + c(-2,2)*0.04083165)
dodge <- predict(carsreg, Cars[c(1001),], se.fit=TRUE)
predvar <- dodge$se.fit^2 + dodge$residual.scale^2
sqrt(predvar)
9.67232 + c(-2,2)*sqrt(predvar)
exp(9.67232 + c(-2,2)*sqrt(predvar))

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


### 
MileStats <- summary(carsreg)$coef["log(mileage)",]
CarsCoef <- function(data, obs, name){
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data[obs,])
    return(fit$coef[name])
}
CarsCoef(Cars, 1:nrow(Cars), name="log(mileage)")
round(MileStats,3)

#########
library(parallel)
detectCores()
system.time( boot(Cars, CarsCoef, name="log(mileage)", 1000) )
system.time( boot(Cars, CarsCoef, name="log(mileage)", 1000, 
                  parallel="multicore", ncpus=8) )
system.time( boot(Cars, CarsCoef, name="log(mileage)", 1000, 
                  parallel="snow", ncpus=8) )
############

## test statistics
library(boot)
MileBoot <- boot(Cars, CarsCoef, name="log(mileage)", 2000, parallel="snow", ncpus=8)
MileBoot
(bhat <- MileStats["Estimate"])
mean(MileageBoot$t) - bhat
bhat/sd(MileageBoot$t)
2*pnorm(-abs(bhat/sd(MileageBoot$t)))

## confidence interval
MileErrors <- MileBoot$t - bhat
quantile(bhat - MileErrors,c(.025,.975))
quantile(MileBoot$t,c(.025,.975))

## bias example: e^b
ebhat <- exp(bhat)
ebhatBoot <- exp(MileBoot$t)
ebhatErrors <- ebhatBoot - ebhat
mean(ebhatErrors)
quantile(ebhat - ebhatErrors,c(.025,.975))
quantile(ebhatBoot,c(.025,.975))

hist(MileBoot$t, freq=FALSE, ylim=c(0,25), border="grey90",
     main="", xlab="price-mileage elasticity")

bhatse <- MileStats["Std. Error"]
grid <- seq(bhat-6*bhatse,bhat+6*bhatse,length=100)
lines(grid, dnorm(grid, bhat, bhatse), col=4, lwd=1.5)

## HC standard errors 
#boxplot(carsreg$residuals ~ Cars$dealer)
library(sandwich)
library(lmtest)
hcstats <- coeftest(carsreg, vcov = vcovHC(carsreg, "HC0"))
round(hcstats["log(mileage)",], 5)
lines(grid, dnorm(grid, bhat, hcstats["log(mileage)","Std. Error"]), 
      col=2, lwd=1.5)

## clustered standard errors. 
CarsByDealer <- split(Cars, Cars$dealer)
CarsBlockCoef <- function(data, ids){
    data <- do.call("rbind",data[ids])
    fit <- glm(log(price) ~ log(mileage) + make + 
                   year + certified + body + city, data=data)
    return(fit$coef["log(mileage)"])
}
CarsBlockCoef(CarsByDealer, 1:length(CarsByDealer))
MileBlockBoot <- boot(CarsByDealer, CarsBlockCoef, 2000, parallel="snow", ncpus=8)
MileBlockBoot
MileBoot

clstats <- coeftest(carsreg, vcov = vcovCL(carsreg, cluster=Cars$dealer))
round(clstats["log(mileage)",], 5)

SampMean <- function(data, obs) mean(data[obs])
PriceByDealer <- split(Cars$price, Cars$dealer)
BlockSampMean <- function(data, ids) mean(unlist(data[ids]))
boot(Cars$price, SampMean, 1000)
boot(PriceByDealer, BlockSampMean, 1000)


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
