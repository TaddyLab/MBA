
# Import the data file on cars for sale into R
Cars <- read.csv("Cars.csv", stringsAsFactors = TRUE)

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

Cars[c(1,101,1001),c("make","model","year","mileage")]
predict(carsreg, Cars[c(1,101,1001),], se.fit=TRUE)

### bootstrapping
# Set number of samples to be taken
B <- 1000

# Create an empty vector to store the calculated means of the samples
mub <- c()

# Construct a for-loop to calculate the mean of samples of the Cars dataset
for (b in 1:B){
        # Sample indices with replacement from 1 to the number of observations in Cars
        samp_b <- sample.int(nrow(Cars), replace=TRUE)
        # Take the mean of values selected in Listing.Price that match the indices sampled above
        b_mean <- mean(Cars$price[samp_b]) 
        # Add b_mean to the vector that stores the sample mean values 
        mub[b] <- b_mean 
}

# Calculate the mean and standard deviation of the sample of means collected above
mean(mub, na.rm=TRUE)
sd(mub, na.rm=TRUE)
sort(samp_b)[1:10]

# View a histogram of the sample means created through bootstrap
hist(mub, 
     main="", 
     xlab="mean price",
     col=8, 
     border="grey90", 
     freq=FALSE,
     xlim=c(xbar-4*xbse,xbar+4*xbse),
     ylim=c(0, max(samp_pdf_values))
)

# Overlay the theoretical sampling distribution under CLT on the histogram
lines(calc_values, samp_pdf_values, col="royalblue", lwd=1.5)
dev.copy(png,'bootstrapprice.png') # figure 3.3
dev.off()

#############################


# Set the working directory to the specific location of the files on your computer
# note: In this case, the working directory was set for my computer
setwd("/Users/mch/Desktop/Uncertainty/code")

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

# False discovery rates
# Run a simple linear regression using interactions
linereg <- glm(log(price) ~ log(mileage) + year + certified  + rating + body + year*body , data=Cars.Listings)
summary(linereg)

# Extract p-values for each regressor from the regression ouput
pvals <- summary(linereg)$coef[-1,"Pr(>|t|)"]

## Sort the p-values from smallest to greatest
sorted_p <- sort(pvals)

# Function to get significance cut-off alpha from FDR q
fdr_cut <- function(pvals, q){
        pvals <- pvals[!is.na(pvals)]
        N <- length(pvals)
        k <- rank(pvals, ties.method="min")
        alpha <- max(pvals[ pvals<= (q*k/N) ])
        return(alpha)
}

# @ 1/10% FDR
cutoff10 <- fdr_cut(pvals,q=.1)
print(cutoff10)
print(sum(pvals<=cutoff10))
sig <- factor(pvals<=cutoff10)
o <- order(pvals)
N <- length(pvals)

# The below generates figure 3.7
# Plot p-values 
plot(sorted_p,
     col=c("grey60","red")[sig[o]], # Colors the significant p-values red
     pch=20,
     bty="n", 
     xlab="rank", 
     ylab="p-values"
)

## Add the line of significance
# Define q
q <- 0.1
# Add line to plot
abline(a=0, b=q/N)
dev.copy(png,'FDRusedcars.png') # Figure 3.7
dev.off()

#################################################################
# Now for a larger example
# data.table is useful for dealing with large datasets
# You can install with install.packages("data.table")
library(data.table)

# Unpack the zipped dataset listed below in the datasets folder
# fread is faster than read.table
system.time(lipids <-  fread("./datasets/jointGwasMc_LDL.txt"))

# We'll then convert back to the usual R 'data.frame'.
# data.tables have other nice capabilities that we'll see later in class
lipids <- as.data.frame(lipids)

# Pull out p-values and label them
pvals <- as.numeric(lipids[,'P-value'])
names(pvals) <- lipids[,'rsid']

# Plot the p-value distribution; notice the tiny spike near zero
hist(pvals, main='', xlab='p-values', col=8, border="grey90",freq=FALSE)
dev.copy(png,'hist_lipids.png') # Figure 3.8
dev.off()


# Now to plot figure 3.9
# Top 10 locations to investigate
names(pvals)[order(pvals)[1:10]] 

# Function to get significance cut-off alpha from FDR q
fdr_cut <- function(pvals, q){
        pvals <- pvals[!is.na(pvals)]
        N <- length(pvals)
        k <- rank(pvals, ties.method="min")
        alpha <- max(pvals[ pvals<= (q*k/N) ])
        return(alpha)
}

# Find the cut
# @ 10% FDR
cutoff10 <- fdr_cut(pvals,q=.1)
print(cutoff10)
print(sum(pvals<=cutoff10))
# @ 1% FDR
cutoff1 <- fdr_cut(pvals,q=.01)
print(cutoff1)
print(sum(pvals<=cutoff1))
# @ 1/10% FDR
cutoff01 <- fdr_cut(pvals,q=.001)
print(cutoff01)
print(sum(pvals<=cutoff01))
# So you get 4000 discoveries, only 4-5 of which you expect to be false

# Visualize the B+H FDR algorithm 
# warning: the plot can take a bit of time/memory
sig <- factor(pvals<=cutoff01)
o <- order(pvals)
N <- length(pvals)
plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
     ylab="p-values", xlab="tests ordered by p-value", main = 'FDR = 0.1%')
lines(1:N, 0.01*(1:N)/N)
dev.copy(png,'FDR_lipids.png') # Figure 3.9
dev.off()


## This code replicates the examples and figures in 3.5 Bayesian Inference
# Clear the environment
remove(list = ls())

# Set the working directory to the specific location of the files on your computer
# note: In this case, the working directory was set for my computer
setwd("/Users/mch/Desktop/Uncertainty/code")

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
