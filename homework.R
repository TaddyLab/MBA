
## chapter 3, SoCalCars

### Larger FDR example
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

#### bootstrap analysis of the CPO effect
# recall our earlier test of whether there was a different expected price for
# certified pre-owned vehicles.  We wrote a model that allowed for two different
# price means as $\R{price} = \alpha + \R{certified}\beta + \varepsilon$. We
# then used \R{glm} to fit the model and test for whether $\beta = 0$. To
# re-evaluate this test with bootstrapping, you simply re-fit this model to many
# with-replacement resamples of the \R{Cars} data.  

CertReg <- glm(price ~ certified, data=Cars) 
mle <- coef(CertReg)["certified"]
mlese <- summary(CertReg)$coef["certified","Std. Error"]

# repeat our analysis of the difference in means for CPO cars
Certified <- function(data, obs){
  fit <- glm(price ~ certified, data=data[obs,])
  return(fit$coef["certified"])
}
Certified(Cars, 1:nrow(Cars))
library(boot)
system.time( 
  CertBoot <- boot(Cars, Certified, 10000, parallel="multicore", ncpus=8) 
)
hist(CertBoot$t)
CertBoot 
mean(CertBoot$t)

CertErrors <- CertBoot$t - mle
mle - quantile(CertErrors,c(.975,.025))
quantile(CertBoot$t,c(.025,.975))


### block bootstrap for the panel data example



