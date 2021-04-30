# ames <- read.csv("AmesHousing.csv", strings=T)
# ames <- ames[,sapply(ames, function(c) sum(is.na(c)))==0]
# ames <- ames[,-c(1,3,4,5,6,8,10,11,13,17:27,30:32,38,40:50,52,53)]
# ames$Lot.Area <- log(ames$Lot.Area)
# names(ames)[1] <- "log.Lot.Area"
# ames <- ames[ames$Yr.Sold>=2009,]
# ames$Yr.Sold <- NULL
# write.csv(ames, "Ames2009.csv", row.names=FALSE)

ames <- read.csv("Ames2009.csv", strings=T)
xbar <- mean(ames$SalePrice)
xsd <- sd(ames$SalePrice)
muse <- xsd/sqrt(nrow(ames))

xbar + c(-1,1)*1.96*muse

amesfit <- glm(log(SalePrice) ~ .-Neighborhood, data=ames)

pvals <- summary(amesfit)$coef[-1,"Pr(>|t|)"]
length(pvals)

hist(pvals, col=8, breaks=10, xlab="p-values", main="", freq=FALSE)
plot(sort(pvals), xlab="rank", ylab="p-values")

# Function to get significance cut-off alpha from FDR q
fdr_cut <- function(pvals, q){
        pvals <- pvals[!is.na(pvals)]
        N <- length(pvals)
        k <- rank(pvals, ties.method="min")
        max(pvals[ pvals<= (q*k/N) ])
}
# @ 1/10% FDR
cutoff5 <- fdr_cut(pvals,q=.05)
print(cutoff5)
print(sum(pvals<=cutoff5))
which(pvals<=cutoff5)


( bstats <- summary(amesfit)$coef["Central.AirY",] )
bstats["Estimate"] + c(-1,1)*1.96*bstats["Std. Error"]

# function 
getBeta <- function(data, obs, var){
    fit <- glm(log(SalePrice) ~ .-Neighborhood, data=data[obs,])
    return(fit$coef[var])
}

library(parallel)
library(boot)
set.seed(1)
( betaBoot <- boot(ames, getBeta, 2000, var="Central.AirY", 
                 parallel="snow", ncpus=detectCores()) )

quantile(betaBoot$t, c(.025, .975))

### clustered standard errors. 
byNBHD <- split(ames, ames$Neighborhood)
length(byNBHD)
set.seed(1)
getBetaBlock <- function(data, ids, var){
    data <- do.call("rbind",data[ids])
    fit <- glm(log(SalePrice) ~ .-Neighborhood, data=data)
    return(fit$coef[var])
}
( betaBootB <- boot(byNBHD, getBetaBlock, 2000, var="Central.AirY",
                          parallel="snow", ncpus=detectCores()) )

library(sandwich)
library(lmtest)
Vblock <- vcovCL(amesfit, cluster=ames$Neighborhood)
clstats <- coeftest(amesfit, vcov = Vblock)
round(clstats["Central.AirY",], 5)

quantile(exp(betaBootB$t), c(.025, .975))
quantile(2*exp(betaBootB$t0) - exp(betaBootB$t), c(.025, .975))