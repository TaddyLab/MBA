######################################
#### Causal Inference - Experiments
######################################

####### #endogeneity Airline demand pricing simulation

yFun <- function(e,p){
     y = 2 + 10*e-3*p + rnorm(length(e),0,.1)
     y[y<0] <- 0
     return(y) }

e <- rgamma(100,1,1)

z <- rgamma(100,1,1)
pObserved <- e + z
pCounterfactual <- rgamma(100,2,1)

yObserved <- yFun(e, pObserved)
yCounterfactual <- yFun(e, pCounterfactual)

plot(pObserved, yObserved, xlim=c(0,6), ylim=c(0,29), pch=21, bg=8,
     xlab="", ylab="", 
     bty="n", main="observed")
# Add OLS line
abline(lm(yObserved ~ pObserved), col="orange", lwd=2)

# Plot generated data
plot(pCounterfactual, yCounterfactual, xlim=c(0,6), ylim=c(0,29), pch=21, bg=8,
     xlab="", ylab="", 
     bty="n", main="counterfactual")

# Add the OLS line to the plot
abline(lm(yCounterfactual ~ pCounterfactual), col="orange", lwd=2)

#### 2SLS
pReg <- lm(pObserved ~ z)
pHat <- predict(pReg, data.frame(z=z))
lin2SLS <- lm(yObserved ~ pHat)
summary(lin2SLS)

summary(lm(yObserved ~ pObserved)) #compare with OLS
