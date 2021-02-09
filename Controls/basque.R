library(Synth)
library(tidyr)
data(basque)

## synthetic controls analysis
## wrangle the data
D <- basque[,1:4] %>% spread(year, gdpcap)
rownames(D) <- D$regionname
D <- D[c(17,2:16,18), -(1:2)]
D <- D[,1:35]
D[1:5,1:4]
D <- as.matrix(D)

## pull out the treatment and control series
y <- D[1,]
x <- t(D[-1,]) 

# fit a regression on the pre-treatment data
library(gamlr)
fit <- gamlr( x[1:tstar,], y0[1:tstar], lmr=1e-4)
plot(fit)

# predict the post-treatment counterfactuals
y0hat <- drop( predict(fit, x) )

# calculate the estimated treatment effects
gamhat <- y - y0hat
mean( gamhat )

# untreated years are through 1968
library(gamlr)
synthc <- function(D, tstar, ...){
	D <- as.matrix(D)
	y <- D[1,]
	x <- t(D[-1,]) 
	fit <- gamlr( x[1:tstar,], y0[1:tstar], ...)
	plot(fit)
	y0hat <- drop( predict(fit, x) )
	gamhat <- y - y0hat
	ate <- mean( gamhat )
	return(list(w=coef(fit)[,1], y0hat=y0hat, ate=ate ) )
}

# run the synthetic controls
sc <- synthc(D, tstar=1969-1954, lmr=1e-4)
sc$w[ sc$w !=0 ]

# treatment effect
year <- as.numeric(rownames(y))
plot(year, sc$y0hat, type="l", ylab="gdp per capita",
	col=rgb(.1,.5,1,0.8), ylim=range(c(y[1,],sc$y0hat)), bty="n", lwd=2)
abline(v=1968, col=8, lty=2)
lines(year, y[,1], col=rgb(1,.5,0,.8), lwd=2)
legend("topleft", bty="n", legend=c("observed basque","synthetic basque"), 
	lwd=2, col=c(col=rgb(1,.5,0,.8),rgb(.1,.5,1,0.8)) )

# permutation test
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("Y", "gamlr", "synthc"))

gety0 <- function(j){ synthc(Y, j, 1969-1954, lmr=1e-4)$y0hat }
Ysynth <- parSapply(cl, 1:ncol(Y), gety0)

# produce the plots
diff <- Ysynth - Y
matplot(year, diff, type="l", lwd=1.5, 
	xlab="year", ylab="synthetic - observed",
 	col=8, lty=1, bty="n")
lines(year, diff[,1], lwd=1.5, col="red")
lines(year, diff[,14], lwd=1.5, lty=2, col=1)
legend("topleft", bty="n", 
	legend=c("basque", "placebo", "(madrid)"), 
	lty=c(1,1,2), lwd=2, col=c(2,8,1))

## ATE
getATE <- function(j){ synthc(Y, j, 1969-1954, lmr=1e-4)$ate }
ate <- parSapply(cl, 1:ncol(Y), getATE)
hist(ate, col=8, main="")
abline(v = ate[1], lwd=2, col=2)
mean(abs(ate[-1]) > abs(ate[1]))

