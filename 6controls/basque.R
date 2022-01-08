######################################
#### Causal Inference - Controls
######################################

####### # Basque

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

## pull out the treatment and control series
D <- as.matrix(D)
y <- D[1,]
x <- t(D[-1,]) 
tstar <- 1968-1954
y[tstar+c(-1,0,1)]

# fit a regression on the pre-treatment data
library(gamlr)
fit <- gamlr( x[1:tstar,], y[1:tstar], lmr=1e-4)
# coefficients:
w <- drop( coef(fit) )
#png('basqueFit.png', width=4, height=4, units="in", res=720)
plot(fit)
#dev.off()

# predict the untreated counterfactuals
y0hat <- drop( predict(fit, x) )
#png('basquePO.png', width=4, height=4, units="in", res=720)
year <- as.numeric(names(y))
plot(year, y0hat, type="l", ylab="gdp per capita",
	col=rgb(.1,.5,1,0.8), ylim=range(c(y,y0hat)), bty="n", lwd=2)
abline(v=1968, col=8, lty=2)
lines(year, y, col=rgb(1,.5,0,.8), lwd=2)
legend("topleft", bty="n", legend=c("observed basque","synthetic basque"), 
	lwd=2, col=c(col=rgb(1,.5,0,.8),rgb(.1,.5,1,0.8)) )
#dev.off()

# calculate the estimated treatment effects
gamhat <- (y - y0hat)
mean( gamhat[-(1:tstar)] )

# Wrap all of this up in a function
library(gamlr)
synthc <- function(D, tstar, treated, ...){
	y <- D[treated,]
	x <- t(D[-treated,]) 
	fit <- gamlr( x[1:tstar,], y[1:tstar], ...)
	y0hat <- predict(fit, x)[,1]
	gamhat <- y - y0hat
	ate <- mean( gamhat[-(1:tstar)] )
	return(list(y0hat=y0hat, gamhat=gamhat, ate=ate ) )
}

# run the synthetic controls
sc <- synthc(D, tstar=14, treated=1, lmr=1e-4)
sc$ate

# permutation test
library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c("D", "gamlr", "synthc"))
getgam <- function(j){ synthc(D, 14, j, lmr=1e-4)$gamhat }
G <- t(parSapply(cl, 1:nrow(D), getgam))
rownames(G) <- rownames(D)
G[1:4,1:3]

ate <- rowMeans(G[,-(1:tstar)])
mean(abs(ate[-1]) > abs(ate[1]))

# produce the plots
#png('basqueG.png', width=4, height=4, units="in", res=720)
matplot(year, t(G), type="l", lwd=1.5, 
	xlab="year", ylab="synthetic - observed",
 	col=8, lty=1, bty="n")
lines(year, G[1,], lwd=1.5, col="red")
lines(year, G[14,], lwd=1.5, lty=2, col="blue")
legend("topleft", bty="n", 
	legend=c("basque", "placebo", "(madrid)"), 
	lwd=2, lty=c(1,1,2), col=c(2,8,4))
#dev.off()
#png('basqueATE.png', width=4, height=4, units="in", res=720)
hist(ate, col=8, main="", xlab="ate")
abline(v = ate[1], lwd=2, col=2)
#dev.off()

