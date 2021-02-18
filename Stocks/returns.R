## analysis of monthly return 
# (these are monthly returns minus the t-bill risk free rate of return)
R <- read.csv("returns.csv", row.names=1)

## pull out the S&P 500 and Amazon
head(R[,1:4],3)
sp500 <- R[,"SP500"]
ind <- which(colnames(R)=="CVX")
y <- R[,ind]
R <- R[,-c(1,ind)]

## look at big tech
round(R[1:3,c("AMZN","GOOG","AAPL","MSFT","FB")],4)
R[27:30,"FB",drop=FALSE]

## use a regularized EM algorithm from josse2016missmda
library(missMDA)
Ri <- imputePCA(R,npc=4)$completeObs
round(Ri[1:3,c("AMZN","GOOG","AAPL","MSFT","FB")], 4)

# use prcomp to fit the principal componets
retpc <- prcomp(Ri, scale=TRUE)  
w <- predict(retpc)
phi <- retpc$rotation

## plot big stock scores
bigs <- read.csv("bigstocks.csv", header=FALSE)
bigs <- bigs[-which(bigs[,1]=="CVX"),]
png('returnsPCA.png', width=10, height=5, units="in", res=720)
par(mfrow=c(1,2),xpd=TRUE)
plot(phi[bigs[,1],1:2], type="n", bty="n")
text(phi[bigs[,1],1:2], labels=bigs[,1], cex=bigs[,2]/350, col="navy") 
plot(phi[bigs[,1],3:4], type="n", bty="n")
text(phi[bigs[,1],3:4], labels=bigs[,1], cex=bigs[,2]/350, col="navy") 
dev.off()

png('returnsZvSNP.png', width=4.5, height=4.5, units="in", res=720)
plot(w[,1],sp500, bty="n", pch=20, xlab="PC1 monthly score", ylab="S&P500 montly return")
dev.off()

## principal components regression
fit <- glm(y ~ PC1 + PC2 + PC3 + PC4, data=as.data.frame(w))
summary(fit)

# onto the lasso
library(gamlr)
set.seed(0)
alasso <- cv.gamlr(w, y, nfold=10)
B <- coef(alasso)[-1,]
B[B!=0]

# both raw stocks and the PCs
blasso <- cv.gamlr(cbind(w,Ri), y, foldid=alasso$foldid)
B <- coef(blasso)[-1,]
B[B!=0]

png('returnsPCReg.png', width=10, height=5, units="in", res=720)
par(mfrow=c(1,2))
plot(alasso, ylim=c(0,0.004), bty="n")
mtext("PC Inputs",line=2, font=2, cex=1.1) 
plot(blasso, ylim=c(0,0.004), bty="n")
mtext("PC+Stocks Inputs",line=2, font=2, cex=1.1) 
dev.off()

### marginal regression
phi <- cor(Ri, y)/apply(Ri,2,sd) 
v <- Ri%*%phi
fwd <- glm(y ~ v)

png('returnsMRG1.png', width=4.5, height=4.5, units="in", res=720)
plot(v, w[,1], bty="n", pch=20, xlab="MR factor", ylab="PC1")
dev.off()
png('returnsMRG2.png', width=4.5, height=4.5, units="in", res=720)
plot(fwd$fitted, y,  pch=20, bty="n", xlab="MR fitted values")
dev.off()


#### partial least squares
library(textir)
retpls <- pls(x=Ri, y=y,  K=3)

png('returnsPLS.png', width=10, height=4, units="in", res=720)
par(mfrow=c(1,3), mai=c(.7,.7,.1,.1))
plot(retpls, bty="n", cex.lab=1.4, pch=21, bg="yellow")
dev.off()

## look at the loadings
phi <- retpls$loadings*apply(Ri,2,sd)
tail(phi[order(abs(phi[,1])),1])
tail(phi[order(abs(phi[,2])),2])
tail(phi[order(abs(phi[,3])),3])

## CV experiment
MSE <- matrix(nrow=10,ncol=5)
for(i in 1:10){
  train <- which(alasso$foldid!=i)
  test <- which(alasso$foldid==i)
  for(k in 1:ncol(MSE)){
  	plsi <- pls(x=Ri[train,], y=y[train], K=k)
  	MSE[i,k] <- mean( (y[test] - predict(plsi, Ri[test,]))^2 )
  }
  cat(i)
} 
MSE <- as.data.frame(MSE)
names(MSE) <- paste(1:ncol(MSE))
png('returnsPLSOOS.png', width=4.5, height=4.5, units="in", res=720)
boxplot(MSE, col="yellow", ylab="mean square error", xlab="K", ylim=c(0,0.004))
abline(h=min(alasso$cvm), lty=2, col=2, lwd=1.5)
abline(h=min(blasso$cvm), lty=2, col=4, lwd=1.5)
dev.off()

png('returnsPLS2D.png', width=4.5, height=4.5, units="in", res=720)
par(xpd=TRUE)
plot(retpls$loadings[bigs[,1],1:2], type="n", bty="n", xlab="PLS(1)", ylab="PLS(2)")
text(retpls$loadings[bigs[,1],1:2], labels=bigs[,1], cex=bigs[,2]/350, col="navy") 
dev.off()
