## analysis of monthly return 
# (these are monthly returns minus the t-bill risk free rate of return)
R <- read.csv("returns.csv", row.names=1)

## pull out the S&P 500 and Amazon
head(R[,1:4],3)
sp500 <- R[,"SP500"]
amzn <- R[,"AMZN"]
R <- R[,-which(colnames(R)%in%c("SP500","AMZN"))]

## look at big tech
head(R[,c("GOOG","AAPL","MSFT","FB")],3)
R[28:30,"FB",drop=FALSE]

## use a regularized EM algorithm from josse2016missmda
library(missMDA)
Ri <- imputePCA(R,npc=4)$completeObs
head(Ri[,c("GOOG","AAPL","MSFT","FB")], 3)

# use prcomp to fit the principal componets
retpc <- prcomp(Ri, scale=TRUE)  
w <- predict(retpc)
phi <- retpc$rotation

## plot big stock scores
bigs <- read.csv("bigstocks.csv", header=FALSE)
bigs <- bigs[-5,]
#png('returnsPCA.png', width=10, height=5, units="in", res=720)
par(mfrow=c(1,2))
plot(phi[bigs[,1],1:2], type="n", bty="n")
text(phi[bigs[,1],1:2], labels=bigs[,1], cex=bigs[,2]/350, col="navy") 
plot(phi[bigs[,1],3:4], type="n", bty="n")
text(phi[bigs[,1],3:4], labels=bigs[,1], cex=bigs[,2]/350, col="navy") 
#dev.off()

png('returnsZvSNP.png', width=4.5, height=4.5, units="in", res=720)
plot(w[,1],sp500, bty="n", pch=20, xlab="PC1 monthly score", ylab="S&P500 montly return")
dev.off()

## principal components regression
fit <- glm(amzn ~ PC1 + PC2 + PC3, data=as.data.frame(w))
summary(fit)

# onto the lasso
library(gamlr)
set.seed(10)
alasso <- cv.gamlr(w, amzn, nfold=10)
B <- coef(alasso)[-1,]
B[B!=0]

# both raw stocks and the PCs
blasso <- cv.gamlr(cbind(w,Ri), amzn, foldid=alasso$foldid)
B <- coef(blasso)[-1,]
B[B!=0]

png('returnsPCReg.png', width=10, height=5, units="in", res=720)
par(mfrow=c(1,2))
plot(alasso, ylim=c(0.004,0.01), bty="n")
mtext("PC Inputs",line=2, font=2, cex=1.1) 
plot(blasso, ylim=c(0.004,0.01), bty="n")
mtext("PC+Stocks Inputs",line=2, font=2, cex=1.1) 
dev.off()

### marginal regression
phi <- cor(Ri, amzn)/apply(Ri,2,sd) 
v <- Ri%*%phi
fwd <- glm(amzn ~ v)

png('returnsMRG1.png', width=4.5, height=4.5, units="in", res=720)
plot(v, w[,1], bty="n", pch=20, xlab="MR factor", ylab="PC1")
dev.off()
png('returnsMRG2.png', width=4.5, height=4.5, units="in", res=720)
plot(fwd$fitted, amzn,  pch=20, bty="n", xlab="MR fitted values")
dev.off()


#### partial least squares
library(textir)

## full pls
retpls <- pls(x=scale(Ri), y=amzn,  K=3)
png('returnsPLS.png', width=12, height=5, units="in", res=720)
par(mfrow=c(1,3), mai=c(.7,.7,.1,.1))
plot(retpls, bty="n", cex.lab=1.4, pch=21, bg="yellow")
dev.off()

## look at the loadings
phi <- retpls$loadings*apply(scaleRi,2,sd)
tail(phi[order(abs(phi[,1])),1])
tail(phi[order(abs(phi[,2])),2])
tail(phi[order(abs(phi[,3])),3])

## CV experiment
MSE <- matrix(nrow=10,ncol=5)
for(i in 1:5){
  train <- which(alasso$foldid!=i)
  test <- which(alasso$foldid==i)
  for(k in 1:10){
  	plsi <- pls(x=Ri[train,], y=amzn[train], K=k)
  	MSE[k,i] <- mean( (amzn[test] - predict(plsi, Ri[test,]))^2 )
  }
  cat(i)
} 
MSE <- as.data.frame(MSE)
names(MSE) <- paste("K=",1:5,sep="")
png('returnsPLSOOS.png', width=8, height=5, units="in", res=720)
boxplot(MSE, col="yellow", ylab="mean square error")
dev.off()

