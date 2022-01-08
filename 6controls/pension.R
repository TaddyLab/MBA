######################################
#### Causal Inference - Controls
######################################

####### # pension

library(hdm)
data(pension)
pension[1,]
y = pension$tw
d = pension$p401
x = pension[, c("i1","i2", "i3", "i4", "i5", "i6", "i7","inc",
				"a1", "a2", "a3", "a4", "a5","fsize",
	"hs", "smcol", "col", "marr", "twoearn","db","pira","hown")]
x[,"inc"] <- x[,"inc"]/1e4

## OLS
ols <- glm(y ~ d + .-1, data=x[,1:7])
summary(ols)

## lasso
library(gamlr)
xbig <- sparse.model.matrix( ~ .^3 -1, data=x)
dim(xbig)

# ols on full data, takes ~20 min!! commented out, run only if you are ready to chill
# system.time( olsbig <- gamlr(cbind(d,xbig), y, lambda.start=0, maxit=1e6) )
# coef(olsbig)[2,,drop=FALSE]

# double residualization
dfit <- gamlr(xbig, d, family="binomial")
dhat <- predict(dfit, newdata=xbig, type="response")[,1]
dtil <- d-dhat

yfit <- gamlr(xbig, d)
yhat <- predict(yfit, newdata=xbig)[,1]
ytil <- y-yhat

glm(ytil ~ dtil -1)

#png('pensionTreatLasso.png', width=4, height=5, units="in", res=720)
plot(dfit)
#dev.off()

#png('pensionBoxplot.png', width=4, height=5, units="in", res=720)
boxplot(dhat ~ d, col="purple", bty="n")
#dev.off()

#png('pensionResponseLasso.png', width=4, height=5, units="in", res=720)
plot(yfit)
#dev.off()

#png('pensionYscatter.png', width=4, height=5, units="in", res=720)
plot(yhat ~ y, col=rgb(1,.5,0,.25), bty="n", pch=20)
#dev.off()

#png('pensionResids.png', width=4, height=5, units="in", res=720)
plot(ytil ~ dtil, col=rgb(1,0,1,.25), bty="n", pch=20)
#dev.off()

# orthogonal ML
set.seed(1)
dml <- doubleML(xbig, d, y, family="binomial", nfold=5)
summary(dml)

library(sandwich)
library(lmtest)
coeftest(dml, vcov=vcovHC(dml, "HC0"))

9808 + c(-2,2)*2316

#naive lasso
naivefit <- gamlr(cbind(d,xbig), y, lmr=1e-3, free=1)
coef(naivefit)[2,,drop=FALSE]
plot(naivefit)

# two treatment effects
d2 <- model.matrix( ~ hown*p401-hown-1, data=pension)
d2[c(1,6234),]
# use parallel library
library(parallel)
cl <- makeCluster(detectCores())
set.seed(1)
dml2 <- doubleML(xbig, d2, y, family="binomial", nfold=5, cl=cl, lmr=1e-3)
coeftest(dml2, vcov=vcovHC(dml2, "HC0"))

# same thing, but interacting with single treatment residuals
dtil <- dml$x
ytil <- dml$y
dml3 <- glm( ytil ~ dtil*hown - hown - 1, data=pension)
coeftest(dml3, vcov=vcovHC(dml3, "HC0"))

# alternatively, an IV analysis (note this is what is in the original academic paper)
library(AER)
z <- pension$e401
aerIV <- ivreg( y  ~ d + . | z + ., data=x)
summary(aerIV)
