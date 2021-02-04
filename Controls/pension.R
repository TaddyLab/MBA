library(hdm)
data(pension)
pension[1,]
y = pension$tw
d = pension$p401
x = pension[, c("i1","i2", "i3", "i4", "i5", "i6", "i7","inc",
				"a1", "a2", "a3", "a4", "a5","fsize",
	"hs", "smcol", "col", "marr", "twoearn","db","pira","hown")]
x[,"inc"] <- x[,"inc"]/1e4

ols <- glm(y ~ d + .-1, data=x[,1:7])
summary(ols)

library(gamlr)
xbig <- sparse.model.matrix( ~ .^3 -1, data=x)
dim(xbig)

#ols on full data
system.time( olsbig <- gamlr(cbind(d,xbig), y, lambda.start=0, maxit=1e6) )
coef(olsbig)[2,,drop=FALSE]

# orthogonal ML
# v1
dfit <- gamlr(xbig, d, family="binomial")
dhat <- predict(dfit, newdata=xbig, type="response")

png('pensionTreatLasso.png', width=3, height=4, units="in", res=720)
plot(dfit)
dev.off()

png('pensionBoxplot.png', width=3, height=4, units="in", res=720)
boxplot(dhat ~ d, col="purple", bty="n")
dev.off()

dres <- drop(d-dhat)
ctrlfit <- gamlr(cbind(dres=dres,xbig), y, lmr=1e-3)
coef(ctrlfit)[2,,drop=FALSE]
plot(ctrlfit)

## with dres unpenalized
ctrlfitFree <- gamlr(cbind(dres=dres,xbig), y, lmr=1e-3, free=1)
coef(ctrlfitFree)[2,,drop=FALSE]
plot(ctrlfit)

#naive lasso
naivefit <- gamlr(cbind(d,xbig), y, lmr=1e-3)
coef(naivefit)[2,,drop=FALSE]
plot(naivefit)

# v2
library(sandwich)
library(lmtest)

yfit <- gamlr(xbig, y)
yhat <- predict(yfit, newdata=xbig)
ytil <- drop(y-yhat)
oml <- glm(ytil ~ dres-1)
coeftest(oml, vcov=vcovHC(oml))

# same thing, but with cross fitting
oml2 <- orthoML(xbig, d, y, nfold=8, lmr=1e-4)
coeftest(oml2, vcov=vcovHC(oml2))

# alternatively, an IV analysis
library(AER)
z <- pension$e401
aerIV <- ivreg( y  ~ d + . | z + ., data=x)
summary(aerIV)

## small samples
set.seed(5807)
ss <- sample.int(nrow(pension),1000)
coef(naivefit <- gamlr(cbind(d,xbig)[ss,], y[ss], lmr=1e-3))[2]
plot(naivefit)

# doesn't converge
# coef(gamlr(cbind(d,xbig)[ss,], y[ss], lambda.start=0))[2]

dfitss <- gamlr(xbig[ss,], d[ss], family="binomial")
plot(dfitss)
dhatss <- predict(dfitss, newdata=xbig, type="response")
fitss <- gamlr(cbind(d-dhatss,xbig)[ss,], y[ss], lmr=1e-3)
plot(fitss)
coef(fitss)[2,,drop=FALSE]

summary(orthoML(xbig[ss,], d[ss], y[ss], nfold=10,lmr=1e-5))

# heterogeneity in home prices
oml3 <- orthoML(xbig, d=cbind(d=d, "d:hown"=d*x[,"hown"]), y, nfold=5)
coeftest(oml3, vcov=vcovHC(oml3))
