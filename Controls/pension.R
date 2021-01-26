library(hdm)
data(pension)

y = pension$tw
d = pension$p401
x = pension[, c("i1","i2", "i3", "i4", "i5", "i6", "i7", "a1", "a2", "a3", "a4", "a5", "ira","fsize",
"hs", "smcol", "col", "marr", "twoearn", "db", "pira", "hown")]

ols <- glm(y ~ d + ., data=x[,2:7])
summary(ols)

library(gamlr)
xbig <- sparse.model.matrix( ~ .^3 -1, data=x)

#ols on full data
olsbig <- gamlr(cbind(d,xbig), y, lambda.start=0)
coef(olsbig)[2,,drop=FALSE]

#naive lasso
naivefit <- gamlr(cbind(d,xbig), y)
coef(naivefit)[2,,drop=FALSE]

# orthogonal ML
# v1
dfit <- gamlr(xbig, d, family="binomial")
dhat <- predict(dfit, newdata=xbig, type="response")
dtil <- drop(d-dhat)

ctrlfit <- gamlr(cbind(dtil,xbig), y, lmr=1e-3)
coef(ctrlfit)[2,,drop=FALSE]

# v2
library(sandwich)
library(lmtest)

yfit <- gamlr(xbig, y)
yhat <- predict(yfit, newdata=xbig)
ytil <- drop(y-yhat)
oml <- glm(ytil ~ dtil-1)
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
coef(gamlr(cbind(d,xbig)[ss,], y[ss], lmr=1e-5))[2]
# doesn't converge
# coef(gamlr(cbind(d,xbig)[ss,], y[ss], lambda.start=0))[2]

dfitss <- gamlr(xbig[ss,], d[ss], family="binomial")
dhatss <- predict(dfitss, newdata=xbig, type="response")
fitss <- gamlr(cbind(d-dhatss,xbig)[ss,], y[ss])
coef(fitss)[2,,drop=FALSE]

summary(orthoML(xbig[ss,], d[ss], y[ss], nfold=10,lmr=1e-5))

# heterogeneity in home prices
oml3 <- orthoML(xbig, d=cbind(d=d, "d:hown"=d*x[,"hown"]), y, nfold=5)
coeftest(oml3, vcov=vcovHC(oml3))
