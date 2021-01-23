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

dfit <- gamlr(xbig, d, family="binomial")
yfit <- gamlr(xbig, y)

dhat <- predict(dfit, newdata=xbig, type="response")
yhat <- predict(yfit, newdata=xbig)

dtil <- drop(d-dhat)
ytil <- drop(y-yhat)

oml <- glm(ytil ~ dtil-1)

library(sandwich)
library(lmtest)
coeftest(oml, vcov=vcovHC(oml))

oml2 <- orthoML(xbig, d, y, nfold=5)
coeftest(oml2, vcov=vcovHC(oml2))

oml3 <- orthoML(xbig, d=cbind(d=d, "d:hown"=d*x[,"hown"]), y, nfold=5)
coeftest(oml3, vcov=vcovHC(oml3))

library(AER)
z <- pension$e401
aerIV <- ivreg( y  ~ d + . | z + ., data=x)
summary(aerIV)

