######################################
#### Causal Inference - Controls
######################################

####### #OJ

oj <- read.csv("OJ.csv")
head(oj, 3)

baseFit <- glm(log(sales) ~ log(price), data=oj)
coef(baseFit)

brandFit <- glm(log(sales) ~ brand + log(price), data=oj)
coef(brandFit)

## calculating treatment effects using residuals
pricereg <- glm(log(price) ~ brand, data=oj)
dhat <- predict(pricereg, newdata=oj)
dtil <- log(oj$price)-dhat
coef( glm( log(sales) ~ dtil, data=oj) )

## double residualization
salesreg <- glm(log(sales) ~ brand, data=oj)
yhat <- predict(salesreg, newdata=oj)
ytil <- log(oj$sales) - yhat

coef( glm( ytil ~ dtil -1 ) )
coef( glm( ytil ~ dtil) )  #intercept is essentially zero


