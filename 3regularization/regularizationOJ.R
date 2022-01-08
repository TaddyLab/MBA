###### REGULARIZATION #######
###### OJ - model matrices and path estimation #######

# call in the oj data from the regression chapter
oj<-read.csv("oj.csv",strings=T)
modMat<-model.matrix(~log(price)+brand,data=oj) # no need to specify response
modMat[c(100,200,300),] #look at one row for each brand

# use naref in gamlr package to create a dummy variable for all levels of categorical variables
library(gamlr)
ojdf <-naref(oj)
ojdf[c(100,200,300),"brand"]

# every factor level will have its own column
# [,-1] gets rid of intercept
modMatAllLevs<-model.matrix(~log(price)+brand,data=ojdf)[,-1]
modMatAllLevs[c(100,200,300),]

# create sparse model matrix without intercept
library(Matrix)
xOJ<-sparse.model.matrix(~log(price)+brand,data=ojdf)[,-1]
xOJ[c(100,200,300),]

class(xOJ)

# as.matrix will change back to dense format
as.matrix(xOJ[c(100,200,300),])
# pulling out a single vector also results in dense format
xOJ[100,]

## Lasso path estimation
# load gamlr package if it isn't already loaded
library(gamlr) # in case you don't already have this loaded
fitOJ <- gamlr(x=xOJ, y=log(ojdf$sales))
plot(fitOJ)

#examine the gamlr object
fitOJ
summary(fitOJ)
names(fitOJ)
dim(fitOJ$beta)
fitOJ$beta[,c(1:2,99:100)]
fitOJ$alpha[c(1:2,99:100)]

# compare to OLS results re-leveling brand to make minute.maid the reference level
oj$brand <- relevel(ojdf$brand, "minute.maid")
glm(log(sales) ~ log(price) + brand, data=oj)

# use free argument to ensure a variable is in the model
fitOJfree <- gamlr(x=xOJ, y=log(ojdf$sales),free="log(price)")
fitOJfree$beta[,c(1:2,99:100)]
