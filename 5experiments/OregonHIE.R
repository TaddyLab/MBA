######################################
#### Causal Inference - Experiments
######################################

################## OHIE
# Get the data from nber.org/oregon/4.data.html

# person_id  is key
# treatment is in Description file, and is random conditional on the numhh (number of names in lottery)
# in 2008 new spots opened for medicaid, which was previously closed to new enroll
# we are interested in health insurance effect on increased utilization (hence costs)
# (impact on health is a longer term outcome of interest)

ohie <- read.csv("OHIEresults.csv")
dim(ohie)
head(ohie,3)
table(ohie[,"selected"])

ybar <- tapply(ohie[,"doc_num"], ohie[,"selected"], mean)
( ATE = ybar['1'] - ybar['0'] )

fitATE <- glm(doc_num ~ selected, data=ohie)
coef(fitATE)["selected"]
summary(fitATE)

library(parallel)
library(boot)
getATE <- function(data, ind)	
	coef(glm(doc_num ~ selected, data=data[ind,]))["selected"]
( bootATE <- boot(ohie, getATE, 1000, 
				  parallel="snow", ncpus=detectCores() ) )

## function for de-biased bootstrap confidence intervals
getCI <- function(bo, p=c(.025,.975)) quantile(2*bo$t0 - bo$t, p)
getCI(bootATE)

# weighting to adjust sample to represent population
nSelW <- tapply(ohie$weight, ohie$selected, sum)
yBarW <- tapply(ohie$weight*ohie$doc_num, ohie$selected, sum)/nSelW
(ATEweighted <-  yBarW['1'] - yBarW['0'])
glm(doc_num ~ selected, weights=weight, data=ohie)

# covariate imbalance - larger households higher chance to be selected
table(ohie[,c("selected","numhh")])

# linear fit to account for household size
fitAdj <- glm(doc_num ~ selected*numhh, data=ohie)
coef(fitAdj)
mean( 
	predict(fitAdj, newdata=data.frame(selected=1, numhh=ohie$numhh)) - 
	predict(fitAdj, newdata=data.frame(selected=0, numhh=ohie$numhh)) )


## look at conditional dependence within households.
library(data.table) # way faster!
ohieHH <- split(ohie, ohie$household)
system.time(rbindlist(ohieHH))
system.time(do.call("rbind",ohieHH))

getAdjATE <- function(data, ids, binder){
    data <- binder(data[ids])
    fit <- glm(doc_num ~ selected*numhh, data=data)
	mean( 
	   predict(fit, newdata=data.frame(selected=1, numhh=data$numhh)) - 
	   predict(fit, newdata=data.frame(selected=0, numhh=data$numhh)) )
}
(bootAdjATE <- boot(ohieHH, getAdjATE, binder=rbindlist,
					1000, parallel="snow", ncpus=detectCores()))
getCI(bootAdjATE)

## maybe faster, but only works for linear models:
x <- scale(model.matrix( ~ numhh, data=ohie)[,-1], scale=FALSE)
colMeans(x)
coef(fitx <- glm(doc_num ~ selected*x, data=ohie))["selected"]

library(sandwich)
library(lmtest)
coeftest(fitx, vcov = vcovCL(fitx, ohie$household) )

( bootAdjATE )


## same ideas but logit
getAdjATEbin <- function(data, ids, binder){
    data <- binder(data[ids])
    fit <- glm( doc_num > 0 ~ selected*numhh, data=data, family="binomial")
	mean( 
	   predict(fit, data.frame(selected=1, numhh=data$numhh), type="response")- 
	   predict(fit, data.frame(selected=0, numhh=data$numhh), type="response") )
}
(bootAdjATEbin <- boot(ohieHH, getAdjATEbin, binder=rbindlist,
					1000, parallel="snow", ncpus=detectCores()))
getCI(bootAdjATEbin)

#### heterogeneous treatment effects

library(gamlr)
rawsurvey <- read.csv("OHIEsurvey.csv")
survey <- naref(rawsurvey, impute=TRUE)
dim(survey)

Xohie <- sparse.model.matrix(~ selected*., 
	data=cbind(ohie[,c("selected","numhh")], survey[,-1]))[,-1]
dim(Xohie)

free <- c("selected","numhh2","numhh3+",
		   "selected:numhh2","selected:numhh3+")
htefit <- gamlr(x=Xohie, y=ohie$doc_num, free=free)
gam <- drop(coef(htefit))
gam <- gam[grep("selected", names(gam))]
gam <- gam[gam!=0]
length(gam)

gam[grep("race", names(gam))]


Xcntrl <- sparse.model.matrix(~ selected*., 
	data=cbind(selected=0, numhh=ohie$numhh, survey[,-1]))[,-1]
Xtreat <- sparse.model.matrix(~ selected*., 
	data=cbind(selected=1, numhh=ohie$numhh, survey[,-1]))[,-1]
survey[1,]

predict(htefit, Xtreat[1,,drop=FALSE]) - 
	predict(htefit, Xcntrl[1,,drop=FALSE])


#### OHIE 2SLS
stage1 <- glm( medicaid ~ selected + numhh, data=ohie)
pHat <- predict(stage1, newdata=ohie)
stage2 <- glm( doc_num ~ pHat + numhh, data=ohie)
coef(stage2)["pHat"]

getIV <- function(data, ids, binder){
	data <- binder(data[ids])
	stage1 <- glm( medicaid ~ selected + numhh, data=data)
	pHat <- predict(stage1, newdata=data)
	stage2 <- glm( doc_num ~ pHat + numhh, data=data)
	coef(stage2)["pHat"]
}
( bootIV <- boot(ohieHH, getIV, 1000, binder=rbindlist,
				 parallel="snow", ncpus=detectCores() ) )
getCI(bootIV)

# IV and SEs using the AER package
library(AER)
aerIV <- ivreg( doc_num  ~ medicaid + numhh | selected + numhh, data=ohie)
summary(aerIV)

library(lmtest)
VCL = vcovCL(aerIV, ohie$household) 
coeftest(aerIV, vcov = VCL)



