library(hdm)
# Current Population Survey in 2012
# similar data used in mulligan+rubinstein (QJE: 2008)
data(cps2012)
# remove the redundant 'year' and trailing columns
# the factor reference levels (ommitted) are male married NE with some college
cps <- cps2012[,-c(1,17:23)]
## exp1 is 'potential experience', age - years of education - 7
## rename it as pexp to make clear
names(cps)[15] <- "pexp"
# response of interest is nominal wage, or earnings/annual.hours
# we will transform this from log nominal wage (lnw) and rename
cps$lnw <- exp(cps$lnw)
names(cps)[1] <- "hrwage"
head(cps,3)

library(tree)
pexptree <- tree(hrwage ~ pexp + female, data=cps, mindev=1e-3)
pexptree
#png('pexpTree.png', width=4, height=5, units="in", res=720)
plot(pexptree, col="grey50")
text(pexptree)
#dev.off()

## produce the plot of wage surfaces
grid <- 0:44
fpred <- predict(pexptree, data.frame(female=1, pexp=grid))
mpred <- predict(pexptree, data.frame(female=0, pexp=grid))
#png('pexpPred.png', width=4, height=5, units="in", res=720)
plot(grid, mpred, lwd=2, type="l", bty="n", 
	xlim=c(0,46), ylim=range(c(fpred,mpred))+c(-1,1),
	ylab="expected hourly wage", xlab="potential experience")
lines(grid, fpred, col=2, lwd=2)
legend("topright", fill=1:2, border=8, legend=c("male","female"), bty="n")
#dev.off()

## binary response
probtree <- tree(factor(hrwage>15) ~ pexp + female, data=cps)
probtree



wagetree <- tree(hrwage ~ ., data=cps, mindev=1e-3)
wagetree
#png('wageTree.png', width=10, height=5, units="in", res=720)
plot(wagetree, col="gray50")
text(wagetree)
#dev.off()

## pruning and CV
cvwt <- cv.tree(wagetree, K=10)

#png('wageCVdev.png', width=4, height=5, units="in", res=720)
plot(cvwt$size, cvwt$dev, xlab="size", ylab="oos deviance", pch=21, bg="lightblue", bty="n")
#dev.off()

wagetreecut <- prune.tree(wagetree, best=5)
#png('wageTreeCut.png', width=4, height=5, units="in", res=720)
plot(wagetreecut, col="grey50")
text(wagetreecut)
#dev.off()

## random forest and variable importance
library(ranger)
wagerf <- ranger(hrwage ~., data=cps, write.forest=TRUE, 
				num.tree=100, min.node.size=1000, importance="impurity")
sort(wagerf$variable.importance, decreasing=TRUE)

## prediction
# data for married from NE with 'some college', both male and female
cps[3,]
newdat <- rbind(cps[3,-1],cps[3,-1])
newdat[2,"female"] <- 1
# predict and get quantiles
wagehat <- predict(wagerf, cps[1:2,], predict.all=TRUE)
dim(wagehat$predictions)
apply(wagehat$predictions,1,quantile, 
	  probs=c(0.025, 0.5, 0.975))

## double ML

# residualization
d <- cps$female
drf <- ranger(d ~., data=cps[,-(1:2)], 
			num.tree=400, min.node.size=1000)
dhat <- predict(drf, cps[,-1])
dtil <- cps$female-dhat$predictions
y <- cps$hrwage
yrf <- ranger(y ~., data=cps[,-(1:2)], 
			num.tree=400, min.node.size=1000)
yhat <- predict(yrf, cps[,-1])
ytil <- cps$hrwage-yhat$predictions

# ATE
glm(ytil ~ dtil-1)

# HTE  (not in book, but good exercise)
library(gamlr)
dw <- cbind(dtil=dtil, dtil*cps[,-(1:2)])
htereg <- gamlr(dw, ytil)
gam <- coef(htereg)[-1,]
gam[gam!=0]

