######################################
#### Trees and Forests
######################################

################## California census data

## Forests
ca <- read.csv("CalCensus.csv")

## First, lets do it with CART
## no need for interactions; the tree finds them automatically
library(tree)
catree <- tree(log(medianHouseValue) ~ ., data=ca) 

#png('CalTree.png', width=5, height=10, units="in", res=720)
plot(catree, col="grey50")
text(catree)
#dev.off()

## Next, with random forest 
## limit the number of trees and the minimum tree size for speed
## also run on 4 cores if you've got them
## add importance so that we store the variable importance information
library(ranger)
carf <- ranger(log(medianHouseValue) ~ ., data=ca, num.threads=4,
               num.tree=200, importance="impurity")
## variable importance 
sort(carf$variable.importance, decreasing=TRUE)

## calculate resiuals and 
## plot the predictions by location
## the plotting is a bit complex here, uses the maps library
yhattree <- predict(catree, ca)
yhatrf <- predict(carf, ca)$predictions
rt <- log(ca$medianHouseValue) - yhattree
rr <- log(ca$medianHouseValue) - yhatrf
library(maps)
#png('CalTreeResiduals.png', width=8, height=4, units="in", res=720)
par(mfrow=c(1,2), mai=c(.1,.1,.1,.1), omi=c(0,0,0,0))
map('state', 'california') 
points(ca[,1:2], col=c("red","black")[1 + (rt>0)], cex=abs(rt))
mtext("tree", line=1)
map('state', 'california') 
points(ca[,1:2], col=c("red","black")[1 + (rr>0)], cex=abs(rr))
mtext("forest", line=1)
legend("topright", title="residuals", bty="n", pch=1, 
	pt.cex=c(2,1,1,2), col=c("black","black","red","red"), legend=c(2,1, -1,-2))
#dev.off()
#########################################
#dev.off()
