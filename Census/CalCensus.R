# California census data
ca <- read.csv("CalCensus.csv")
linc <- log(ca[,"medianIncome"])
lhval <- log(ca[,"medianHouseValue"]) 
summary(glm(lhval ~ linc))

# [relatively] fast GP fits
library(laGP)
s <- ca[,1:2] # long and lat
# fitting GP surfaces
gpinc <- aGP(s, linc, s, end=20, verb=0)
gphval <- aGP(s, lhval, s, end=20, verb=0)
# calculate residuals and regress
rinc <- linc - gpinc$mean
rhval <- lhval - gphval$mean
summary(glm(rhval ~ rinc))

### plots!  
#png('CalIncFit.png', width=4, height=5, units="in", res=720)
plot(linc, gpinc$mean, col=rgb(0,0,.1,.25), pch=16, bty="n", 
	xlab="log Median Income", ylab="GP fitted value")
#dev.off()
#png('CalHValFit.png', width=4, height=5, units="in", res=720)
plot(lhval, gphval$mean, col=rgb(.1,0,0,.25), pch=16, bty="n", 
	xlab="log Median Home Value", ylab="GP fitted value")
#dev.off()

# maps package is fun.  Check out fields for more 
hvalBreaks <- quantile(ca$medianHouseValue,(0:20)/20)
hvalCut <- cut(ca$medianHouseValue,breaks=hvalBreaks)
hvalCols <- heat.colors(20)[as.numeric(hvalCut)]

incBreaks <- quantile(ca$medianIncome,(0:20)/20)
incCut <- cut(ca$medianIncome,breaks=incBreaks)
incCols <- heat.colors(20)[as.numeric(incCut)]

library(maps)
#png('CalHVal.png', width=4, height=5, units="in", res=720)
map('state', 'california') 
points(ca[,1:2], col=hvalCols, pch=20)
legend("topright", title="Home Value",
	legend=c("15k","120k","180k","265k","500k"),
	fill=heat.colors(5), bty="n")
#dev.off()

#png('CalInc.png', width=4, height=5, units="in", res=720)
map('state', 'california') 
points(ca[,1:2], col=incCols, pch=20)
legend("topright", title="Income",
	legend=c("5k","26k","35k","47k","150k"),
	fill=heat.colors(5), bty="n")
#dev.off()

######
# Forests
ca <- read.csv("CalCensus.csv")

## First, lets do it with CART
## no need for interactions; the tree finds them automatically
library(tree)
catree <- tree(log(medianHouseValue) ~ ., data=ca) 

#png('CalTree.png', width=5, height=10, units="in", res=720)
plot(catree, col="grey50")
text(catree)
#dev.off()

## looks like the most complicated tree is best! 
cvca <- cv.tree(catree)
cvca$size[which.min(cvca$dev)]
plot(cvca)

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

## out of sample test run
MSE <- list(CART=NULL, RF=NULL)
splits <- sample(1:10, nrow(ca), replace=TRUE)
for(i in 1:10){
  train <- which(splits!=i)
  test <- which(splits==i)

  rt <- tree(log(medianHouseValue) ~ ., data=ca[train,], mindev=1e-4) 
  yhat.rt <- predict(rt, newdata=ca[-train,])
  MSE$CART <- c( MSE$CART, 
  				var(log(ca$medianHouseValue)[-train] - yhat.rt))

  rf <- ranger(log(medianHouseValue) ~ ., data=ca[train,], 
          		num.tree=400, num.threads=4)
  yhat.rf <- predict(rf, data=ca[-train,])$predictions
  MSE$RF <- c( MSE$RF, var(log(ca$medianHouseValue)[-train] - yhat.rf) )
 
  cat(i)
} 
# results
lapply(MSE, mean)
# plot
#png('CalOOS.png', width=5, height=5, units="in", res=720)
boxplot(as.data.frame(MSE), col="dodgerblue", xlab="model", ylab="MSE")
#dev.off()
