####### Regression #######
###### Gaussian Process Modeling

# California census data
ca <- read.csv("CalCensus.csv")
ca[1,]

linc <- log(ca[,"medianIncome"])
lhval <- log(ca[,"medianHouseValue"]) 
summary(glm(lhval ~ linc))

# [relatively] fast GP fits
library(laGP)
s <- ca[,1:2] # long and lat
# fitting GP surfaces
gpinc <- aGP(s, linc, XX=s, end=20, verb=0)
gphval <- aGP(s, lhval, XX=s, end=20, verb=0)
# calculate residuals and regress
rinc <- linc - gpinc$mean
rhval <- lhval - gphval$mean
summary(glm(rhval ~ rinc))

### plots! 
par(mfrow=c(1,2)) 
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

