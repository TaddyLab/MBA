
### Seattle Crime
SeattleCR<-read.csv("SeattleCrimeStats.csv", strings=T)
str(SeattleCR)
SeattleUE<- read.csv("SeattleUnemployment.csv", strings=T)
str(SeattleUE)

SeattleCR$REPORT_DATE <- as.Date(SeattleCR$REPORT_DATE,
                                 format='%m/%d/%Y')
#aggregate to get total crimes for each date
crimeAgg<-aggregate(SeattleCR$STAT_VALUE,
                    by=list(SeattleCR$REPORT_DATE),FUN=sum)
colnames(crimeAgg) <- c('date','totalCrimes')
crimeAgg <- crimeAgg[order(crimeAgg$date),]
str(crimeAgg)


SeattleUE$DATE<-as.Date(SeattleUE$DATE,format='%Y-%m-%d')
colnames(SeattleUE)<-c("DATE","ueRate")
identical(SeattleUE$DATE,crimeAgg$date) #uh-oh
which(SeattleUE$DATE-crimeAgg$date!=0) #find out where the problem is
SeattleUE$DATE[73:76]
crimeAgg$date[73:76] # ahhh....looks like a change in reporting

#fit the model
lagUE <- head(SeattleUE$ueRate,-1)
head(lagUE)
currentUE <- SeattleUE$ueRate[2:length(SeattleUE$ueRate)]
head(currentUE)
fitDL1 <- glm(crimeAgg$totalCrimes[-1]~currentUE+lagUE)
summary(fitDL1)
1-fitDL1$deviance/fitDL1$null.deviance #R-squared

#autoregressive and lags on x
lagC<-head(crimeAgg$totalCrimes,-1)
fitARDL1 <- glm(crimeAgg$totalCrimes[-1]~lagC+currentUE+lagUE)
summary(fitARDL1)
1-fitARDL1$deviance/fitARDL1$null.deviance #R-squared