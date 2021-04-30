ames <- read.csv("AmesHousing.csv", strings=T)
ames <- ames[,sapply(ames, function(c) sum(is.na(c)))==0]
ames <- ames[,-c(1,3,4,5,6,8,10,11,13,17:27,30:32,38,40:50,52,53)]
ames$Lot.Area <- log(ames$Lot.Area)
names(ames)[1] <- "log.Lot.Area"
ames <- ames[ames$Yr.Sold>=2009,]
ames$Yr.Sold <- NULL
write.csv(ames, "Ames2009.csv", row.names=FALSE)

