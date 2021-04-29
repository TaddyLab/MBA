byday <- read.csv("day.csv")
byday <- byday[,c(2,5,6,7,8,9,10,12,13,16)]
byday[1,]

byday$temp <- byday$temp*41 #C
byday$hum <- byday$hum*100 #%
byday$windspeed <- byday$windspeed*67 #%kmh

byday$weathersit <- factor(byday$weathersit, 
                labels=c("clear","cloudy","wet"))
write.csv(byday, file="bikeshare.csv", row.names=FALSE)
