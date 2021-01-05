
#Hass avocado panel example
hass <- read.csv("hass.csv",stringsAsFactors=TRUE)
head(hass,3)

hass$date <- as.Date(hass$date,format='%m/%d/%Y')
hass<-hass[order(hass$region,hass$date),] 
head(hass,3)

fitHass <- glm(log(units) ~ log(asp) + region, data=hass)
coef(fitHass)["log(asp)"]

hass$week <- factor(hass$date)
fitHassDF <- glm(log(units) ~ log(asp) + region + week, data=hass)
coef(fitHassDF)["log(asp)"]


tapply(c(1:5), c("a","b","a","b","c"), function(x) max(x))

hass$lag.asp <- unlist(tapply(hass$asp, hass$region, 
					function(x) c(NA,x[-length(x)])))
hass$lag.units <- unlist(tapply(hass$units, hass$region, 
					function(x) c(NA,x[-length(x)])))

fitHassLags <- glm(log(units) ~ log(lag.units) + log(asp)  
							+ log(lag.asp) + region, data=hass)
summary(fitHassLags)$coefficients[2:4,] #equiv reg coefs
