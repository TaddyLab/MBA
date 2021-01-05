
#Hass avocado panel example
hass<-read.csv("hass.csv",stringsAsFactors=TRUE)
head(hass,3)

hass$date <- as.Date(hass$date,format='%m/%d/%Y')
hass<-hass[order(hass$region,hass$date),] 
head(hass,3)

tapply(c(1:5), c("a","a","b","b","c"), function(x) max(x))

hass$lag.asp <- unlist(tapply(hass$asp, hass$region, 
					function(x) c(NA,x[-length(x)])))
hass$lag.units <- unlist(tapply(hass$units, hass$region, 
					function(x) c(NA,x[-length(x)])))

fitHass <- glm(log(units) ~ log(asp) + log(lag.units) 
							+ log(lag.asp) + region, data=hass)
summary(fitHass)$coefficients[2:4,] #equiv reg coefs
