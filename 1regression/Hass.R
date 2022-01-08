####### Regression #######
## Hass avocado panel example

hass <- read.csv("hass.csv",stringsAsFactors=TRUE)
head(hass,3)

# convert date to date variable
hass$date <- as.Date(hass$date,format='%m/%d/%Y')

# order by date and region - good practice in panel data
hass<-hass[order(hass$region,hass$date),] 
head(hass,3)

# regress onto log(asp) and region fixed effects
fitHass <- glm(log(units) ~ log(asp) + region, data=hass)
coef(fitHass)["log(asp)"]

# adding week to the model
hass$week <- factor(hass$date)
fitHassDF <- glm(log(units) ~ log(asp) + region + week, data=hass)
coef(fitHassDF)["log(asp)"]

## tapply demo
#  output is a list 
tapply(c(1:5), c("a","b","a","b","c"), function(x) max(x))

# use tapply to create lagged variables
# we can do this since we ordered the data
hass$lag.asp <- unlist(tapply(hass$asp, hass$region, 
					function(x) c(NA,x[-length(x)])))
hass$lag.units <- unlist(tapply(hass$units, hass$region, 
					function(x) c(NA,x[-length(x)])))
head(hass,3) #looks like it is ordered correctly

# add lagged variables to model
fitHassLags <- glm(log(units) ~ log(lag.units) + log(asp)  
							+ log(lag.asp) + region, data=hass)
summary(fitHassLags)$coefficients[2:4,] #equiv reg coefs
