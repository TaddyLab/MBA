#' ---
#' title: "Homework"
#' ---

#' ## Chapter 1 Regression

#' The data are 17379 observations of hourly counts from 2011 to 2012 for bike rides (rentals)
#' from the Capital Bikeshare system inWashington DC. It was originally compiled by Fanaee and
#' Gama in ‘Event labeling combining ensemble detectors and background knowledge’ (2013).
#' bikeshare.csv contains:  

#' * season: 1:spring, 2:summer, 3:fall, 4:winter  
#' * yr: year (0:2011, 1:2012)  
#' * mnth: month (1 to 12)  
#' * hr: hour (0 to 23)  
#' * holiday: whether day is holiday or not  
#' * weekday: day of the week, counting from 0:sunday.  
#' * notbizday: if day is either weekend or holiday is 1, otherwise is 0.  
#' * weathersit:  
#' 1. Clear, Few clouds, Partly cloudy, Partly cloudy  
#' 2. Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist  
#' 3. Light Snow, Thunderstorm + Scattered clouds, Light Rain + Scattered clouds  
#' 4. Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog  
#' * temp Temperature, measured in standard deviations from average.  
#' * hum: Humidity, measured in standard deviations from average.  
#' * windspeed: Wind speed, measured in standard deviations from average.  
#' * dteday: date  
#' * cnt: count of total rental bikes  

#' ### Q: What are the ??  
#' **read the data**
biketab <- read.csv("bikeshare.csv")


#' tell R which are factors
