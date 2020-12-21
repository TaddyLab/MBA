
#Hass avocado panel example
hass<-read.csv("avocado.csv",strings=T)
library(plm)
pdim(hass,'region')
data.frame(table(hass$region))

hass$Date <- as.Date(hass$Date,format='%m/%d/%Y')
colnames(hass)[3]<-"price"  #rename for convenience
colnames(hass)[4]<-"sales"  #rename for convenience

# make sure order by region and date for plm package
hass<-hass[order(hass$region,hass$Date),] 
head(pdata.frame(hass,index=c('region'))) 

fitPLM<- plm(formula = log(sales) ~ log(price) + type 
             ,data = hass 
             ,model = 'within' # fixed effects model
             ,index = c('region') 
)

summary(fitPLM)

fitGLM <- glm(log(sales)~log(price)+type+region,data=hass)
summary(fitPLM)$coefficients #PLM coefficients
summary(fitGLM)$coefficients[c(2,3),] #equiv reg coefs

data.frame(fixef(fitPLM)) #fixed effects from plm
data.frame(summary(fitGLM)$coefficients[,1])

#can equate
fixef(fitPLM)[2]
summary(fitGLM)$coefficients[1,1] +
  summary(fitGLM)$coefficients[4,1]
fixef(fitPLM)[1]
summary(fitGLM)$coefficients[1,1] 

