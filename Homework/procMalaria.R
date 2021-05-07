# https://doi.org/10.7910/DVN/M4LY6C
library(haven)
d <- read_dta("PACT_30March2012.dta")
d <- as.data.frame(d)
d <- d[d$p35%in%c(1,2),]
d <- d[d$treated%in%c(0,1),]
d$adhere <- as.numeric(d$p35==1)
d$age <- d$h3_respage
d <- d[d$age>0, ]
d$treated <- d$treated_ate
d <- d[d$h2_respsex %in% c(1,2), ]
d$male <- as.numeric(d$h2_respsex==1) 

write.csv(d[,c("treated","adhere","age","male")], 
		  file="malaria.csv", row.names=FALSE)

