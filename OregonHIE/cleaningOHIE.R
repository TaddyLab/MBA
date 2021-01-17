######################################
#### Causal Inference - Experiments
######################################

################## OHIE
# Get the data from nber.org/oregon/4.data.html

# person_id  is key
# treatment is in Description file, and is random conditional on the numhh_list (number of names in lottery)
# in 2008 new spots opened for medicaid, which was previously closed to new enroll
# we are interested in health insurance effect on increased costs and utilization (on health is longer term)
# admin data is clean, survey data not necessarily balanced due to non-response bias
# admin data has hospital admission (by dept, emerg itself is non-signif)
# we can also look at number of hostpital days or total list cost

library(foreign)

descr <- read.dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey12m_vars.dta")

# nicely organized, one row per person
all(s12$person_id == descr$person_id)
all(s12$person_id == prgm$person_id)

P <- descr[,c("person_id","household_id", "numhh_list")]
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
levels(P$numhh_list) <- c("1","2","3+")

# 12 month is the survey that really matters
# need to control for household size interacted with survey return time
Y <- s12[,c("weight_12m","doc_num_mod_12m")]

# smk_ever_12m - num19_12m are sources of heterogeneity, plus descr
X <- s12[,121:147]
X$dt_returned <- factor(format(s12$dt_returned_12m, "%Y-%m"))

insurv <- which(s12$sample_12m_resp == "12m mail survey responder")
X <- X[insurv,]
Y <- Y[insurv,]
P <- P[insurv,]

sapply(Y,function(y) sum(is.na(y)))
nomiss <- which( !apply(Y,1, function(y) any(is.na(y))) )
X <- X[nomiss,]
Y <- Y[nomiss,]
P <- P[nomiss,]

# replace some ridiculous values in survey and drop num19
X$hhsize_12m[X$hhsize_12m>10] <- 10
X$num19_12m <- NULL

# organize to make it pretty for text
names(P)[3] <- "numhh"
names(Y) <- sub("_12m","",names(Y))
names(Y) <- sub("_mod","",names(Y))
names(X) <- sub("_12m","",names(X))


OHIEresults <- cbind(P,Y)
OHIEsurvey <- cbind(P[,1,drop=FALSE],X)

write.csv(OHIEresults,"OHIEresults.csv",row.names=FALSE,quote=FALSE)
write.csv(OHIEsurvey,"OHIEsurvey.csv",row.names=FALSE)
