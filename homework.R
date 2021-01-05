
## chapter 3, SoCalCars

### Larger FDR example
# Now for a larger example
# data.table is useful for dealing with large datasets
# You can install with install.packages("data.table")
library(data.table)

# Unpack the zipped dataset listed below in the datasets folder
# fread is faster than read.table
system.time(lipids <-  fread("./datasets/jointGwasMc_LDL.txt"))

# We'll then convert back to the usual R 'data.frame'.
# data.tables have other nice capabilities that we'll see later in class
lipids <- as.data.frame(lipids)

# Pull out p-values and label them
pvals <- as.numeric(lipids[,'P-value'])
names(pvals) <- lipids[,'rsid']

# Plot the p-value distribution; notice the tiny spike near zero
hist(pvals, main='', xlab='p-values', col=8, border="grey90",freq=FALSE)
dev.copy(png,'hist_lipids.png') # Figure 3.8
dev.off()


# Now to plot figure 3.9
# Top 10 locations to investigate
names(pvals)[order(pvals)[1:10]] 

# Function to get significance cut-off alpha from FDR q
fdr_cut <- function(pvals, q){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/N) ])
  return(alpha)
}

# Find the cut
# @ 10% FDR
cutoff10 <- fdr_cut(pvals,q=.1)
print(cutoff10)
print(sum(pvals<=cutoff10))
# @ 1% FDR
cutoff1 <- fdr_cut(pvals,q=.01)
print(cutoff1)
print(sum(pvals<=cutoff1))
# @ 1/10% FDR
cutoff01 <- fdr_cut(pvals,q=.001)
print(cutoff01)
print(sum(pvals<=cutoff01))
# So you get 4000 discoveries, only 4-5 of which you expect to be false

# Visualize the B+H FDR algorithm 
# warning: the plot can take a bit of time/memory
sig <- factor(pvals<=cutoff01)
o <- order(pvals)
N <- length(pvals)
plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
     ylab="p-values", xlab="tests ordered by p-value", main = 'FDR = 0.1%')
lines(1:N, 0.01*(1:N)/N)
dev.copy(png,'FDR_lipids.png') # Figure 3.9
dev.off()

#### bootstrap analysis of the CPO effect
# recall our earlier test of whether there was a different expected price for
# certified pre-owned vehicles.  We wrote a model that allowed for two different
# price means as $\R{price} = \alpha + \R{certified}\beta + \varepsilon$. We
# then used \R{glm} to fit the model and test for whether $\beta = 0$. To
# re-evaluate this test with bootstrapping, you simply re-fit this model to many
# with-replacement resamples of the \R{Cars} data.  

CertReg <- glm(price ~ certified, data=Cars) 
mle <- coef(CertReg)["certified"]
mlese <- summary(CertReg)$coef["certified","Std. Error"]

# repeat our analysis of the difference in means for CPO cars
Certified <- function(data, obs){
  fit <- glm(price ~ certified, data=data[obs,])
  return(fit$coef["certified"])
}
Certified(Cars, 1:nrow(Cars))
library(boot)
system.time( 
  CertBoot <- boot(Cars, Certified, 10000, parallel="multicore", ncpus=8) 
)
hist(CertBoot$t)
CertBoot 
mean(CertBoot$t)

CertErrors <- CertBoot$t - mle
mle - quantile(CertErrors,c(.975,.025))
quantile(CertBoot$t,c(.025,.975))


### block bootstrap for the panel data example

####  Time series data


# \subsection{Extending and Combining Ideas}\label{sec:DL}
# The AR(\textit{p}) model is a model where we incorporate past values of the dependent variable, $y_t$ and how far we looked in the past was called a lag. Another way to model a time series is to model the current value of a time series $y_t$ on the current and past values of an independent variable, $x$.  This is commonly called a \gls{distributed lag} (\acrshort{dl}) model and is just $y_t$ regressed onto $x_t$ and lagged $x_{t-1}$.  Of course, it is possible to incorporate higher order lags. 

# \begin{example}\label{ex:DLcrime} \textbf{Seattle Crime: Modeling with Lag in $\bm{x}$}
# It might be of interest to explore the relationship between crime and unemployment.  For this exercise, we have one dataset with crime statistics and another with the unemployment rate data for Seattle. The \R{SeattleCrimeStats.csv} file contains data collected on crimes in Seattle from 2008 through 2019 and \R{SeattleUnemployment.csv} contains the monthly unemployment rate in Seattle over the same time period. The first step here is to wrangle the data. Let's start by calling in both datasets and take a look at their structure. 

# \begin{snugshade}
# \begin{verbatim}
# > SeattleCR<-read.csv("SeattleCrimeStats.csv", strings=T)
# > str(SeattleCR)
# 'data.frame':   27125 obs. of  8 variables:
#  $ Police.Beat      : Factor w/ 51 levels "B1","B2","B3",..: 49 
#  $ CRIME_TYPE       : Factor w/ 7 levels "Assault","Burglary",.
#  $ CRIME_DESCRIPTION: Factor w/ 7 levels "Assault","Burglary",.
#  $ STAT_VALUE       : int  12 9 13 3 2 0 0 0 0 3 ...
#  $ REPORT_DATE      : Factor w/ 76 levels "01/01/2008", ...
#  $ Sector           : Factor w/ 17 levels "B","C","D","E",..
#  $ Precinct         : Factor w/ 5 levels "E","N","SE","SW",.
#  $ Row_Value_ID     : int  27265 27264 27263 27262 27261 27260
 
# > SeattleUE<- read.csv("SeattleUnemployment.csv", strings=T)
# > str(SeattleUE)
# 'data.frame':   76 obs. of  2 variables:
#  $ DATE     : Factor w/ 76 levels "2008-01-01","2008-02-01",..
#  $ SEAT653UR: num  3.6 3.7 3.8 3.9 4.1 4.3 4.4 4.6 4.9 5.2 ...
# \end{verbatim}
# \end{snugshade}
# \marginpar{Manipulating data is part of data wrangling --- the process of merging, cleaning, and formatting data to get it into a form that is ready for analysis. It is not uncommon to repeat this process several times after exploratory analysis yields other interesting patterns you would like to explore. This process can often take longer than the actual analysis itself.} 

# Notice there are 76 levels (dates) in the \R{REPORT\_DATE} column, but 27,125 reported crimes since there are several crimes  reported on the same date.  We can aggregate reported crimes by month and year to match the format of the unemployment rate file. 
# \begin{snugshade}
# \begin{verbatim}
# > SeattleCR$REPORT_DATE <- as.Date(SeattleCR$REPORT_DATE,
# +        format='%m/%d/%Y')
# > #aggregate to get total crimes for each date
# > crimeAgg<-aggregate(SeattleCR$STAT_VALUE,
# +       by=list(SeattleCR$REPORT_DATE),FUN=sum)
# > colnames(crimeAgg) <- c('date','totalCrimes')
# > crimeAgg <- crimeAgg[order(crimeAgg$date),]
# > str(crimeAgg)
# 'data.frame':   76 obs. of  2 variables:
#  $ date       : Date, format: "2008-01-01" "2008-02-01" ...
#  $ totalCrimes: int  2712 2573 2803 2822 3064 3073 3210 3086 
# \end{verbatim}
# \end{snugshade}

# And lastly, we format the dates in \R{SeattleUE} and make sure the dates for both datasets match.
# \begin{snugshade}
# \begin{verbatim}
# > SeattleUE$DATE<-as.Date(SeattleUE$DATE,format='%Y-%m-%d')
# > colnames(SeattleUE)<-c("DATE","ueRate")
# > identical(SeattleUE$DATE,crimeAgg$date)
# [1] FALSE
# > which(SeattleUE$DATE-crimeAgg$date!=0)
# [1] 73 74 75 76
# > SeattleUE$DATE[73:76]
# [1] "2014-01-01" "2014-02-01" "2014-03-01" "2014-04-01"
# > crimeAgg$date[73:76]
# [1] "2014-01-30" "2014-02-28" "2014-03-30" "2014-04-30"
# \end{verbatim}
# \end{snugshade}

# Notice the \R{indentical()} function tells us our dates don't match.  However, after finding which don't match and exploring, it appears there is a simple change in reporting to denote the last day instead of the first day of the month. Now that we have unemployment rates for the 76 monthly observations and total crimes for each of the 76 months, the data is ready for analysis. It's reasonable to think that higher unemployment in the previous period as well as the current period could predict crime, so we will use a the current period value of unemployment in Seattle and a 1-period lagged value of the unemployment rate to model the number of crimes in Seattle. 

# \begin{figure}
# \includegraphics[width=0.5\linewidth]{seattleUnemploy.png}
# \caption{\label{fig:seattleUnemploy} Unemployment in Seattle. Notice the peak during the Great Recession around 2010.
# }
# \end{figure}

# \begin{snugshade}
# \begin{verbatim}
# > #fit the model
# > lagUE <- head(SeattleUE$ueRate,-1)
# > head(lagUE)
# [1] 3.6 3.7 3.8 3.9 4.1 4.3
# > currentUE <- SeattleUE$ueRate[2:length(SeattleUE$ueRate)]
# > head(currentUE)
# [1] 3.7 3.8 3.9 4.1 4.3 4.4
# > fitDL1 <- glm(crimeAgg$totalCrimes[-1]~currentUE+lagUE)
# > summary(fitDL1)
# ...
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2772.8      125.0   22.18  < 2e-16 ***
# currentUE      466.1      157.0    2.97  0.00405 ** 
# lagUE         -435.7      153.9   -2.83  0.00602 ** 
# ...
#     Null deviance: 5621215  on 74  degrees of freedom
# Residual deviance: 4921150  on 72  degrees of freedom
# AIC: 1052.7
# \end{verbatim}
# \end{snugshade}
# If your model isn't accounting for enough variability in crimes, you can incorporate other lags.  You can try to improve the model by including different lags and you can even include the idea behind the AR(1) model.  Combining the two ideas is called auto-regressive distributed lag  models and we can quickly fit one here.  

# \begin{snugshade}
# \begin{verbatim}
# > lagC<-head(crimeAgg$totalCrimes,-1)
# > fitARDL1 <- glm(crimeAgg$totalCrimes[-1]~lagC+currentUE+lagUE)
# > summary(fitARDL1)
# ...
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1701.2406   315.4705   5.393 8.65e-07 ***
# lagC           0.3966     0.1087   3.650 0.000497 ***
# currentUE    300.3810   151.9857   1.976 0.051998 .  
# lagUE       -285.5445   148.0775  -1.928 0.057811 .  
# ...
#     Null deviance: 5621215  on 74  degrees of freedom
# Residual deviance: 4143631  on 71  degrees of freedom
# AIC: 1041.8
# \end{verbatim}
# \end{snugshade}
# Indeed, the $R^2$ increased from 1-4921150/5621215 = 0.1245398 to 1-4143631/5621215 = 0.2628585. Once you decide on your model, you can use the output and use the model for prediction in the usual way.  
# \end{example}

# Another common scenario that arises is when you have several time series together. We will show an example of this using Hass avocado sales data.
