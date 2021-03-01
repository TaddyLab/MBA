###### REGULARIZATION #######

# First example to show over and under-fit and OOS performance
set.seed(865) # this allows you to get the same "random" data
x<-seq(-3,6,0.5) # define x's
data<-(1.5 + (x-1)^2) + rnorm(length(x),0,2.5) #a quadratic function with random noise added
data

fitL<-glm(data~x) #model with a straight line
fitQ<-glm(data~x+I(x^2)) #model with a quadratic
fit18<-glm(data~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+ #18th degree polynomial
	I(x^8)+I(x^9)+I(x^10)+I(x^11)+I(x^12)+I(x^13)+I(x^14)+
	I(x^15)+I(x^16)+I(x^17)+I(x^18))
par(mfrow=c(1,3)) #set graph window to 1 row of 3 graphs
plot(x,data,ylab="y",pch=21,col="darkgreen",bg="darkgreen",bty="n")
abline(fitL,lwd=2) #add regression line
plot(x,data,ylab="y",pch=21,col="darkgreen",bg="darkgreen",bty="n")
lines(x,fitted(fitQ),lwd=2) #add fitted model
plot(x,data,ylab="y",pch=21,col="darkgreen",bg="darkgreen",bty="n")
lines(x,fitted(fit18),lwd=2)

1-fitL$deviance/fitL$null.deviance  #R2 linear
1-fitQ$deviance/fitQ$null.deviance  #R2 quadratic
1-fit18$deviance/fit18$null.deviance  #R2 poly 18

r2OOSSLR<-c(); r2OOSQuad<-c(); r2OOSPoly<-c()
for (i in 1:10000){
	dataNew<-(1.5 + (x-1)^2) + rnorm(length(x),0,2.5)
 
	dev<-sum((dataNew-fitted(fitL))^2)
	dev.null<-sum((dataNew-mean(dataNew))^2)
	r2L<-1-dev/dev.null
	r2OOSSLR <- c(r2OOSSLR,r2L)
 
	dev<-sum((dataNew-fitted(fitQ))^2)
	dev.null<-sum((dataNew-mean(dataNew))^2)
	r2Q<-1-dev/dev.null
	r2OOSQuad <- c(r2OOSQuad,r2Q)

	dev<-sum((dataNew-fitted(fit18))^2)
	dev.null<-sum((dataNew-mean(dataNew))^2)
	r2P<-1-dev/dev.null
	r2OOSPoly <- c(r2OOSPoly,r2P)
  }

boxplot(r2OOSSLR,r2OOSQuad,r2OOSPoly,
     names=c("SLR","Quadratic","18th Polynomial"),
     col=c("blue","green","yellow"),
     main="R-square OOS",cex.main=1.5)

summary(r2OOSSLR)
summary(r2OOSQuad)
summary(r2OOSPoly)
boxplot(r2OOSSLR,r2OOSQuad,r2OOSPoly,
	names=c("SLR","Quadratic","18th Polynomial"),
	col=c("blue","green","yellow"),
	main="R-square OOS",cex.main=1.5)

length(r2OOSSLR[r2OOSSLR<0])


# Semiconductor example
SC <- read.csv("semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance

pVals <- summary(full)$coef[-1,4] #-1 drops intercept
hist(pVals, xlab="P-value", main="", col="blue")

fdr_cut <- function(pvals, q=0.1){
   pvals <- sort(pvals[!is.na(pvals)])
   N <- length(pvals)
   k <- rank(pvals, ties.method="min")
   alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
   
   plot(pvals, log="xy", xlab="order", 
   main=sprintf("FDR of %g",q),
   ylab="p-value", bty="n", 
   col=c(8,2)[(pvals<=alpha) + 1], pch=20)
   lines(1:N, q*(1:N)/(N+1))
 
   return(alpha)
 }
 
fdr_cut(pVals)

signif <- which(pVals <= 0.0122)
cutvar<-c("FAIL",names(signif))
cut <- glm(FAIL ~ ., data=SC[,cutvar],family="binomial")
1 - cut$deviance/cut$null.deviance # new in-sample R2

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred,
     family=c("gaussian","binomial")){
     family <- match.arg(family)
     if(family=="gaussian"){
         return( sum( (y-pred)^2 ) )
     }else{
        if(is.factor(y)) y <- as.numeric(y)>1
        return(-2*sum(y*log(pred)+(1-y)*log(1-pred)))
     }
 }

 ## get null deviance too, and return R2
R2 <- function(y, pred, 
     family=c("gaussian","binomial")){
     fam <- match.arg(family)
     if(fam=="binomial"){
         if(is.factor(y)){ y <- as.numeric(y)>1 }
     }
     dev <- deviance(y, pred, family=fam)
     dev0 <- deviance(y, mean(y), family=fam)
     return(1-dev/dev0)
 }

n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
OOS <- data.frame(full=rep(NA,K), cut=rep(NA,K))

for(k in 1:K){
     train <- which(foldid!=k) # except fold `k'

     ## fit the two regressions
     rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
     rcut <- glm(FAIL~., data=SC[,cutvar],subset=train, family=binomial)

     ## get predictions: type=response so we have probabilities
     predfull <- predict(rfull, newdata=SC[-train,], type="response")
     predcut <- predict(rcut,newdata=SC[-train,], type="response")

     ## calculate and log R2
     OOS$full[k] <- R2(y=SC$FAIL[-train],pred=predfull, family="binomial")
     OOS$cut[k] <- R2(y=SC$FAIL[-train],pred=predcut, family="binomial")

     ## print progress
     cat(k, " ")
 }

apply(OOS,2,mean)

# Forward stepwise regression
null <- glm(FAIL~1, data=SC)
start<-Sys.time()
fwd <- step(null, scope=formula(full), dir="forward")
end<-Sys.time()
end-start
length(coef(fwd)) # chooses around 70 coef

# Sparse Model Matrices
oj<-read.csv("oj.csv",strings=T)
modMat<-model.matrix(~log(price)+brand,data=oj)
modMat[c(100,200,300),] #look at one row for each brand

xnaref <- function(x){
     if(is.factor(x))
         if(!is.na(levels(x)[1]))
             x <- factor(x,levels=c(NA,levels(x)),
     exclude=NULL)
     return(x) }
naref <- function(DF){
     if(is.null(dim(DF))) return(xnaref(DF))
     if(!is.data.frame(DF))
         stop("You need to give me a data.frame
         or a factor")
     DF <- lapply(DF, xnaref)
     return(as.data.frame(DF)) }

ojNAref<-naref(oj)
modMatAllLevs<-model.matrix(~log(price)+brand,data=ojNAref) 
modMatAllLevs[c(100,200,300),]

# get rid of intercept column
modMatNoInt<-model.matrix(~log(price)+brand,data=ojNAref)[,-1]
modMatNoInt[c(100,200,300),]

# make it a sparse representation
library(Matrix)
modMatSparse<-sparse.model.matrix(~log(price)+brand,data=ojNAref)[,-1]
modMatSparse[c(1:3,10001:10003),]

# Simple triplet matrix
rowNum<-c(1,3,2)
colNum<-c(1,1,2)
non0data<-c(-4,5,10)
sparseMat<-sparseMatrix(i=rowNum,j=colNum,x=non0data,
     dims=c(3,2),
     dimnames=list(c("r1","r2","r3"),c("c1","c2")))
sparseMat
str(sparseMat)

# Path estimation in gamlr
# Semiconductor Lasso
library(gamlr)
# create the numeric design matrix.  
scX <- sparse.model.matrix(FAIL ~ ., data=SC)[,-1] # do -1 to drop intercept!
# we included the y (FAIL) so that it woldn't include it with the remaining columns as x's
# here, we could have also just done x <- as.matrix(SC[,-1]).
# but sparse.model.matrix is a good way of doing things if you have factors.
scY <- SC$FAIL # pull out `y' too just for convenience
scLasso <- gamlr(scX, scY, family="binomial")
plot(scLasso) 
scLasso

dim(scLasso$beta) 
sum(scLasso$beta[,1]!=0)
sum(scLasso$beta[,100]!=0)

cvScLasso<-cv.gamlr(scX,scY,family="binomial")
plot(cvScLasso)

cvScLasso$cvm[100] #mean at 100th lambda 
cvScLasso$cvs[100] #SE at 100th lambda
cvScLasso$seg.1se #which lambda for 1se rule
cvScLasso$seg.min #which lambda for cv-min rule
cvScLasso$lambda.1se #lambda # lambda value for 1se rule
log(cvScLasso$lambda.1se)
cvScLasso$lambda.min #lambda # lambda value for CV-min rule
log(cvScLasso$lambda.min)

beta1SE<-coef(cvScLasso) #coefficients using 1se rule
cbind(beta1SE@Dimnames[[1]][which(beta1SE!=0)],beta1SE[which(beta1SE!=0)])

betaMin<-coef(cvScLasso, select="min") # coefficients for CV-min rule
cbind(betaMin@Dimnames[[1]][which(betaMin!=0)],betaMin[which(betaMin!=0)])

1-cvScLasso$cvm[cvScLasso$seg.1se]/cvScLasso$cvm[1] #OOS R2 for 1se
1-cvScLasso$cvm[cvScLasso$seg.min]/cvScLasso$cvm[1] #OOS R2 for CV-min rule

plot(cvScLasso, bty="n")
lines(log(scLasso$lambda),AICc(scLasso)/n, col="green", lwd=2)
lines(log(scLasso$lambda),BIC(scLasso)/n, col="maroon", lwd=2)
legend("top", fill=c("blue","green","maroon"),
	legend=c("CV","AICc","BIC"), bty="n")

