####### Classification #######
####### Heating Data #######

## Data from kenneth train,
## for more economically detailed analysis, see
## https:https://cran.r-project.org/web/packages/mlogit/vignettes/e1mlogit.html
## 
## outcome categories
# gas central (gc),
# gas room (gr),
# electric central (ec),
# electric room (er),
# heat pump (hp).

library(mlogit)
data(Heating)
head(Heating)

# create model matrix and pull out response
xH <- model.matrix( depvar ~ ., data=Heating[,-1])[,-1]
yH <- Heating$depvar
table(yH)

# mnlogit in glmnet
library(glmnet)
netfit <- glmnet(xH, yH, family="multinomial")

# some predictions at lambda100
L100 <- min(netfit$lambda) 
pnet <- drop( predict(netfit, xH, s=L100, type="response") )
newi <- c(1,14,21,4,24)
pnet[newi,]
yH[newi]

head( levels(yH)[apply(pnet,1,which.max)] )

hpHat <- pnet[,"hp"]> 0.1
table(trueHP=yH=="hp", predHP=hpHat==1)
8/(8+56) # precision

# png('glmnetMNFit.png', width=6, height=4, units="in", res=720)
boxplot( pnet[cbind(1:nrow(xH),as.numeric(yH))] ~ yH, col=2:6, ylab="p.hat for y", xlab="", varwidth=TRUE)
# dev.off()

# coefficients for lambda100
Bnet <- coef(netfit,s=min(netfit$lambda))
Bnet <- do.call(cbind, Bnet)
colnames(Bnet) <- levels(yH)
round(Bnet, 4)

exp( (Bnet["income","hp"] - Bnet["income","gc"]) )

# path plots
#png('glmnetMNLogit.png', width=12, height=4, units="in", res=720)
par(mfrow=c(1,5)) ## note we can use xvar="lambda" to plot against log lambda
plot(netfit, xvar="lambda") 
#dev.off()

# with cross validation
cv.netfit <- cv.glmnet(xH, yH, family="multinomial")

log(cv.netfit$lambda.min)
Bcv <- do.call(cbind, coef(cv.netfit,s="lambda.min"))
colnames(Bcv) <- levels(yH)
round(Bcv, 4)


## distributed multinomial regression
library(parallel)
cl = makeCluster(detectCores())
cl
library(distrom)
dmrfit <- dmr(cl, xH, yH)
length(dmrfit)
dmrfit[[1]]

# coefficients at lambda100s (different for each class)
round(coef(dmrfit),4)

# path plots
#png('dmrMNLogit.png', width=12, height=4, units="in", res=720)
par(mfrow=c(1,5))
for(k in names(dmrfit)) plot(dmrfit[[k]], main=k)  
#dev.off()

### extra: fitting an econ-style choice model
heat <- cbind( 
	Heating[,c("depvar","income","agehed","rooms","region")],
	incost = as.numeric(Heating[cbind(1:nrow(Heating),2+as.numeric(Heating$depvar))]),
	opcost = as.numeric(Heating[cbind(1:nrow(Heating),7+as.numeric(Heating$depvar))]))
head(heat)

xH2 <- model.matrix( depvar ~ ., data=heat)[,-1]
netfit2 <- glmnet(xH2, yH, data=heat, family="multinomial")
bnet2 <- do.call( cbind, coef(netfit2,s=min(netfit2$lambda)) )
colnames(bnet2) <- levels(yH)
round(bnet2, 4)
