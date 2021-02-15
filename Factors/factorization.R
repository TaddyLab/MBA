####  dog pic

###Bivariate Normal plot
#plot bivariate normal
mu<-c(0,0)
sigma<-matrix(c(1, 0, 0, 1), 2) 
x1<-seq(-4,4,0.1)
x2<-x<-seq(-4,4,0.1)

library(mvtnorm)
f<-matrix(rep(0,(length(x1)^2)),length(x1))
for(i in 1:length(x1)){
	for(j in 1:length(x1)){f[i,j]<-dmvnorm(cbind(x1[i],x2[j]),mean=mu,sigma=sigma)
}}
png('bivariateNorm.png', width=5, height=5, units="in", res=720)
persp(x1, x2, f,col="green",cex.lab=0.001, xlab="x1", ylab="x2", zlab="density")
de.off()

### UBER: K-means
uber <- read.csv("uber.csv")
head(uber)

## run K-means
hubs <- kmeans(uber[,2:3], centers=10, nstart=3)
hubs$centers
library(maps)
png('uberhubs.png', width=8, height=4, units="in", res=720)
par(mai=c(0,0,0,0),omi=c(0,0,0,0))
map('county', c('new york','new jersey'), col="grey60",
	xlim=c(-74.4,-73.1), ylim=c(40.4,41))
points(hubs$centers, pch=21, bg=2)
dev.off()

# choosing k
kSeq<-seq(5,100,by=5)
BIC<-c()
sse<-c()
for(k in 1:length(kSeq)){
	hubs<-kmeans(uber[,2:3],centers=k,nstart=3, iter.max=20)
	sse[k]<-hubs$tot.withinss
	BIC[k]<-sse[k]+log(nrow(uber))*k*2
}

png('uberBICplot.png', width=5, height=5, units="in", res=720)
plot(kSeq,BIC,type="l",xlab="K",ylab="BIC", bty="n")
dev.off()

###PCA
#Guinness
library(jpeg)
setwd("C://Users/leslie.hendrix/Downloads")
guinness<- readJPEG('guinness.jpg')
dim(guinness)

r <- guinness[,,1]
g <- guinness[,,2]
b <- guinness[,,3]
dim(r)

rPCA <- prcomp(r, center=F)
gPCA <- prcomp(g, center=F)
bPCA <- prcomp(b, center=F)

rgbPCA <- list(rPCA, gPCA, bPCA)
str(rPCA$rotation)
str(predict(rPCA))
dim(predict(rPCA))
predict(rPCA)[1:5,1:5]

for (i in round(seq(3,nrow(guinness) - 10, length.out = 10),0)) {
  pcaIMG <- sapply(rgbPCA, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pcaIMG, paste('guinness_compressed_', round(i,0), '_components.jpg', sep = ''))
}

for (i in c(3,25,75,150,500)) {
  pcaIMG <- sapply(rgbPCA, function(j) {
    comprIMG <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pcaIMG, paste('guinness_compressed_', round(i,0), '_components.jpg', sep = ''))
}

summary(rPCA)

par(mfrow=c(1,3))
plot(rPCA,main="Red")
plot(gPCA,main="Green")
plot(bPCA,main="Blue")

########################################################################
##### *** rollcall voting *** #####
setwd("C://Users/leslie.hendrix/Dropbox/extraWork/TaddyProject/9-Factorization")
votes <- read.csv("rollcall-votes.csv")
legis <- read.csv("rollcall-members.csv",strings=T)
votes[1:6,1:5]

pcaVote <- prcomp(votes, scale=TRUE)
plot(pcaVote, main="")
mtext(side=1, "Rollcall-Vote Principle Components",  line=1, font=2)

votePC <- predict(pcavote) # scale(votes)%*%pcavote$rotation
legis <- read.csv("rollcall-members.csv",strings=T)
plot(votePC[,1:2], pch=21, bg=c("darkblue","green","darkred")[legis$party], main="")

# big scores on pc1 are left and right ideologically
votePC[order(votepc[,1])[1:5],1]
votePC[order(-votepc[,1])[1:5],1]

# big scores -/+ on pc 2?
votepc[order(votepc[,2])[1:5],2]
votepc[order(-votepc[,2])[1:5],2]

sort(votePC[,2])

# look at the loadings
loadings <- pcaVote$rotation[,1:2]

## the 1st is traditional left-right
hist(loadings[,1], main="", xlab="1st Principle Component Vote-Loadings",
     col=8, border=grey(.9))
abline(v=loadings[884,1], col="darkred")
text(x=loadings[884,1], y=550, "Afford. Health (amdt.)", xpd=TRUE, col="darkred", font=3)
abline(v=loadings[25,1], col="darkblue")
text(x=loadings[25,1], y=550, "TARP", xpd=TRUE, col="darkblue", font=3)

## trying to interpret the 2nd factor
loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:5],2]
## attendance!
sort(rowSums(votes==0), decreasing=TRUE)[1:5]
####################################################################

###### *** Gas Octane Data *** ######
setwd("C://Users/leslie.hendrix/Dropbox/extraWork/TaddyProject/9-Factorization")
gas <- read.csv("gasoline.csv")
octane <- gas[,1]
nir <- as.matrix(gas[,-1])

#putting nir in terms of just the number
nm <- sapply(
	strsplit(substring(colnames(nir),2), ".", fixed=TRUE),
	function(v) as.numeric(v[1]))

par(mai=c(.8,.8,0,0))
plot(nm, nir[1,], type='l', ylab='NIR', bty="n", xlab="")
for(i in 2:60) 
	lines(nm, nir[i,], col=rainbow(60)[i])
mtext(side=1, "nanometres", line=2.5)

gasPCA<-prcomp(nir,scale=T)
z<-predict(gasPCA)
zDF<-as.data.frame(z)

###get min AICc and BIC for various models to pick K
kfits <- lapply(1:ncol(zDF), 
	function(K) glm(octane~., data=zDF[,1:K,drop=FALSE]))
aicc <- sapply(kfits, AICc) # apply AICc to each fit
which.min(aicc) ## it likes 9 factors best
## you could also use BIC
bic <- sapply(kfits, BIC) 
which.min(bic) ## likes 59

###run lasso with all components

gasPCR <- gamlr(zDF,octane)
B <- coef(gasPCR)[-1,]
B[B!=0]

par(mfrow=c(1,2))
plot(aicc, pch=21, bg="maroon", xlab="K", ylab="AICc")
plot(gasPCR, col=0, ylab="AICc",ylim=c(-170,63))
points(log(gasPCR$lambda), AICc(gasPCR), pch=21,bg="navy")

plot(aicc[1:40], pch=21, bg="maroon", xlab="K", ylab="AICc")

folds<-5
gasLassoCV<-cv.gamlr(nir,octane, nfold=folds,lmr=1e-3)
B<-coef(gasLassoCV)[-1]
comp<-rownames(coef(gasLassoCV))[-1]
cbind(comp[B!=0],B[B!=0])

gasPCRCV <- cv.gamlr(zDF,octane, nfold=folds)
cvB<-coef(gasPCRCV)[-1]
comp<-rownames(coef(gasPCRCV))[-1]
cbind(comp[cvB!=0],cvB[cvB!=0])

gasPCRboth <- cv.gamlr(as.matrix(cbind(nir,zDF)),octane, nfold=folds)
cvBboth<-coef(gasPCRboth)[-1]
comp<-rownames(coef(gasPCRboth))[-1]
cbind(comp[cvBboth!=0],cvBboth[cvBboth!=0])

par(mfrow=c(1,3), mai=c(.2,.2,.5,.1), omi=c(.5,.5,0,0))
plot(gasLassoCV, main="Lasso on X", ylim=c(0,2.5), ylab="", xlab="", df=FALSE, bty="n")
plot(gasPCRCV, main="Lasso on V (PCR)", ylim=c(0,2.5), ylab="", xlab="", df=FALSE, bty="n")
plot(gasPCRboth, main="Lasso on X and V", ylim=c(0,2.5), ylab="", xlab="", df=FALSE, bty="n")
mtext(side=2, "mean squared error", outer=TRUE, line=2)
mtext(side=1, "log lamba", outer=TRUE, line=2)



### marginal regression
phi <- cor(nir, octane)/apply(nir,2,sd) 
v <- nir%*%phi
fwd <- glm(octane ~ v)

par(mai=c(.8,.8,.0,0))
plot(v, octane, pch=21, bg="lightgreen", bty="n", 
	xlab="MR factor v")


### Partial Least Squares
library(textir)
gasPLS <- pls(x=nir, y=octane,  K=3)
par(mfrow=c(1,3), mai=c(.7,.7,.1,.1))
plot(gasPLS, bty="n", cex.lab=1.4,col="darkorange")

foldid <- rep(1:6,each=10)[sample(1:60)]
OOS <- matrix(nrow=6, ncol=10)
for(b in 1:6){
	print(b)
	for(k in 1:10){
		gpls <- pls(x=nir[foldid!=b,], y=octane[foldid!=b], K=k)
		OOS[b,k] <- 
			mean( (octane[foldid==b] - predict(gpls, nir[foldid==b,], K=k))^2 )
	}
}
cvm <- apply(OOS,2,mean)
cvs <- apply(OOS,2,sd)
OOS <- as.data.frame(OOS)
names(OOS) <- 1:10

par(mfrow=c(1,2), mai=c(.9,.8,.8,.1))
boxplot(OOS, ylab="mean squared error", xlab="K", col="darkgreen", log="y", main="", ylim=c(0.01,2))
mtext(side=3, "PLS", line=2)
gasgl <- cv.gamlr(x=nir, y=octane, lmr=1e-3)
plot(gasgl, log="y", main="", ylim=c(0.01,2)) 
mtext(side=3, "lasso", line=2)







