### *** European Protein Consumption, in grams/person-day *** ###
food <- read.csv("protein.csv", row.names=1) # 1st column is country name

# fit the factors
pcfood <- prcomp(food, scale=TRUE)
round(pcfood$rotation, 1)
round( predict(pcfood, newdata=food["France",]),2)
head( round(wfood <- predict(pcfood),1)) 

## predict is just doing the same thing as the below:
w <- scale(food)%*%pcfood$rotation
all(w==wfood)

## implies rotations are on scale of standard deviations if scale=TRUE
## looks like PC1 is an 'average diet', PC2 is iberian
t( round(pcfood$rotation[,1:2],2) )

## do some k-means, for comparison
K <- 4
grpProtein <- kmeans(wfood, centers=K, nstart=20)

## how do the PCs look?
#png('proteinPCA.png', width=10, height=5, units="in", res=720)
par(mfrow=c(1,2))
plot(wfood[,1:2], type="n", xlim=c(-4,5),bty="n")
text(x=wfood[,1], y=wfood[,2], labels=rownames(food), col=grpProtein$cluster)
plot(wfood[,3:4], type="n", xlim=c(-3,3),bty="n")
text(x=wfood[,3], y=wfood[,4], labels=rownames(food), col=grpProtein$cluster)
#dev.off()

## Scree plot
#png('proteinScree.png', width=5, height=5, units="in", res=720)
plot(pcfood, main="European Protein PCA")
#dev.off()

## summary puts these scree plots on a more intuitive scale: 
## proportion of variation explained.
summary(pcfood)

## same things, but with eigen values
X <- as.matrix(scale(food))
xbar <- colMeans(X)
XX <- crossprod(X) 
S <- XX/nrow(X) - tcrossprod(xbar)
evd <- eigen(S, symmetric=TRUE)
wevd <- X%*%evd$vectors

#png('proteinEVD.png', width=5, height=5, units="in", res=720)
plot(wfood,wevd,pch=20,bty="n")
#dev.off()
