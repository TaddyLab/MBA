# Clustering examples

## code to produce the bivariate normal plot
mu<-c(0,0)
sigma<-matrix(c(1, 0, 0, 1), 2) 
x1<-seq(-4,4,0.1)
x2<-x<-seq(-4,4,0.1)
library(mvtnorm)
f<-matrix(rep(0,(length(x1)^2)),length(x1))
for(i in 1:length(x1)){
	for(j in 1:length(x1)){
		f[i,j]<-dmvnorm(cbind(x1[i],x2[j]), mean=mu,sigma=sigma)
}}
png('bivariateNorm.png', width=6, height=5, units="in", res=720)
persp(x1, x2, f,col="green",xlab="x1",ylab="x2",zlab="density", theta=-30)
dev.off()

### UBER: K-means
uber <- read.csv("uber.csv")
head(uber)

## run K-means
hubs <- kmeans(uber[,2:3], centers=10, nstart=3)
hubs$centers

## plot the centers (hubs)
library(maps)
#png('uberhubs.png', width=8, height=4, units="in", res=720)
#par(mai=c(0,0,0,0),omi=c(0,0,0,0))
map('county', c('new york','new jersey'), col="grey60",
	xlim=c(-74.4,-73.1), ylim=c(40.4,41))
points(hubs$centers, pch=21, bg=2)
#dev.off()

# choosing k
kSeq<-seq(5,50,by=5)
BIC<-c()
sse<-c()
for(k in 1:length(kSeq)){
	hubs<-kmeans(uber[,2:3],centers=k,nstart=5)
	sse[k]<-hubs$tot.withinss
	BIC[k]<-sse[k]+log(nrow(uber))*k*2
}

#png('uberBICplot.png', width=5, height=5, units="in", res=720)
plot(kSeq,BIC,type="l",xlab="K",ylab="BIC", bty="n")
#dev.off()
