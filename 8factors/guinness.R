####  dog pic
library(jpeg)
guinness<- readJPEG('guinness.jpg')
dim(guinness)

r <- guinness[,,1]
g <- guinness[,,2]
b <- guinness[,,3]
dim(r)

rPCA <- prcomp(r, center=F)
gPCA <- prcomp(g, center=F)
bPCA <- prcomp(b, center=F)

K <- 25
rW <- predict(rPCA)[,1:K]
gW <- predict(gPCA)[,1:K]
bW <- predict(bPCA)[,1:K]
dim(rW)

rPhi <- rPCA$rotation[,1:K]
gPhi <- gPCA$rotation[,1:K]
bPhi <- bPCA$rotation[,1:K]

newPic <- array(
	c(rW%*%t(rPhi), gW%*%t(gPhi), bW%*%t(bPhi)), 
	dim=c(960,720,3) )
writeJPEG(newPic, "guinnessPCA.jpg")

## print the picture for a set of X
rgbPCA <- list(rPCA, gPCA, bPCA)
for (i in c(3,25,75,150,300)) {
  pcaIMG <- sapply(rgbPCA, function(j) {
    comprIMG <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pcaIMG, paste('guinness_compressed_', round(i,0), '_components.jpg', sep = ''))
}
