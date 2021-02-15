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

rgbPCA <- list(rPCA, gPCA, bPCA)

pcaIMG <- sapply(rgbPCA, 
			function(color) {
    			comprIMG <- color$x[,1:25] %*% t(color$rotation[,1:25])},
    			simplify="array")
dim(pcaIMG)
dim(guinness)

for (i in c(3,25,75,150,500)) {
  pcaIMG <- sapply(rgbPCA, function(j) {
    comprIMG <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pcaIMG, paste('guinness_compressed_', round(i,0), '_components.jpg', sep = ''))
}
