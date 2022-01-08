######################################
#### Text as Data
######################################

### *** LastFM play counts *** ###

lastFM<-read.csv("lastfm.csv",colClasses="factor")
head(lastFM)

#convert to sparse Matrix for text2vec
library(Matrix)
fmX <-  sparseMatrix(
      i=as.numeric(lastFM$user),
      j=as.numeric(lastFM$artist), 
      x=rep(1,nrow(lastFM)),
      dims=c(nlevels(lastFM$user),nlevels(lastFM$artist)),
      dimnames=list(levels(lastFM$user),levels(lastFM$artist)))
dim(fmX)
fmX[1:2,2:5]
fmX[1, fmX[1,]!=0]

# run the topic model
library(text2vec)
fmLDA <- LDA$new(n_topics = 10)
fmW <- fmLDA$fit_transform( fmX )
fmLDA$get_top_words(n = 3, lambda=1)
fmLDA$get_top_words(n = 3, lambda=0)


# install.packages(c("LDAvis","servr"))
fmLDA$plot()
