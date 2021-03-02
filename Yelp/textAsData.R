########################################################
### TEXT AS DATA
########################################################

library(jsonlite)
## file created via
##$ tail -60000 yelp_academic_dataset_review.json > Yelp60kReviews.json
reviews <- stream_in(file("Yelp60kReviews.json"))
# what did this create?  a data frame!
dim(reviews)
reviews[7,]

# create a doc term matrix
library(text2vec) # see http://text2vec.org/
library(Matrix)  ## always load this when you load text2vec
tokYelp = itoken(reviews$text, 
             preprocessor = tolower, 
             tokenizer = word_tokenizer, 
             ids = reviews$review_id)

# create the vocab and prune stopwords
vocab<-create_vocabulary(tokYelp, stopword=c("if","and","or"))

# prune vocab with settings for rare and common words
vocab<-prune_vocabulary(vocab,
            doc_proportion_max = 0.1,
            doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(tokYelp, vectorizer)
class(dtm)
dim(dtm)

# look at column counts for the bag of words
colSums(dtm[,1:9])
colSums(dtm[,ncol(dtm)-0:3]>0)
dtm[1,dtm[1,]!=0]

# we can get bigrams (2-grams) instead of single words with the "ngrams" argument
# ngrams=c(1,2) uses single words and word pairs
# ngrams=c(1,3) used single words, bigrams, and 3-grams (three adjacent words)
vocab2<-create_vocabulary(tokYelp,ngram=c(1,2))
vocab2<-prune_vocabulary(vocab2,
            doc_proportion_max = 0.1,
            doc_proportion_min = 0.001)
vectorizer2 = vocab_vectorizer(vocab2)
dtm2 = create_dtm(tokYelp, vectorizer2)
dim(dtm2)
dtm2[1,dtm2[1,]>0]
head(colnames(dtm2))
tail(colnames(dtm2))

# to stem, specify how to stem first then follow the same steps as previously
stem_tokenizer =function(x) {
   tokens = word_tokenizer(x)
   lapply(tokens, SnowballC::wordStem, language="en")
}
tokStemYelp <- itoken(reviews$text, 
          preprocessor = tolower, 
          tokenizer = stem_tokenizer, 
          ids = reviews$id)
vocabS <- create_vocabulary(tokStemYelp) 
vocabS <- prune_vocabulary(vocabS,
            doc_proportion_max = 0.1,
            doc_proportion_min = 0.001)
vectorizerS = vocab_vectorizer(vocabS)
dtmS = create_dtm(tokStemYelp, vectorizerS)
dim(dtmS)
dtmS[1,dtmS[1,]>0]

### Text regression

# lasso linear regression
library(gamlr)
fitlin <- gamlr(dtm>0, reviews$stars, lmr=1e-3)

png('yelpLinpath.png', width=4.5, height=4.5, units="in", res=720)
plot(fitlin)
dev.off()

# fitted values
yhat <- drop( predict(fitlin, dtm>0) )

png('yelpLinfit.png', width=4.5, height=4.5, units="in", res=720)
boxplot(yhat ~ stars, data=reviews, col=rev(heat.colors(5)))
dev.off()

## the most positive and negative short reviews
l <- rowSums(dtm)
reviews$text[l<10][which.max(yhat[l<10])]

reviews$text[l<10][which.min(yhat[l<10])]

## inspect coefficients if you'd like
blin <- coef(fitlin)[colnames(dtm),]
mean(blin!=0)
head(sort(blin))
tail(sort(blin))

#### multinomial logit via dmr
library(distrom)
library(parallel)
cl <- makeCluster(detectCores())
fitdmr <- dmr(cl, dtm>0, factor(reviews$stars))

## plot the paths
par(mfrow=c(1,5))
for(k in names(fitdmr)) plot(fitdmr[[k]], main=k)  

phat <- predict(fitdmr, dtm>0, type="response")

png('yelpDMRfit.png', width=4, height=6, units="in", res=720)
boxplot(phat[cbind(1:nrow(phat),reviews$stars)] ~ stars, ylim=c(0,1), data=reviews, 
        xlab="star rating", ylab="p.hat for true rating", col=rev(heat.colors(5)))
dev.off()

reviews$text[l<10][which.max(phat[l<10,1])]
reviews$text[l<10][which.max(phat[l<10,3])]
reviews$text[l<10][which.max(phat[l<10,5])]

## coefficients
B <- coef(fitdmr)
head(round(B,2))
B["sucks",]
exp( B["sucks",1] - B["sucks",5] )
exp( B["sucks",1] - B["sucks",2] )

### TOPIC MODELS
# note that prcomp chokes when expanding to dense
# > pca <- prcomp(dtm, scale=TRUE)
# Error: cannot allocate vector of size 2.2 Gb

# now the topic model
tpc <- LDA$new(n_topics = 20)
W <- tpc$fit_transform(dtm)

dim(W)
round(W[1,],2)
sum(W[1,])

drop( tpc$get_top_words(n = 5, topic=7, lambda=1) )
drop( tpc$get_top_words(n = 5, topic=7, lambda=0) )
drop( tpc$get_top_words(n = 5, topic=19, lambda=1) )
drop( tpc$get_top_words(n = 5, topic=19, lambda=0) )

# install.packages(c("LDAvis","servr"))
tpc$plot()

# topic regression
library(ranger)
wdat <- data.frame(stars=factor(reviews$stars),W)
topicRF <- ranger(stars ~ ., data=wdat, num.tree=100, probability=TRUE)
## misclassification rate
topicRF$prediction.error

pwRF <- predict(topicRF, wdat)$predictions
round(pwRF[1,],2)

png('yelpTopicRF.png', width=4, height=6, units="in", res=720)
boxplot(pwRF[cbind(1:nrow(reviews),reviews$stars)] ~ stars, ylim=c(0,1), data=reviews, 
        xlab="star rating", ylab="p.hat for true rating", col=rev(heat.colors(5)))
dev.off()

### WORD EMBEDDING
tcm = create_tcm(tokYelp, vectorizer)
class(tcm)
dim(tcm)
mean(tcm==0)

glove = GlobalVectors$new(rank = 20, x_max = 10)
vGlove = glove$fit_transform(tcm)
dim(vGlove)
round(vGlove[1,,drop=FALSE],2)

sims = sim2(x = vGlove, y = vGlove["upscale",,drop=FALSE])
sort(sims[,"upscale"], decreasing = TRUE)[1:5]

analogy <- vGlove["pizza",] - vGlove["pepperoni",] + vGlove["tofu",]
sims <- sim2(x = vGlove, y = matrix(analogy,nrow=1))
sort(sims[,1], decreasing = TRUE)[1:5]

## 
all(rownames(vGlove)==colnames(dtm))
dim(dtm)
dim(vGlove)

# create the reviev vectors as averages of word vectors for each review
V = as.matrix( (dtm %*% vGlove)/rowSums(dtm) )
round(V[1,],2)
V[is.na(V)] <- 0

# random forest
vdat <- data.frame(stars=factor(reviews$stars),V)
gloveRF <- ranger(stars ~ ., data=vdat, num.tree=100, prob=TRUE)
## misclassification rate
gloveRF$prediction.error

pvRF <- predict(gloveRF, vdat)$predictions

png('yelpGloveRF.png', width=4, height=6, units="in", res=720)
boxplot(pvRF[cbind(1:nrow(reviews),reviews$stars)] ~ stars, ylim=c(0,1), data=reviews, 
        xlab="star rating", ylab="p.hat for true rating", col=rev(heat.colors(5)))
dev.off()
