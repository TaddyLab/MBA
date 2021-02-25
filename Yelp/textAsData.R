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

library(stopwords)
stopWords <- stopwords(language="en",source = "snowball") #English language, snowball list

# if you want to keep the words "above", and "below", edit the command:
stopWords<-stopWords[!stopWords%in%c("above","below")]

# create a doc term matrix
library(text2vec) # see http://text2vec.org/
library(Matrix)  ## always load this when you load text2vec
prep = tolower  #make everything lowercase
tok = word_tokenizer #words are the tokens
# prep
itYelp = itoken(reviews$text, 
             preprocessor = prep, 
             tokenizer = tok, 
             ids = reviews$review_id)

# create the vocab
vocab<-create_vocabulary(itYelp, stopwords=stopWords)

# prune vocab with settings for rare and common words
vocab<-prune_vocabulary(vocab,
            term_count_min = 10,
            doc_proportion_max = 0.5,
            doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(itYelp, vectorizer)
class(dtm)
dim(dtm)
head(colnames(dtm))

# look at column counts for the bag of words
colSums(dtm[,1:9])
colSums(dtm[,4600:4609])


# we can get bigrams (2-grams) instead of single words with the "ngrams" argument
# ngrams=c(1,2) uses single words and word pairs
# ngrams=c(1,3) used single words, bigrams, and 3-grams (three adjacent words)
vocab2<-create_vocabulary(itYelp,stopwords=stopWords,ngram=c(1,2))
vocab2<-prune_vocabulary(vocab2,
            term_count_min = 10,
            doc_proportion_max = 0.5,
            doc_proportion_min = 0.001)
vectorizer2 = vocab_vectorizer(vocab2)
dtm2 = create_dtm(itYelp, vectorizer2)
dim(dtm2)
head(colnames(dtm2))
tail(colnames(dtm2))

# to stem, specify how to stem first then follow the same steps as previously
stem_tokenizer =function(x) {
   tokens = word_tokenizer(x)
   lapply(tokens, SnowballC::wordStem, language="en")
}
itStemYelp <- itoken(reviews$text, 
          preprocessor = prep, 
          tokenizer = stem_tokenizer, 
          ids = reviews$id)
vocabS <- create_vocabulary(itStemYelp,stopwords=stopWords) 
vocabS <- prune_vocabulary(vocabS,
            term_count_min = 10,
            doc_proportion_max = 0.5,
            doc_proportion_min = 0.001)
vectorizerS = vocab_vectorizer(vocabS)
dtmS = create_dtm(itStemYelp, vectorizerS)
dim(dtmS)
head(colnames(dtmS))

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

png('yelpDMRfit.png', width=4.5, height=4.5, units="in", res=720)
boxplot(phat[cbind(1:nrow(phat),reviews$stars)] ~ stars, data=reviews, 
        xlab="star rating", ylab="p.hat for true rating", col=rev(heat.colors(5)))
dev.off()

reviews$text[l<10][which.max(phat[l<10,1])]
reviews$text[l<10][which.max(phat[l<10,3])]
reviews$text[l<10][which.max(phat[l<10,5])]

## coefficients
B <- coef(fitdmr)
head(B)
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
round(head(W),2)
head(rowSums(W))

# install.packages(c("LDAvis","servr"))
tpc$plot()

# topic regression
library(ranger)

wagerf <- ranger(factor(reviews$stars) ~ W, data=reviews, num.tree=100)

### WORD EMBEDDING
tcm = create_tcm(itYelp, vectorizer)
class(tcm)
dim(tcm)
mean(tcm==0)

glove = GlobalVectors$new(rank = 20, x_max = 10)
wGlove = glove$fit_transform(tcm)
dim(wGlove)

sims = sim2(x = wGlove, y = wGlove[c("sucks","upscale"),])
sort(sims[,"sucks"], decreasing = TRUE)[1:10]
sort(sims[,"upscale"], decreasing = TRUE)[1:10]

taco <- wGlove["cheese",] - wGlove["pizza",] + wGlove["taco",]
sims <- sim2(x = wGlove, y = matrix(taco,nrow=1))
sort(sims[,1], decreasing = TRUE)[1:10]

## 
all(rownames(wGlove)==colnames(dtm))
dim(dtm)
dim(wGlove)
length(l)

# create the review vectors as averages of word vectors for each review
V = as.matrix( (dtm %*% wGlove)/l )
head(V)
summary(glm(reviews$stars ~ V))

