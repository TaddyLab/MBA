######################################
#### Causal Inference - Controls
######################################

################## beer

library(gamlr)
load("Beer.rda")
ls()

# data
head(sales)
dim(sales)

head(upc)
dim(upc)  # how many upcs?

# check data types
str(sales)

# calculate lags
sales <- sales[order(sales$upc,sales$week),] 
sales$lag <- unlist(tapply(sales$units, sales$upc, 
							function(x) c(NA,x[-length(x)])))
sales <- sales[!is.na(sales$lag),]
head(sales, 3)
tail(sales, 3)

# create price per oz
sales$lpoz <- log(sales$price/upc[as.character(sales$upc),"oz"])

## calculate fixed effects
library(gamlr)
x <- sparse.model.matrix( ~ week + upc, data=sales)[,-1]
dim(x)

# parse the item description text as a bag o' words
library(text2vec) 
ititle <- itoken(as.character(upc$title),
	tokenizer = word_tokenizer,
	id=rownames(upc))
vocab <- create_vocabulary(ititle)
vectorizer = vocab_vectorizer(vocab)
w = create_dtm(ititle, vectorizer)
class(w)
dim(w)
w[1:5,ncol(w)-0:4]
w[242,w[242,]!=0]

## store and match to the observations
wupc <- w
w <- w[as.character(sales$upc),]
dim(w)

# all together (results are garbage)
coef( margfit <- glm(log(units) ~ lpoz, data=sales) )

## Orthogonal ML
library(sandwich)
library(lmtest)
set.seed(1)
beerDML <- doubleML(cbind(log(sales$lag),x), sales$lpoz, log(sales$units), 
			   nfold=5, lmr=1e-5, free=1)
coeftest(beerDML, vcov=vcovHC(beerDML,"HC0"))

# fit with OLS 
beerOLS <- glm( log(units) ~ lpoz + log(lag) + as.matrix(x), data=sales )
summary(beerOLS)

# do the HTE fit.
ytil <- beerDML$y
dtil <- beerDML$x 
dw <- cbind(dtil, w*drop(dtil))

beerHTE <- gamlr(dw, ytil, standardize=FALSE, lmr=1e-4, free=1)

#png('beerHTE.png', width=4, height=5, units="in", res=720)
plot(beerHTE)
#dev.off()

gam <- coef(beerHTE)[-1,]
sum(gam!=0)
head(sort(gam))

elastics <- drop( cbind(1,wupc) %*% gam )			
upc$elastics <- elastics
head(upc,1)
tail(upc,1)

#png('beerElastics.png', width=5, height=5, units="in", res=720)
hist(elastics, freq=FALSE, xlab="elastcity", main="UPC elasticities")
#dev.off()
