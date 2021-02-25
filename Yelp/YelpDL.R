# data from https://www.yelp.com/dataset
library(keras)
library(tensorflow) # on some systems you might not need this line
# you need to wait a beat before running below.
# tf$constant("Hellow Tensorflow")

library(jsonlite)
## file created via
##$ tail -60000 yelp_academic_dataset_review.json > Yelp60kReviews.json
reviews <- stream_in(file("Yelp60kReviews.json"))
# what did this create?  a data frame!
dim(reviews)
reviews[1,]

# instead of one-hot encoding (i.e., bag of words) we'll 
# turn to a numeric sequence representation
max_tokens <- 10000
max_length <- 300
text_vectorization <- layer_text_vectorization(
  max_tokens = max_tokens, 
  output_sequence_length = max_length, 
)

#help("layer_text_vectorization")
text_vectorization %>% 
  adapt(reviews$text)

get_vocabulary(text_vectorization) # e.g., item 15 is 'this'

text_vectorization(matrix(reviews$text[1], ncol = 1))
text_vectorization(matrix("this restaurant rules", ncol=1))[1,1:5]
text_vectorization(matrix("this restaurant sucks", ncol=1))[1,1:5]

######### EG 1
# create the sentiment target
reviews$positive <- as.numeric(reviews$stars > 3)

#define the model
input <- layer_input(shape = c(1), dtype = "string")
hidden <- input %>% 
  text_vectorization() %>% 
  layer_embedding(input_dim = max_tokens+1, output_dim = 32) %>%
  layer_global_average_pooling_1d() %>% # average across words in sequence
  layer_dropout(0.1) %>% # dropout!  Bayesian interpretation...
  layer_dense(units = 16, activation = "relu") 
output <- hidden %>%
  layer_dense(units = 1, activation = "sigmoid") # sigmoid = logistic

starmod <- keras_model(input, output)
summary(starmod)

starmod %>% compile(
  optimizer = 'adam', # a key ingredient
  loss = 'binary_crossentropy', # just DL fancy for 'logistic'
  metrics = list('accuracy') # track accuracy in addition to loss
)

set.seed(5807)
train <- sample.int(6e4,5e4) ## random sample.  
history <- starmod %>% fit(
  reviews$text[train],
  reviews$positive[train],
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

results <- starmod %>% evaluate(reviews$text[-train], 
                              as.numeric(reviews$stars[-train] > 3), 
                              verbose = 0)
results # should give >90% OOS accuracy

# another plot of training 
plot(history)

# look at predicted probabilities
probs <- predict(starmod, reviews$text[-train])
probs[1] # likely negative
reviews$text[-train][1]
probs[2] # likely positive
reviews$text[-train][2]
probs[10] # unsure ~ 1/2 prob
reviews$text[-train][10] #actually pretty negative

## access embeddings
starmod$layers[[3]]$weights
# [[1]]
# <tf.Variable 'embedding/embeddings:0' shape=(10000, 32) dtype=float32, numpy=
starmod$layers[[3]]$output # 300 x 32
starmod$layers[[4]]$output # 1 x 32 (just does a simple average)
###

# you can also do linear regression
linout <- hidden %>% 
  layer_dense(units = 1) 
linstarmod <- keras_model(input, linout)
linstarmod %>% compile(optimizer = 'adam', loss = 'mse')
linstarmod %>% fit(
  reviews$text[train],
  reviews$stars[train], # just raw star count
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)
pred_star <- predict(linstarmod, reviews$text[-train])
boxplot(pred_star ~ reviews$stars[-train])


#####################################
## vote modeling, multi-task learning
## multiple inputs, multiple outputs
## https://keras.rstudio.com/articles/functional_api.html

# modelling vote counts, note that they depend upon date
boxplot(reviews$useful ~ format(as.Date(reviews$date),"%Y"), outline=FALSE)
# create a date input (crucial to scale to get good convergence)
reviews$numdate <- scale(as.numeric(as.Date(reviews$date))) 
# create the date input layer
date_input <- layer_input(shape = c(1))

# re-create our text input layer
text_input <- layer_input(shape = c(1), dtype = "string")
# build a new intermediate text processing architecture
text_hidden <- text_input %>%
  text_vectorization() %>% 
  layer_embedding(input_dim = max_tokens + 1, output_dim = 32) %>%
  layer_global_average_pooling_1d() %>%
  layer_dropout(0.1) %>% 
  layer_dense(units = 16, activation = "relu")
  
## combine shared text layer with date for each output
funny_out <- layer_concatenate(c(text_hidden, date_input))  %>% 
  layer_dense(units = 8, activation="relu") %>%
  layer_dense(units = 1, activation = "exponential", name="funny") 

useful_out <- layer_concatenate(c(text_hidden, date_input))  %>% 
  layer_dense(units = 8, activation="relu") %>%
  layer_dense(units = 1, activation = "exponential", name="useful") 

cool_out <- layer_concatenate(c(text_hidden, date_input))  %>% 
  layer_dense(units = 8, activation="relu") %>%
  layer_dense(units = 1, activation = "exponential", name="cool")

votemod <- keras_model(list(text_input,date_input), 
                      list(funny_out,useful_out,cool_out))
votemod %>% compile(optimizer = 'adam', loss="poisson") # poisson for count data
# could also have mixed losses, e.g. loss=c("mse","poisson","mse")
# or given different weights, 
# e.g. loss_weights=c(1.0,0.2,0.2) to focus on the funny reviews

votemod %>% fit(
  x = list(reviews$text,reviews$numdate), 
  y = as.list(reviews[,c("funny","useful","cool")]),
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

evote <- do.call(cbind, 
     predict(votemod, list(reviews$text,rep(0,6e4))))

plot(evote[,1:2])
# for example, look at two reviews with high expected funny votes 
# but relatively low expected useful votes

(humor <- which(evote[,1] > 15 & evote[,2] < 15 & evote[,3] > 20))
substr(reviews$text[humor],1,1000)


## for SGD, redo linear model with SGD.

## take another look at text vectorizer
shorttext <- c("all world is a stage","all world","world","all")
# integer vectorization; storage grows with length of docs
lilvec <- layer_text_vectorization(max_tokens=6, output_sequence_length = 5) 
lilvec %>% adapt(shorttext)
lilvec(matrix(shorttext, ncol = 1))
#bag of words; storage grows with size of vocabulary
lilbow <- layer_text_vectorization(output_mode = "binary", max_tokens=5)
lilbow %>% adapt(shorttext)
lilbow(matrix(shorttext, ncol = 1))

# build a bag of words layer for our review text
bow <- layer_text_vectorization(output_mode = "binary", max_tokens=3000)
bow %>% adapt(reviews$text)
bow(matrix(reviews$text[1:2],ncol=1))

text_in <- layer_input(shape = c(1), dtype = "string")
bow_out <- text_in %>% bow %>% layer_dense(units = 1, activation = "sigmoid")
logitmod <- keras_model(text_in, bow_out)
logitmod %>% compile(optimizer = 'adam', 
                     loss = 'binary_crossentropy',
                     metrics="accuracy")

set.seed(5807)
train <- sample.int(6e4,5e4) ## random sample.  
history <- logitmod %>% fit(
  reviews$text[train],
  as.numeric(reviews$stars[train] > 3),
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

logitmod %>% evaluate(reviews$text[-train], 
                 as.numeric(reviews$stars[-train] > 3))

results # should give >90% OOS accuracy ~ same as the NN!
logitmod$layers[[3]]$weights

beta <- as.matrix(logitmod$layers[[3]]$weights[[1]])
vocab <- get_vocabulary(bow)
exp( c( max(beta), min(beta) ) )
vocab[c( which.max(beta), which.min(beta) )-1]
vocab[which.min(beta)-1]


#########################################
## Image classification, CNNs, transfer learning
# Include_top=FALSE removes the top architecture, and then this layer would 
# output higher dimension image embeddings for use in another training scheme.
inception <- application_inception_v3(weights = 'imagenet', include_top = TRUE)

## dataset of images
library(jsonlite)
pics <- stream_in(file("photos5k.json"))
pics$label <- factor(pics$label)
table(pics$label)

# function to read a jpeg and feed in through inceptionv3
# required '$conda install pillow'
iv3gen <- function(id){
  # read the image and convert to an array
  img <- image_load(sprintf("photos/%s.jpg",id), target_size = c(299,299))
  x <- image_to_array(img)
  # ensure we have a 4d tensor with single element in the batch dimension
  x <- array_reshape(x, c(1, dim(x)))
  # use the inception pre-preprocessing fn (requires 299x299 size)
  x <- inception_v3_preprocess_input(x)
  # predict the 1000 dim embedding using trained inception model
  inception %>% predict(x)
}

# test it on a picture of the mcmuffin lineup
mcmuffin <- iv3gen("Z2HgD4BT3aMe9LTN5FAgkg")
dim(mcmuffin)
# imagenet decoding to map to highest probability classes
imagenet_decode_predictions(mcmuffin, top = 3)[[1]]
# i can haz cheezburger with 14% prob, but more likely bagel

# read in the features from inception3 (pre-processed)
iv3feat <- read.csv("photos5k_iv3.csv",row.names=1)
all(rownames(iv3feat)==pics$photo_id)

# format for fit
iv3feat <- as.matrix(iv3feat)
piclab <- to_categorical(as.numeric(pics$label)-1)

# standard flow of input/output/mod/compile/fit
pic_in <- layer_input(shape = c(1000))
pic_out <- pic_in %>% 
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 5, activation = "softmax") 
pic_mod <- keras_model(pic_in, pic_out)
pic_mod %>% compile(optimizer = 'adam', 
                  loss = 'categorical_crossentropy',
                  metrics="accuracy")
pic_mod %>% fit(
  x = iv3feat,
  y = piclab, 
  epochs = 10,
  batch_size = 10,
  validation_split = 0.2,
  verbose=2
)

## look at performance on some test pictures
testpics <- rbind(iv3gen("Z2HgD4BT3aMe9LTN5FAgkg"),# mcmuffin
                  iv3gen("_2p-dRCq5cLqfGoNiEVo2g"),# menu
                  iv3gen("--8aLa0rf2gfOjA6kCC3WQ"),# bar
                  iv3gen("_5w-w04BmPKh9R3Eh7t3lA"))# eggspectation
testpics <- pic_mod %>% predict(testpics)
colnames(testpics) <- levels(pics$label)
round(testpics, 2)



