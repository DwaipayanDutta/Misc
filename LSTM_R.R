# Simple LSTM Model for sentiment mining
# Tensor flow Error Read below
# https://stackoverflow.com/questions/44611325/r-keras-package-error-python-module-tensorflow-contrib-keras-python-keras-was-n
# Data : https://www.kaggle.com/c/quora-insincere-questions-classification/data
rm(list=ls())
load.libraries <- c('tidyverse', "tidytext","keras",'data.table', 'tensorflow')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
suppressPackageStartupMessages(sapply(load.libraries, require, character = TRUE))

options(scipen=999) # turn off scientific display

# Reading the files
train <- fread('C:/Users/Dwaipayan/Desktop/train.csv', data.table = FALSE)
test <- fread('C:/Users/Dwaipayan/Desktop/test.csv', data.table = FALSE)
# Head 
test %>% head()
train %>% filter(target == 1) %>% sample_n(5)

# Setup some parameters
max_words <- 15000 # Maximum number of words to consider as features
maxlen <- 64 # Text cutoff after n words

# Prepare to tokenize the text
full <- rbind(train %>% select(question_text), test %>% select(question_text))
texts <- full$question_text

tokenizer <- text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(texts)

# Tokenize - i.e. convert text into a sequence of integers
sequences <- texts_to_sequences(tokenizer, texts)
word_index <- tokenizer$word_index

# Pad out texts so everything is the same length
data = pad_sequences(sequences, maxlen = maxlen)


# Split back into train and test
train_matrix = data[1:nrow(train),]
test_matrix = data[(nrow(train)+1):nrow(data),]


# Prepare training labels
labels = train$target


# Prepare a validation set
set.seed(1337)

training_samples = nrow(train_matrix)*0.90
validation_samples = nrow(train_matrix)*0.10

indices = sample(1:nrow(train_matrix))
training_indices = indices[1:training_samples]
validation_indices = indices[(training_samples + 1): (training_samples + validation_samples)]

x_train = train_matrix[training_indices,]
y_train = labels[training_indices]

x_val = train_matrix[validation_indices,]
y_val = labels[validation_indices]

# Training dimensions

dim(x_train)
table(y_train)

lines <- readLines('C:/Users/Dwaipayan/Desktop/wiki-news-300d-1M/wiki-news-300d-1M.vec')

fastwiki_embeddings_index = new.env(hash = TRUE, parent = emptyenv())

lines <- lines[2:length(lines)]

pb <- txtProgressBar(min = 0, max = length(lines), style = 3)
for (i in 1:length(lines)){
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word<- values[[1]]
  fastwiki_embeddings_index[[word]] = as.double(values[-1])
  setTxtProgressBar(pb, i)
}

# Create our embedding matrix

fastwiki_embedding_dim = 300
fastwiki_embedding_matrix = array(0, c(max_words, fastwiki_embedding_dim))

for (word in names(word_index)){
  index <- word_index[[word]]
  if (index < max_words){
    fastwiki_embedding_vector = fastwiki_embeddings_index[[word]]
    if (!is.null(fastwiki_embedding_vector))
      fastwiki_embedding_matrix[index+1,] <- fastwiki_embedding_vector # Words without an embedding are all zeros
  }
}


gc()


# Setup input

input <- layer_input(
  shape = list(NULL),
  dtype = "int32",
  name = "input"
)

# Model layers

embedding <- input %>% 
  layer_embedding(input_dim = max_words, output_dim = fastwiki_embedding_dim, name = "embedding")

lstm <- embedding %>% 
  layer_lstm(units = maxlen,dropout = 0.25, recurrent_dropout = 0.25, return_sequences = FALSE, name = "lstm")

dense <- lstm %>%
  layer_dense(units = 128, activation = "relu", name = "dense") 

predictions <- dense %>% 
  layer_dense(units = 1, activation = "sigmoid", name = "predictions")


# Bring model together

model <- keras_model(input, predictions)

# Freeze the embedding weights initially to prevent updates propgating back through and ruining our embedding

get_layer(model, name = "embedding") %>% 
  set_weights(list(fastwiki_embedding_matrix)) %>% 
  freeze_weights()


# Compile

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "binary_crossentropy",
  metrics = "binary_accuracy"
)


# Print architecture (plot_model isn't implemented in the R package yet)

print(model)

# Train model 

history <- model %>% fit(
  x_train,
  y_train,
  batch_size = 2048,
  validation_data = list(x_val, y_val),
  epochs = 35,
  view_metrics = FALSE,
  verbose = 0
)

# Look at training results

print(history)
plot(history)


predictions <- predict(model, test_matrix)
predictions <- ifelse(predictions >= 0.5, 1, 0)

submission = data.frame(cbind(test$qid, predictions))
names(submission) = c("qid", "prediction")

write_csv(submission, "submission.csv")