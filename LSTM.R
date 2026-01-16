library(caret)
library(keras)
library(tensorflow)
use_condaenv("tensorflow_env", required = T)

# can't get imdb so should try something else
# imdb <- dataset_imdb(num_words = 500)
# c(c(train_x, train_y), c(test_x, test_y)) %<-% dives2
set.seed(987239)
train_ind <- createDataPartition(dives2$`External O2 Sensor 1 (mV)`,
                                 times = 1,
                                 p = 0.8,
                                 list = F)
train_x <- dives2[train_ind, !names(dives2) %in% c('External O2 Sensor 1 (mV)',
                                                   'External O2 Sensor 2 (mV)',
                                                   'External O2 Sensor 3 (mV)',
                                                   'External O2 Sensor 4 (mV)',
                                                   'External O2 Sensor 5 (mV)')]
test_x <- dives2[-train_ind, !names(dives2) %in% c('External O2 Sensor 1 (mV)',
                                                  'External O2 Sensor 2 (mV)',
                                                  'External O2 Sensor 3 (mV)',
                                                  'External O2 Sensor 4 (mV)',
                                                  'External O2 Sensor 5 (mV)')]

train_y <- dives2[train_ind, names(dives2) %in% c('External O2 Sensor 1 (mV)')]
table(train_y)
train_y

test_y <- dives2[-train_ind, names(dives2) %in% c('External O2 Sensor 1 (mV)')]
table(test_y)
test_y

# train_x <- pad_sequences(train_x, maxlen = 90)
# test_x <- pad_sequences(test_x, maxlen = 90)

# Model
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

# Compile Model
model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("acc"))

# Fit model
history <- model %>% fit(train_x, train_y,
                         epochs = 25,
                         batch_size = 128,
                         validation_split = 0.2)
plot(history)

model %>% evaluate(train_x, train_y) 
pred <- model %>%   
  predict_classes(train_x) 
table(Predicted=pred, Actual=imdb$train$y)   

model %>% evaluate(test_x, test_y) 
pred1 <- model %>%   
  predict_classes(test_x) 
table(Predicted=pred1, Actual=imdb$test$y) 

model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32,return_sequences = TRUE,activation = 'relu') %>% 
  layer_simple_rnn(units = 32,return_sequences = TRUE,activation = 'relu') %>% 
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

z<-NULL
for(i in 1:250000){z[i]<-print(length(train_x[[i]]))}
summary(z)

train_x <- pad_sequences(train_x, maxlen = 200)
test_x <- pad_sequences(test_x, maxlen = 200)

model %>% evaluate(train_x, train_y)
model %>% evaluate(test_x, test_y)

# LSTM Network in R
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_lstm(units = 32,return_sequences = TRUE) %>% 
  layer_lstm(units = 32,return_sequences = TRUE) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile
model %>% compile(optimizer = "adam",
                  loss = "binary_crossentropy",
                  metrics = c("acc"))

# Bidirectional LSTM Model
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_lstm(units = 32,return_sequences = TRUE) %>%
  layer_lstm(units = 32,return_sequences = TRUE) %>%
  bidirectional(layer_lstm(units = 32)) %>%
  layer_dense(units = 1, activation = "sigmoid")

