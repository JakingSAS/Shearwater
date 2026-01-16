library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)
library(keras)
library(tensorflow)

######################
# Helper functions
######################


######################
# Select and clean model features
######################
features <- dives2 |>
  select(
    dive_id     = Dive.Number,
    time_sec    = `Time (sec)`,
    depth       = Depth,
    water_temp  = `Water Temp`,
    sensor1     = `External O2 Sensor 1 (mV)`,
    sensor2     = `External O2 Sensor 2 (mV)`,
    sensor3     = `External O2 Sensor 3 (mV)`
  ) |>
  drop_na()

feature_cols <- c("depth", "water_temp", "sensor1", "sensor2", "sensor3")

######################
# Within-dive scaling
######################
seq_features <- c("depth", "water_temp", "sensor1", "sensor2", "sensor3")

df_seq_scaled <- features %>%
  group_by(dive_id) %>%
  mutate(
    across(
      all_of(seq_features),
      ~ {
        s <- sd(.x, na.rm = TRUE)
        ifelse(is.na(s) | s == 0, 0, (.x - mean(.x, na.rm = TRUE)) / s)
      }
    )
  ) %>%
  ungroup()


######################
# Build 3D LSTM input array
######################
timesteps <- df_seq_scaled %>%
  count(dive_id) %>%
  summarise(t = max(n)) %>%
  pull(t)

dive_ids <- df_seq_scaled %>% distinct(dive_id) %>% pull()
n_dives <- length(dive_ids)
n_features <- length(seq_features)

X_seq <- array(0, dim = c(n_dives, timesteps, n_features))

for (i in seq_along(dive_ids)) {
  tmp <- df_seq_scaled %>%
    filter(dive_id == dive_ids[i]) %>%
    arrange(time_sec) %>%
    select(all_of(seq_features)) %>%
    as.matrix()
  
  X_seq[i, 1:nrow(tmp), ] <- tmp
}

######################
# Static features
######################
df_static <- features %>%
  group_by(dive_id) %>%
  summarise(max_depth = max(depth, na.rm = TRUE), 
            run_time  = max(time_sec, na.rm = TRUE))

df_static <- df_static %>%
  filter(dive_id %in% dive_ids) %>%
  arrange(match(dive_id, dive_ids))

X_static <- df_static %>%
  select(max_depth, run_time) %>%
  as.matrix()

# Optional: global scaling for statics
X_static <- scale(X_static)
n_static <- ncol(X_static)

######################
# Multi-input temporal autoencoder
######################
seq_input <- layer_input(
  shape = c(timesteps, n_features),
  name = "sequence_input"
)

static_input <- layer_input(
  shape = c(n_static),
  name = "static_input"
)

# ----- Encoder -----
encoded_seq <- seq_input %>%
  layer_lstm(64, return_sequences = FALSE)

encoded_static <- static_input %>%
  layer_dense(16, activation = "relu")

latent <- layer_concatenate(list(encoded_seq, encoded_static)) %>%
  layer_dense(32, activation = "relu", name = "latent")

# ----- Decoder -----
decoded <- latent %>%
  layer_dense(timesteps * n_features) %>%
  layer_reshape(target_shape = c(timesteps, n_features))

model <- keras_model(
  inputs = list(seq_input, static_input),
  outputs = decoded
)

model %>% compile(
  optimizer = "adam",
  loss = "mse"
)

######################
# Train model
######################
history <- model %>% fit(
  x = list(X_seq, X_static),
  y = X_seq,
  epochs = 50,
  batch_size = 16,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(patience = 5, restore_best_weights = TRUE)
  )
)

######################
# Per-dive anomaly score
######################
X_hat <- model %>% predict(list(X_seq, X_static))

recon_error <- (X_seq - X_hat)^2

dive_anomaly_score <- apply(recon_error, 1, mean, na.rm = TRUE)

anomaly_df <- tibble(
  dive_id = dive_ids,
  anomaly_score = dive_anomaly_score
)

######################
# Time-localized anomaly signal
######################
time_anomaly <- apply(recon_error, c(1, 2), mean)

# Example: visualize dive i
plot(time_anomaly[5, ], type = "l",
     main = "Within-Dive Anomaly Signal",
     ylab = "Reconstruction Error",
     xlab = "Time")








