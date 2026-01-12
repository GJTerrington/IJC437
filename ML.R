# setup

set.seed(123)

ml_data$genre_group <- as.factor(ml_data$genre_group)
ml_data$artist_type <- as.factor(ml_data$artist_type)
ml_data$decade <- as.factor(ml_data$decade)

# inspect how unbalanced classes are

table(ml_data$popularity_binary)
prop.table(table(ml_data$popularity_binary))

# make train/test split 80/20

ml_data <- ml_data[sample(1:nrow(ml_data)), ]

train_size <- 0.8
train_rows <- 1:(train_size * nrow(ml_data))

train_data <- ml_data[train_rows, ]
test_data  <- ml_data[-train_rows, ]

# -------------------------------RQ2 FULL MODEL---------------------------------

train_data$popularity_binary <- as.factor(as.character(train_data$popularity_binary))
test_data$popularity_binary  <- as.factor(as.character(test_data$popularity_binary))

# logistic regression
log_model_full <- glm(
  popularity_binary ~ genre_group + artist_popularity + artist_pop_missing +
    artist_type + explicit + duration_ms + key + mode + time_signature +
    acousticness + danceability + energy + instrumentalness +
    liveness + loudness + speechiness + valence + tempo,
  family = binomial(link = "logit"),
  data = train_data
)

log_probs_full <- predict(
  log_model_full,
  newdata = test_data,
  type = "response"
)

log_preds_full <- ifelse(log_probs_full > 0.5, 1, 0)

# accuracy
log_error_full <- mean(log_preds_full != test_data$popularity_binary)
paste("Logistic Regression Accuracy (Full):", 1 - log_error_full)

# confusion matrix
confusion_log_full <- table(
  Actual = test_data$popularity_binary,
  Predicted = log_preds_full
)
confusion_log_full

# roc auc
roc_log_full <- roc(test_data$popularity_binary, log_probs_full)
auc(roc_log_full)

# recall
recall_popular <- confusion_log_full["1", "1"] / sum(confusion_log_full["1", ])
recall_popular

paste("Recall (Popular Songs):", round(recall_popular, 3))

# precision
precision_popular <- confusion_log_full["1", "1"] / sum(confusion_log_full[, "1"])
precision_popular

# f1 score
f1_popular <- 2 * (precision_popular * recall_popular) /
  (precision_popular + recall_popular)
f1_popular

# random forest (balanced trees)
rf_full <- randomForest(
  popularity_binary ~ genre_group + artist_popularity + artist_pop_missing +
    artist_type + explicit + duration_ms + key + mode + time_signature +
    acousticness + danceability + energy + instrumentalness +
    liveness + loudness + speechiness + valence + tempo,
  data = train_data,
  ntree = 500,
  sampsize = rep(min(table(train_data$popularity_binary)), 2),
  importance = TRUE
)


rf_probs_full <- predict(rf_full, test_data, type = "prob")[, 2]
rf_preds_full <- ifelse(rf_probs_full > 0.5, 1, 0)

# accuracy
rf_error_full <- mean(rf_preds_full != test_data$popularity_binary)
paste("Random Forest Accuracy (Full):", 1 - rf_error_full)

# confusion matrix
confusion_rf_full <- table(
  Actual = test_data$popularity_binary,
  Predicted = rf_preds_full
)
confusion_rf_full

# roc auc
roc_rf_full <- roc(test_data$popularity_binary, rf_probs_full)
auc(roc_rf_full)

# recall
recall_popular_rf <- confusion_rf_full["1", "1"] / sum(confusion_rf_full["1", ])
recall_popular_rf

paste("Recall (Popular Songs):", round(recall_popular_rf, 3))

# precision
precision_popular_rf <- confusion_rf_full["1", "1"] / sum(confusion_rf_full[, "1"])
precision_popular_rf

# f1 score
f1_popular_rf <- 2 * (precision_popular_rf * recall_popular_rf) /
  (precision_popular_rf + recall_popular_rf)
f1_popular_rf

# random forest (unbalanced)
rf_full_unbalanced <- randomForest(
  popularity_binary ~ genre + artist_popularity + artist_pop_missing +
    artist_type + explicit + duration_ms + key + mode + time_signature +
    acousticness + danceability + energy + instrumentalness +
    liveness + loudness + speechiness + valence + tempo,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

rf_probs_full_un <- predict(rf_full_unbalanced, test_data, type = "prob")[, 2]
rf_preds_full_un <- ifelse(rf_probs_full_un > 0.5, 1, 0)

# accuracy
rf_error_full_un <- mean(rf_preds_full_un != test_data$popularity_binary)
paste("Random Forest Accuracy (Full):", 1 - rf_error_full_un)

# confusion matrix - we see here that the model does not predict any popular songs
# therefore the model is completely ineffective and further evaluation cannot be done effectively
confusion_rf_full_un <- table(
  Actual = test_data$popularity_binary,
  Predicted = rf_preds_full_un
)
confusion_rf_full_un
 
#------------------------ Acoustic features only -------------------------------

# logistic regression
log_model_acoustic <- glm(
  popularity_binary ~ acousticness + danceability + energy +
    instrumentalness + liveness + loudness +
    speechiness + valence + tempo,
  family = binomial(link = "logit"),
  data = train_data
)

log_probs_acoustic <- predict(
  log_model_acoustic,
  newdata = test_data,
  type = "response"
)

log_preds_acoustic <- ifelse(log_probs_acoustic > 0.5, 1, 0)

# accuracy
log_error_acoustic <- mean(log_preds_acoustic != test_data$popularity_binary)
paste("Logistic Regression Accuracy (Acoustic):", 1 - log_error_acoustic)

# confusion matrix
confusion_log_acoustic <- table(
  Actual = test_data$popularity_binary,
  Predicted = log_preds_acoustic
)
confusion_log_acoustic

# random forest (unbalanced)
rf_acoustic <- randomForest(
  popularity_binary ~ acousticness + danceability + energy +
    instrumentalness + liveness + loudness +
    speechiness + valence + tempo,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

rf_probs_acoustic <- predict(rf_acoustic, test_data, type = "prob")[, 2]
rf_preds_acoustic <- ifelse(rf_probs_acoustic > 0.5, 1, 0)

# accuracy
rf_error_acoustic <- mean(rf_preds_acoustic != test_data$popularity_binary)
paste("Random Forest Accuracy (Acoustic):", 1 - rf_error_acoustic)

# confusion matrix - again, we see here that the model does not predict any popular songs
# therefore the model is completely ineffective and further evaluation cannot be done effectively
confusion_rf_acoustic <- table(
  Actual = test_data$popularity_binary,
  Predicted = rf_preds_acoustic
)
confusion_rf_acoustic

# random forest (balanced)
rf_acoustic_balanced <- randomForest(
  popularity_binary ~ acousticness + danceability + energy +
    instrumentalness + liveness + loudness +
    speechiness + valence + tempo,
  data = train_data,
  ntree = 500,
  sampsize = rep(min(table(train_data$popularity_binary)), 2),
  importance = TRUE
)

rf_probs_balanced <- predict(rf_acoustic_balanced, test_data, type = "prob")[, 2]
rf_preds_balanced <- ifelse(rf_probs_balanced > 0.5, 1, 0)

# accuracy
rf_error_balanced <- mean(rf_preds_balanced != test_data$popularity_binary)
paste("Random Forest Accuracy (Acoustic):", 1 - rf_error_balanced)

# roc auc
roc_rf_balanced <- roc(test_data$popularity_binary, rf_probs_balanced)
auc(roc_rf_balanced)

# confusion matrix
confusion_rf_balanced <- table(
  Actual = test_data$popularity_binary,
  Predicted = rf_preds_balanced
)
confusion_rf_balanced

# recall
recall_rf_balanced <- confusion_rf_balanced["1", "1"] / sum(confusion_rf_balanced["1", ])
recall_rf_balanced

paste("Recall:", round(recall_rf_balanced, 3))

# precision
precision_rf_balanced <- confusion_rf_balanced["1", "1"] / sum(confusion_rf_balanced[, "1"])
precision_rf_balanced

# f1 score
f1_rf_balanced <- 2 * (precision_rf_balanced * recall_rf_balanced) /
  (precision_rf_balanced + recall_rf_balanced)
f1_rf_balanced



