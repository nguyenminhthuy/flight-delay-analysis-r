library(data.table)
library(dplyr)
library(caret)
library(pROC)
library(PRROC)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(randomForest)
library(xgboost)
library(stringr)

df <- fread("data/flights_sample_10k.csv")

##########################################
# Phân tích dự đoán (Predictive / Machine Learning)
##########################################
#----------------------------------------#
# 1. Phân loại (Classification)
# 1.1 Xây dựng mô hình dự đoán chuyến bay có delay >15 phút (0/1)
#----------------------------------------#
df <- df |>
  mutate(FL_DATE = as.IDate(FL_DATE),
         YEAR = format(FL_DATE, "%Y") |> as.integer(),
         MONTH = format(FL_DATE, "%m") |> as.integer(),
         TARGET = as.integer(DEP_DELAY > 15)) |>
  filter(!is.na(TARGET))

cols <- c('MONTH', 'AIRLINE', 'ORIGIN', 'DEST', 'DEP_TIME', 'DISTANCE', 'CRS_DEP_TIME', 'TARGET')
df <- df |> select(all_of(cols))

# Xử lý missing
numeric_cols <- c('MONTH','DEP_TIME','DISTANCE','CRS_DEP_TIME')
categorical_cols <- c('AIRLINE','ORIGIN','DEST')

df <- df |>
  mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) |>
  mutate(across(all_of(categorical_cols), ~ ifelse(is.na(.x), names(sort(table(.x), decreasing = TRUE))[1], .x)))

# Gộp các level hiếm cho categorical
df <- df |>
  mutate(across(all_of(categorical_cols), ~ fct_lump_prop(.x, 0.01, other_level = "Other")))

#-----------------------------------------
# One-hot encoding
df <- fastDummies::dummy_cols(df, remove_first_dummy = TRUE, 
                              remove_selected_columns = TRUE)

# Đổi tên cột: thay ký tự không hợp lệ thành "_"
# Đổi tên các cột sau one-hot:
colnames(df) <- str_replace_all(colnames(df), "[^0-9A-Za-z_]", "_")

# loại bỏ các dòng NA
non_na_index <- which(!is.na(y))
X <- X[non_na_index, ]
y <- y[non_na_index]

#-----------------------------------------
# Split train/test
set.seed(42)
train_index <- createDataPartition(df$TARGET, p = 0.8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# Chuyển target sang factor
train$TARGET <- factor(train$TARGET, levels = c(0,1))
test$TARGET <- factor(test$TARGET, levels = c(0,1))
#-----------------------------------------
metrics_calculator <- function(y_true, y_pred_class, y_pred_prob, model_name = "Model") {
  
  conf_mat <- confusionMatrix(factor(y_pred_class), factor(y_true))
  roc_auc <- auc(roc(y_true, y_pred_prob))
  pr_auc <- pr.curve(scores.class0 = y_pred_prob, weights.class0 = y_true, curve = FALSE)$auc.integral
  
  precision <- posPredValue(factor(y_pred_class), factor(y_true), positive = "1")
  recall <- sensitivity(factor(y_pred_class), factor(y_true), positive = "1")
  f1 <- 2 * precision * recall / (precision + recall)
  accuracy <- conf_mat$overall['Accuracy']
  
  metrics <- data.frame(
    ROC_AUC = roc_auc,
    PR_AUC = pr_auc,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_score = f1
  )
  
  cat("\n===== Metrics for", model_name, "=====\n")
  print(conf_mat)
  print(metrics)
  
  return(metrics)
}

metrics_plots <- function(y_true, y_pred_class, y_pred_prob){
  # Confusion Matrix
  cm <- table(Predicted = y_pred_class, 
              Actual = y_true)
  cm_df <- as.data.frame(cm)
  
  p1 <- ggplot(cm_df, aes(Predicted, Actual, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 6, fontface = "bold") +
    scale_fill_gradient(low = "#d1c4e9", high = "#512da8") +
    labs(title = "Confusion Matrix") +
    theme_minimal()
  
  # ROC Curve
  roc_obj <- roc(y_true, y_pred_prob)
  roc_df <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )
  
  p2 <- ggplot(roc_df, aes(fpr, tpr)) +
    geom_line(size = 0.75, color = "#5e35b1") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal()
  
  # Precision - Recall
  pr_obj <- pr.curve(
    scores.class0 = y_pred_prob,
    weights.class0 = y_true,
    curve = TRUE
  )
  
  pr_df <- data.frame(
    recall = pr_obj$curve[, 1],
    precision = pr_obj$curve[, 2]
  )
  
  p3 <- ggplot(pr_df, aes(recall, precision)) +
    geom_line(size = 0.75, color = "#8e24aa") +
    labs(title = "Precision–Recall Curve", 
         x = "Recall",
         y = "Precision") +
    theme_minimal()
  
  # ===== SHOW ALL =====
  grid.arrange(p1, p2, p3, ncol = 3)
}

#-----------------------------------------
# Logistic Regression
lr_model <- glm(TARGET ~ ., data = train, family = binomial)

# Dự đoán
lr_pred_prob <- predict(lr_model, test, type = "response")
lr_pred_class <- ifelse(lr_pred_prob > 0.3, 1, 0)

# Metrics
metrics_calculator(y_test, lr_pred_class, lr_pred_prob, "Logistic Regression")
metrics_plots(y_test, lr_pred_class, lr_pred_prob)

#-----------------------------------------
# Random Forest
rf_model <- randomForest(TARGET ~ ., data = train, ntree = 100)
rf_pred_prob <- predict(rf_model, test, type = "prob")[,2]
rf_pred_class <- ifelse(rf_pred_prob > 0.3, 1, 0)

# Metrics
metrics_calculator(y_test, rf_pred_class, rf_pred_prob, "Random Forest")
metrics_plots(y_test, rf_pred_class, rf_pred_prob)

#----------------------------------------#
# 1.2 
#----------------------------------------#










