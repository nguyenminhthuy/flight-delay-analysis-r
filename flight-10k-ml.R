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
library(vip)
library(pdp)
library(iml)
library(Metrics)
library(forcats)
library(tidyverse)

df_flights <- fread("data/flights_sample_10k.csv")

##########################################
# Phân tích dự đoán (Predictive / Machine Learning)
##########################################
#----------------------------------------#
# 1. Phân loại (Classification)
# 1.1 Xây dựng mô hình dự đoán chuyến bay có delay >15 phút (0/1)
#----------------------------------------#
df <- copy(df_flights)

df <- df |>
  mutate(
    FL_DATE = as.IDate(FL_DATE),
    YEAR = as.integer(format(FL_DATE, "%Y")),
    MONTH = as.integer(format(FL_DATE, "%m")),
    DEP_HH = DEP_TIME %/% 100,
    CRS_DEP_HH = CRS_DEP_TIME %/% 100,
    IS_DELAYED = as.integer(DEP_DELAY > 15)
  ) |> 
  filter(!is.na(IS_DELAYED))

cols <- c('MONTH', 'AIRLINE', 'ORIGIN', 'DEST', 'DEP_HH', 'DISTANCE', 'CRS_DEP_HH', 'IS_DELAYED')
df <- df |> select(all_of(cols))

#-----------------------------------------
# Xử lý missing
#-----------------------------------------
numeric_cols <- c('MONTH','DEP_HH','DISTANCE','CRS_DEP_HH')
categorical_cols <- c('AIRLINE','ORIGIN','DEST')

df <- df |>
  mutate(across(all_of(numeric_cols), 
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) |>
  mutate(across(all_of(categorical_cols), 
                ~ ifelse(is.na(.x), names(sort(table(.x), decreasing = TRUE))[1], .x)))

# Gộp các level hiếm cho categorical
df <- df |>
  mutate(across(all_of(categorical_cols), 
                ~ fct_lump_prop(.x, 0.01, other_level = "Other")))

#-----------------------------------------
# One-hot encoding
#-----------------------------------------
df <- fastDummies::dummy_cols(df, remove_first_dummy = TRUE, 
                              remove_selected_columns = TRUE)

# Đổi tên cột: thay ký tự không hợp lệ thành "_"
colnames(df) <- str_replace_all(colnames(df), "[^0-9A-Za-z_]", "_")

#-----------------------------------------
# Split train/test
#-----------------------------------------
set.seed(42)
train_index <- createDataPartition(df$IS_DELAYED, p = 0.8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# RF/XGBoost target là factor
train$IS_DELAYED <- factor(train$IS_DELAYED, levels = c(0,1))
test$IS_DELAYED <- factor(test$IS_DELAYED, levels = c(0,1))

#-----------------------------------------
# metrics and plots function
#-----------------------------------------
metrics_calculator <- function(y_true, y_pred_class, y_pred_prob, model_name = "Model") {
  
  y_true <- as.numeric(as.character(y_true))
  valid_index <- which(!is.na(y_true) & !is.na(y_pred_prob))
  y_true_clean <- y_true[valid_index]
  y_pred_prob_clean <- y_pred_prob[valid_index]
  y_pred_class_clean <- y_pred_class[valid_index]
  
  # confusion matrix, roc-auc, pr-auc
  conf_mat <- confusionMatrix(factor(y_pred_class_clean), factor(y_true_clean))
  roc_obj <- pROC::roc(
    response = y_true_clean,
    predictor = y_pred_prob_clean)
  roc_auc <- pROC::auc(roc_obj) 
  
  pr_obj <- pr.curve(scores.class0 = y_pred_prob_clean,
                     weights.class0 = y_true_clean,
                     curve = FALSE)
  pr_auc <- pr_obj$auc.integral
  
  # Precision, Recall, F1
  precision <- posPredValue(factor(y_pred_class_clean), factor(y_true_clean), positive = "1")
  recall <- sensitivity(factor(y_pred_class_clean), factor(y_true_clean), positive = "1")
  f1 <- 2 * precision * recall / (precision + recall)
  accuracy <- conf_mat$overall['Accuracy']
  
  metrics <- data.frame(ROC_AUC = roc_auc, PR_AUC = pr_auc, 
                        Accuracy = accuracy, Precision = precision, 
                        Recall = recall, F1_score = f1)
  
  cat("\n===== Metrics for", model_name, "=====\n")
  print(conf_mat)
  print(metrics)
  
  return(metrics)
}

metrics_plots <- function(y_true, y_pred_class, y_pred_prob){
  
  y_true <- as.numeric(as.character(y_true))
  valid_index <- which(!is.na(y_true) & !is.na(y_pred_prob))
  y_true <- y_true[valid_index]
  y_pred_class <- y_pred_class[valid_index]
  y_pred_prob <- y_pred_prob[valid_index]
  
  # Confusion Matrix
  cm <- table(Predicted = y_pred_class, Actual = y_true)
  cm_df <- as.data.frame(cm)
  p1 <- ggplot(cm_df, aes(Predicted, Actual, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "black", size = 6, fontface = "bold") +
    #scale_fill_gradient(low = "#d1c4e9", high = "#512da8") +
    scale_fill_distiller(palette = "RdPu", 
                         name = "Frequency") +
    labs(title = "Confusion Matrix") +
    theme_minimal()
  
  # Precision-Recall
  pr_obj <- pr.curve(scores.class0 = y_pred_prob,
                     weights.class0 = y_true,
                     curve = TRUE)
  pr_df <- data.frame(recall = pr_obj$curve[,1],
                      precision = pr_obj$curve[,2])
  
  p2 <- ggplot(pr_df, aes(recall, precision)) +
    geom_line(size = 0.75, color = "#8e24aa") +
    labs(title = "Precision–Recall Curve", x = "Recall", y = "Precision") +
    theme_minimal()
  
  # ROC Curve
  roc_obj <- roc(y_true, y_pred_prob)
  roc_df <- data.frame(fpr = 1 - roc_obj$specificities,
                       tpr = roc_obj$sensitivities)
  p3 <- ggplot(roc_df, aes(fpr, tpr)) +
    geom_line(size = 0.75, color = "#5e35b1") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal()
  
  # Show all
  grid.arrange(p1, p2, p3, ncol = 3)
}

#-----------------------------------------
# Logistic Regression
#-----------------------------------------
lr_model <- glm(IS_DELAYED ~ ., data = train, family = binomial)
lr_pred_prob <- predict(lr_model, test, type = "response")
lr_pred_class <- ifelse(lr_pred_prob > 0.3, 1, 0)

# Metrics
metrics_calculator(test$IS_DELAYED, lr_pred_class, lr_pred_prob, "Logistic Regression")
metrics_plots(test$IS_DELAYED, lr_pred_class, lr_pred_prob)

#-----------------------------------------
# Random Forest
#-----------------------------------------
rf_model <- randomForest(IS_DELAYED ~ ., data = train, ntree = 100)
rf_pred_prob <- predict(rf_model, test, type = "prob")[,2]
rf_pred_class <- ifelse(rf_pred_prob > 0.3, 1, 0)

# Metrics
metrics_calculator(test$IS_DELAYED, rf_pred_class, rf_pred_prob, "Random Forest")
metrics_plots(test$IS_DELAYED, rf_pred_class, rf_pred_prob)

#----------------------------------------#
# XGBoost - nhận matrix
#----------------------------------------#
train_matrix <- xgb.DMatrix(data=as.matrix(train |> select(-IS_DELAYED)), 
                            label=as.numeric(as.character(train$IS_DELAYED)))
test_matrix <- xgb.DMatrix(data=as.matrix(test |> select(-IS_DELAYED)), 
                           label=as.numeric(as.character(test$IS_DELAYED)))

xgb_model <- xgb.train(
  params=list(objective="binary:logistic", eval_metric="auc"),
  data=train_matrix,
  nrounds=100,
  verbose=0
)

xgb_pred_prob <- predict(xgb_model, test_matrix)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.3, 1, 0)

metrics_calculator(test$IS_DELAYED, xgb_pred_class, xgb_pred_prob, "XGBoost")
metrics_plots(test$IS_DELAYED, xgb_pred_class, xgb_pred_prob)

#----------------------------------------#
# Các đặc trưng nào (giờ khởi hành, hãng, sân bay, tháng, distance) ảnh hưởng mạnh nhất đến khả năng delay?
#----------------------------------------#
# Method 1: Using Built-in Functions for Specific Models
# View the feature importance
rf_imp <- importance(rf_model)
rf_imp

xgb_imp <- xgb.importance(
  feature_names = colnames(train_matrix),  # colnames numeric matrix train
  model = xgb_model
)
xgb_imp

# Plot feature importance using the vip package
vip(rf_model)
xgb.plot.importance(xgb_imp)

#----------------------------------------#
# Method 2: Using Permutation Importance
# Extract and plot variable importance
var_importance <- varImp(rf_model)
plot(var_importance)

#----------------------------------------#
# Method 3: SHAP Values
X_train <- train |> select(-IS_DELAYED)
y_train <- as.numeric(as.character(train$IS_DELAYED))

predictor <- Predictor$new(
  model = rf_model, 
  data = X_train, 
  y = y_train, 
  type = "prob"   # vì là classification, trả về xác suất
)

x_interest <- X_train[1, ]

shap <- Shapley$new(predictor, x.interest = x_interest)
shap_df <- shap$results
shap_df <- shap_df |>
  mutate(abs_phi = abs(phi)) |> # phi = SHAP value
  arrange(desc(abs_phi)) |>
  top_n(20)

ggplot(shap_df, aes(x=reorder(feature, abs_phi), y=abs_phi, fill=phi)) +
  geom_col() +
  coord_flip() +
  labs(title="Top 10 SHAP Features (1 observation)",
       x="Feature", y="Absolute SHAP Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  theme_minimal()

#----------------------------------------#
# Có thể dự đoán chuyến bay bị hủy dựa trên đặc trưng thời gian và sân bay không?
#----------------------------------------#
df_1 <- copy(df_flights)

df_1 <- df_1 |>
  mutate(
    FL_DATE = as.IDate(FL_DATE),
    YEAR = as.integer(format(FL_DATE, "%Y")),
    MONTH = as.integer(format(FL_DATE, "%m")),
    DEP_HH = DEP_TIME %/% 100,
    CRS_DEP_HH = CRS_DEP_TIME %/% 100,
    IS_CANCELLED = as.integer(CANCELLED ==1)
  ) |> 
  filter(!is.na(IS_CANCELLED))

cols_1 <- c('MONTH', 'ORIGIN', 'DEST', 'CRS_DEP_HH', 'IS_CANCELLED')

df_1 <- df_1 |> select(all_of(cols_1))

numeric_cols_1 <- c('CRS_DEP_HH')
categorical_cols_1 <- c('MONTH','ORIGIN','DEST')

df_1 <- df_1 |>
  mutate(across(all_of(numeric_cols_1), 
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) |>
  mutate(across(all_of(categorical_cols_1), 
                ~ ifelse(is.na(.x), names(sort(table(.x), decreasing = TRUE))[1], .x)))

# Gộp các level hiếm cho categorical
df_1 <- df_1 |>
  mutate(
    MONTH = as.factor(MONTH),
    ORIGIN = as.factor(ORIGIN),
    DEST = as.factor(DEST)
  ) |>
  mutate(across(all_of(categorical_cols_1), 
                ~ fct_lump_prop(.x, 0.01, other_level = "Other")))

df_1 <- fastDummies::dummy_cols(df_1, remove_first_dummy = TRUE, 
                              remove_selected_columns = TRUE)

# Đổi tên cột: thay ký tự không hợp lệ thành "_"
colnames(df_1) <- str_replace_all(colnames(df_1), "[^0-9A-Za-z_]", "_")


train_index_1 <- createDataPartition(df_1$IS_CANCELLED, p = 0.8, list = FALSE)
train_1 <- df_1[train_index_1, ]
test_1 <- df_1[-train_index_1, ]

#----------------------------------------#
lr_model_1 <- glm(IS_CANCELLED ~ ., data = train_1, family = binomial)
lr_pred_prob_1 <- predict(lr_model_1, test_1, type = "response")
lr_pred_class_1 <- ifelse(lr_pred_prob_1 > 0.3, 1, 0)

# Metrics
metrics_calculator(test_1$IS_CANCELLED, lr_pred_class_1, lr_pred_prob_1, "Logistic Regression")
metrics_plots(test_1$IS_CANCELLED, lr_pred_class_1, lr_pred_prob_1)

#----------------------------------------#
rf_model_1 <- randomForest(IS_CANCELLED ~ ., data = train_1, ntree = 100)
rf_pred_prob_1 <- predict(rf_model_1, test_1, type = "response")
rf_pred_class_1 <- ifelse(rf_pred_prob_1 > 0.3, 1, 0)

# Metrics
metrics_calculator(test_1$IS_CANCELLED, rf_pred_class_1, rf_pred_prob_1, "Random Forest")
metrics_plots(test_1$IS_CANCELLED, rf_pred_class_1, rf_pred_prob_1)

#----------------------------------------#
# XGBoost - nhận matrix

train_matrix_1 <- xgb.DMatrix(data=as.matrix(train_1 |> select(-IS_CANCELLED)), 
                            label=as.numeric(as.character(train_1$IS_CANCELLED)))

test_matrix_1 <- xgb.DMatrix(data=as.matrix(test_1 |> select(-IS_CANCELLED)), 
                           label=as.numeric(as.character(test_1$IS_CANCELLED)))

xgb_model_1 <- xgb.train(
  params=list(objective="binary:logistic", eval_metric="auc"),
  data=train_matrix_1,
  nrounds=100,
  verbose=0
)

xgb_pred_prob_1 <- predict(xgb_model_1, test_matrix_1)
xgb_pred_class_1 <- ifelse(xgb_pred_prob_1 > 0.3, 1, 0)

metrics_calculator(test_1$IS_CANCELLED, xgb_pred_class_1, xgb_pred_prob_1, "XGBoost")
metrics_plots(test_1$IS_CANCELLED, xgb_pred_class_1, xgb_pred_prob_1)

#----------------------------------------#
# 1.2 Hồi quy (Regression)
#----------------------------------------#
# Dự đoán số phút delay (ARR_DELAY)
#----------------------------------------#













