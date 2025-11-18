library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(scales) # scale_y_continuous(labels = comma)
library(hexbin) # Hexbin plot (nhẹ hơn rất nhiề so vs scatter)
library(xgboost)
library(caret)
library(fastDummies)

# faster than read_csv
df = fread("data/flights_sample_10k.csv", header=TRUE)

df <- df |>
  mutate(
    TARGET = as.integer(df$DEP_DELAY > 15, na.rm=TRUE),
    FL_DATE = as.Date(FL_DATE),
    YEAR = year(FL_DATE),
    MONTH = month(FL_DATE)
  ) |>
  filter(!is.na(TARGET))

########################################
# 1. Phân loại (Classification)
########################################
#---------------------------------------
# 1.1 Xây dựng mô hình dự đoán chuyến bay có delay >15 phút (0/1)
#---------------------------------------
X <- df %>% 
  select(MONTH, AIRLINE, ORIGIN, DEST, DEP_TIME, DISTANCE, CRS_DEP_TIME)

y <- df$TARGET

# --- Tách cột numeric & categorical ---
numeric_features <- c("DEP_TIME", "DISTANCE", "CRS_DEP_TIME", "MONTH")
categorical_features <- c("AIRLINE", "ORIGIN", "DEST")

# --- Xử lý NA ---
# Numeric -> median
for (col in numeric_features) {
  X[[col]][is.na(X[[col]])] <- median(X[[col]])
}

# Categorical -> mode (giá trị thường gặp nhất)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (col in categorical_features) {
  mode_val <- get_mode(X[[col]])
  X[[col]][is.na(X[[col]])] <- mode_val
}

# --- One-hot encoding ---
X <- fastDummies::dummy_cols(X, remove_first_dummy = TRUE, 
                             remove_selected_columns = TRUE)

# --- Train/Test Split (80/20) ---
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[train_index, ]
X_test  <- X[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]

# --- Check shapes ---
cat("X_train dim: ", dim(X_train), "\n")
cat("X_test dim:  ", dim(X_test), "\n")
cat("y_train length:", length(y_train), "\n")
cat("y_test length:",  length(y_test), "\n")

metrics_calculators <- function(y_test, pred, proba, model_name="Model"){
  
}






