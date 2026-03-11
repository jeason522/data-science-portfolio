
######################## 底層代碼 ##############################################################

tool_function <- function( data , tool_list) {
  
  formula_str <- paste("OBSERVATION ~", paste(tool_list, collapse = " + "))
  model <- lm(as.formula(formula_str), data = data)
  
  print(summary(model))
  
  
  for (i in tool_list) {
    cat("\n機台：", i, "\n")
    
    total_lot <- data %>%
      group_by(LOT) %>%
      summarise(total_count = n())
    
    
    pass_lot <- data %>%
      filter(.data[[i]] == 1) %>%
      group_by(LOT) %>%
      summarise(pass_count = n())
    
    
    result <- left_join(total_lot, pass_lot, by = "LOT") %>%
      mutate(pass_count = ifelse(is.na(pass_count), 0, pass_count),
             cover = pass_count / total_count)
    
    print(result)
  }
}



reg_plot_function <- function(data, tool_list) {
  
  formula_str <- paste("OBSERVATION ~", paste(tool_list, collapse = " + "))
  model <- lm(as.formula(formula_str), data = data)
  
  pred <- predict(model, newdata = data, se.fit = TRUE)
  
  df_pred <- data %>%
    mutate(
      prediction = pred$fit,
      LOT_char = as.character(LOT)
    )
  

  lot_sd <- df_pred %>%
    group_by(LOT_char) %>%
    summarise(lot_sd = sd(OBSERVATION), .groups = "drop")
  
  
  df_pred <- df_pred %>%
    left_join(lot_sd, by = "LOT_char") %>%
    mutate(
      upper = prediction + 2 * lot_sd,
      lower = prediction - 2 * lot_sd,
      index = row_number() 
    )
  
  # 畫圖
  ggplot(df_pred, aes(x = index, y = OBSERVATION)) +
    geom_point(aes(color = LOT_char)) +
    geom_line(aes(y = prediction), color = "black", linewidth  = 1) +
    geom_line(aes(y = upper), color = "blue", linetype = "dashed") +
    geom_line(aes(y = lower), color = "blue", linetype = "dashed") +
    labs(title = "迴歸模型", x = "Wafer", y = "OBSERVATION") +
    theme_minimal()
}


plot_scatter_matrix <- function(data, tool_list) {
  
  df_long <- data %>%
    mutate(WaferIndex = row_number()) %>%
    pivot_longer(cols = all_of(tool_list), names_to = "Feature", values_to = "Pass") %>%
    mutate(
      Status = ifelse(Pass == 1, paste0("通過 ", Feature), paste0("未通過 ", Feature)),
      Status = ifelse(Pass == 1, "通過", "未通過")  
    )
  
  
  ggplot(df_long, aes(x = WaferIndex, y = OBSERVATION, color =Status)) +
    geom_point(size = 2) +
    facet_wrap(~ Feature, scales = "free") +
    scale_color_manual(values = c("通過" = "red", "未通過" = "blue")) +
    labs(title = "機台散佈矩陣", x = "Wafer", y = "OBSERVATION") +
    theme_minimal()
  
}

###########################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)


df <- read.csv('new_wafer_data.csv')

tool_list <- c('T0569',
                'T1579',
                'T1241',
                'T0536',
                'T1250',
                'T0873',
                'T1202',
                'T1309',
                'T0872',
                'T1787')

tool_function( df , tool_list)

reg_plot_function( df , tool_list)

plot_scatter_matrix(df, tool_list)




# ==============================================================
#                   XGBoost (梯度提升決策樹)
# ==============================================================

library(xgboost)
library(Matrix)

# =====================================
#              資料前處理
# =====================================

# 取出 OBSERVATION 與機台特徵 (機台欄位開頭都是 T)

df_model <- df %>%
  dplyr::select(OBSERVATION, starts_with("T"))


# 轉成 xgboost 格式

X <- as.matrix(df_model %>% dplyr::select(-OBSERVATION))
y <- df_model$OBSERVATION
dtrain <- xgb.DMatrix(data = X, label = y)

# =====================================
#           XGBoost 訓練模型
# =====================================

# 設定參數
param <- list(
  objective = "reg:squarederror",  # 這邊是回歸任務
  max_depth = 5,
  eta = 0.3
)

# 訓練模型

xgb_model <- xgb.train(
  params = param,
  data = dtrain,
  nrounds = 500
)

xgb_importance <- xgb.importance(model = xgb_model)
head(xgb_importance, 20)

# =========================================
#       依序畫出每個重要機台的散佈圖
# =========================================

top_n <- 10

important_features <- xgb_importance$Feature[1:top_n]
xgb.plot.importance(xgb_importance, top_n = top_n)

plot_scatter_matrix(df, important_features)


# ==========================================
#                   Twohit
# ==========================================

library(MASS)
source("Twohit.R")

X <- as.matrix( df[, 4:ncol(df)])
y <- as.matrix(df[,3])


result <- Twohit(X, y)
result


reg_plot_function(df_new ,  c('T0404' , 'T1370' ,'T2395' , 'T2455'))
tool_function(df_new ,  c('T0404' , 'T1370' ,'T2395' , 'T2455'))



X <- as.matrix( df[, 4:ncol(df)])
y <- as.matrix(df[,3])

result <- Ohit2011(X, y)
result$OGA


