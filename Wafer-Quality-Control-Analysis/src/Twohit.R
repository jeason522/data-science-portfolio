Ohit2011 = function(X, y, Kn = NULL, c1 = 5, HDIC_Type = "HDHQ", const2 = seq(0.5, 2, 0.5), const3 = seq(0.2, 1, 0.2)){
  
  n = nrow(X)
  p = ncol(X)
  K = ifelse(is.null(Kn), max(1, min(floor(c1 * sqrt(n/log(p))), p)), Kn)
  
  dy = y - mean(y)
  dX = apply(X, 2, function(x) x - mean(x))
  Jhat = sigma2hat = rep(0, K)
  XJhat = matrix(0, n, K)
  u = as.matrix(dy)
  xnorms = sqrt(colSums((dX)^2))
  aSSE = (abs(t(u) %*% dX)/xnorms)
  Jhat[1] = which.max(aSSE)
  XJhat[, 1] = (dX[, Jhat[1]]/sqrt(sum((dX[, Jhat[1]])^2)))
  u = u - XJhat[, 1] %*% t(XJhat[, 1]) %*% u
  sigma2hat[1] = mean(u^2)
  if(K > 1){
    for(k in 2:K){
      aSSE = (abs(t(u) %*% dX)/xnorms)
      aSSE[Jhat[1:(k - 1)]] = 0
      Jhat[k] = which.max(aSSE)
      rq = dX[, Jhat[k]] - XJhat[, 1:(k - 1)] %*% t(XJhat[, 1:(k - 1)]) %*% dX[, Jhat[k]]
      XJhat[, k] = (rq/sqrt(sum((rq)^2)))
      u = u - XJhat[, k] %*% t(XJhat[, k]) %*% u
      sigma2hat[k] = mean(u^2)
    }
  }

  if(HDIC_Type == "HDAIC"){penalty = 1}
  if(HDIC_Type == "HDBIC"){penalty = log(n)}
  if(HDIC_Type == "HDHQ"){penalty = log(log(n))} 

  kn_hat_now = 0
  result_all = NULL

  for(c2 in const2){
    omega_n = c2 * penalty
    hdic = (n * log(sigma2hat)) + ((1:K) * omega_n * (log(p)))
    kn_hat = which.min(hdic)

    if(kn_hat_now != kn_hat){     
      for(c3 in const3){
        benchmark = (n * log(sigma2hat[kn_hat])) + (kn_hat * c3 * penalty * (log(p)))
        J_HDIC = sort(Jhat[1:kn_hat])
        J_Trim = Jhat[1:kn_hat]
        trim_pos = rep(0, kn_hat)
        if(kn_hat > 1){
          for(l in 1:kn_hat){
            JDrop1 = J_Trim[-l]
            fit = lm(dy ~ . - 1, data = data.frame(dX[, JDrop1]))
            uDrop1 = fit$residuals
            HDICDrop1 = (n * log(mean(uDrop1^2))) + ((kn_hat - 1) * c3 * penalty * (log(p)))
            if(HDICDrop1 > benchmark){
              trim_pos[l] = 1
            }
          }
          J_Trim = J_Trim[which(trim_pos == 1)]
        }

        result = c(sort(J_Trim), rep(0, K - length(J_Trim)))
        result_all = rbind(result_all, result)
      }
      kn_hat_now = kn_hat
    }
  }

  return(list("OGA" = Jhat, "Trim" = result_all))
}




Twohit = function(X, y, Kn = NULL, c1 = 5, HDIC_Type = "HDHQ", const2 = seq(0.5, 2, 0.5), const3 = seq(0.2, 1, 0.2)){
  
  n = nrow(X)
  p = ncol(X)
  benchmark = Inf

  vs_reg = Ohit2011(X, y, Kn, c1, HDIC_Type, const2, const3)
  variable_all_reg = vs_reg$Trim
  num_all_reg = nrow(variable_all_reg)

  for(i in 1:num_all_reg){
    variable_reg = variable_all_reg[i, which(variable_all_reg[i, ] != 0)]
    nowX_reg = cbind(rep(1, n), X[, variable_reg])

    u = lm(y ~ nowX_reg-1)$residuals
    pos = which(u^2 < (10^(-8)))
    if(length(pos)>0){
      u[pos] = sign(u[pos]) * (10^(-4))
    }
    newU = log(u^2)

    vs_dis = Ohit2011(X, newU, Kn, c1, HDIC_Type, const2, const3)
    variable_all_dis = vs_dis$Trim
    num_all_dis = nrow(variable_all_dis)

    for(j in 1:num_all_dis){
      variable_dis = variable_all_dis[j, which(variable_all_dis[j, ] != 0)]
      nowX_dis = cbind(rep(1, n), X[, variable_dis])

      if(length(variable_reg) == 0 & length(variable_dis) == 0){
        L = function(theta){
              A = rep(1, n)
              B = rep(1, n)
              sum(B * theta[2]) + sum((y - A * theta[1])^2 / exp(B * theta[2]))
            }
        initial_estimate = c(mean(y), mean(newU))
      }
      if(length(variable_reg) > 0 & length(variable_dis) == 0){
        L = function(theta){
              A <- nowX_reg
              B <- rep(1, n)
              sum(B * theta[(length(variable_reg)+2)]) + sum((y - A %*% theta[1:(length(variable_reg)+1)])^2 / exp(B * theta[(length(variable_reg)+2)]))
            } 
        initial_estimate = c(as.vector(lm(y ~ nowX_reg-1)$coefficients), mean(newU))   
      }
      if(length(variable_reg) == 0 & length(variable_dis) > 0){
        L = function(theta){
              A <- rep(1, n)
              B <- nowX_dis
              sum(B %*% theta[2:length(theta)]) + sum((y - A * theta[1])^2 / exp(B %*% theta[2:length(theta)]))
            }
        initial_estimate = c(mean(y), as.vector(lm(newU ~ nowX_dis-1)$coefficients))
      }
      if(length(variable_reg) > 0 & length(variable_dis) > 0){
        L = function(theta){
              A <- nowX_reg
              B <- nowX_dis
              sum(B %*% theta[(length(variable_reg)+2):length(theta)]) + sum((y - A %*% theta[1:(length(variable_reg)+1)])^2 / exp(B %*% theta[(length(variable_reg)+2):length(theta)]))
            }
        initial_estimate = c(as.vector(lm(y ~ nowX_reg-1)$coefficients), as.vector(lm(newU ~ nowX_dis-1)$coefficients))
      }

      paraEst = try(optim(initial_estimate, L, method = "L-BFGS-B"), silent = TRUE)
      if('try-error' %in% class(paraEst)){
        next
      }else{
        hdicNow = L(paraEst$par) + ((length(variable_reg) + length(variable_dis)) * log(2*p) * log(log(n)) * 2)
 
        if(hdicNow < benchmark){
          OGA_reg = vs_reg$OGA
          Trim_reg = variable_reg
          OGA_dis = vs_dis$OGA
          Trim_dis = variable_dis
          Beta = paraEst$par[1:(length(variable_reg)+1)]
          Alpha = paraEst$par[(length(variable_reg)+2):(length(variable_reg)+length(variable_dis)+2)]

          benchmark = hdicNow
        }
      }     
    } 
  }
  return(list("Trim_reg" = Trim_reg, "Trim_dis" = Trim_dis, "Beta" = Beta, "Alpha" = Alpha))
}


df <- read.csv("C:/Users/jason/Desktop/DATA/new_wafer_data.csv")
X <- as.matrix(df[, 5:ncol(df)])
y <- as.matrix(df[, 4])

Twohit(X, y)





install.packages("glmnet")
library(glmnet)

df_test <- read.csv("C:/Users/jason/Desktop/DATA/new_wafer_data.csv")
# 1. 準備 X 與 y
X_lasso <- as.matrix(df_test[, 5:ncol(df_test)])    
y_lasso <- df_test$OBSERVATION                 

# 2. cross‐validated Lasso：自動幫你挑最好的 λ
#    alpha = 1 表示 Lasso (L1 懲罰)
cvfit <- cv.glmnet(
  x     = X_lasso,
  y     = y_lasso,
  alpha = 1,
  nfolds = 10        # 10 折交叉驗證
)

# 3. 畫出 CV 曲線，檢視 MSE 隨 λ 變化
plot(cvfit)

# 4. 取兩個常用的 λ：
#    λ_min：讓 CV MSE 最小的 λ
#    λ_1se：在 CV MSE+1SE 內最大的 λ（更精簡的模型）
lambda_min <- cvfit$lambda.min
lambda_1se <- cvfit$lambda.1se

# 5. 看被選進模型的變數係數（非零係數）
coef_min <- coef(cvfit, s = "lambda.min")
coef_1se <- coef(cvfit, s = "lambda.1se")

# 只篩出非零的那幾行（去掉截距項）
sel_min <- which(coef_min[-1, ] != 0)
sel_1se <- which(coef_1se[-1, ] != 0)

vars_min <- rownames(coef_min)[-1][sel_min]
vars_1se <- rownames(coef_1se)[-1][sel_1se]

# 列出前幾個被 Lasso 選到的重要工具
print(paste("λ_min 選到的變數：", paste(vars_min, collapse = ", ")))
print(paste("λ_1se 選到的變數：", paste(vars_1se, collapse = ", ")))

# ======================================

# ── 套件載入 ───────────────────────────────────────────────
# 若尚未安裝請先執行：install.packages(c("data.table", "glmnet"))
library(data.table)   # 讀取與處理資料
library(glmnet)       # Lasso / Ridge / Elastic Net

# ── 1. 讀取資料 ────────────────────────────────────────────


# ── 2. 準備 y (目標變數) ───────────────────────────────────
y <- df$OBSERVATION

# ── 3. 建立 X 設計矩陣 ─────────────────────────────────────
# (a) 取原來的數值型特徵（假設從第 5 欄開始都是數值機台指標）
numeric_X <- as.matrix(df[, 5:ncol(df)])

# (b) 將 LOT 做 one-hot encoding，並移除截距 (-1)
lot_dummies <- model.matrix(~ LOT - 1, data = df)  # 例：LOT1 → LOTLOT1

# (c) 合併為最終設計矩陣
X <- cbind(numeric_X, lot_dummies)

# ── 4. 執行 10 折交叉驗證的 Lasso ──────────────────────────
set.seed(123)  # 固定隨機種子以利重現
cvfit <- cv.glmnet(
  x       = X,
  y       = y,
  alpha   = 1,        # L1 懲罰 → Lasso
  nfolds  = 10,
  family  = "gaussian",
  standardize = TRUE  # 建議標準化
)

# (可選) 繪製 CV 曲線
plot(cvfit)

# ── 5. 取得常用的兩個 λ 值 ────────────────────────────────
lambda_min <- cvfit$lambda.min   # 最小 CV MSE 的 λ
lambda_1se <- cvfit$lambda.1se   # 在最小 CV MSE＋1SE 內最大的 λ

# ── 6. 擷取非零係數 (即被 Lasso 留下的變數) ───────────────
get_nonzero_vars <- function(cv_obj, lambda_val) {
  coefs <- coef(cv_obj, s = lambda_val)
  nz_idx <- which(coefs[-1, ] != 0)           # 去掉截距後找非零
  rownames(coefs)[-1][nz_idx]
}

vars_min  <- get_nonzero_vars(cvfit, lambda_min)
vars_1se  <- get_nonzero_vars(cvfit, lambda_1se)

# ── 7. 列印結果 ───────────────────────────────────────────
cat("λ_min 選到的變數：", paste(vars_min,  collapse = ", "), "\n")
cat("λ_1se 選到的變數：", paste(vars_1se, collapse = ", "), "\n")
