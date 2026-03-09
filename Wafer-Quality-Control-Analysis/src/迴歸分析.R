df = read.csv('new_wafer_data.csv')

df$LOT = as.factor(df[[1]])
onehot = model.matrix(~ LOT - 1, data = df)
n_lot = ncol(onehot)
colnames(onehot) = paste0("LOT", seq_len(n_lot))

# 保留原本的所有欄位，並加上 One-Hot 欄位
df_new = cbind(df, onehot)


#######################################################################

library(car)
# 執行 Levene's Test
# 假設 df 有 OBSERVATION 與 LOT 兩欄
leveneTest(OBSERVATION ~ LOT, data = df_new)


welch_result <- oneway.test(OBSERVATION ~ LOT, data = df_new, var.equal = FALSE)
print(welch_result)


library(PMCMRplus)

gh_result <- gamesHowellTest(OBSERVATION ~ LOT, data = df_new)
gh_result




###########################################################




OBSERVATION = df[,3]
T0404 = df[,407]
T1370 = df[,1373]

model = lm(formula = OBSERVATION ~ T0404 + T1370,data = df_new)
summary(model)


model = lm(formula = OBSERVATION ~ T0404 + T1370 + LOT5, data = df_new)
summary(model)




lot_fac <- factor(df$LOT)                 
n_lot   <- nlevels(lot_fac)


# 指派顏色（≤10 個 LOT 用 rainbow；>10 可以改成 RColorBrewer 或 viridis）
col_vec <- setNames(rainbow(n_lot), levels(lot_fac))

# 繪圖：每點依 LOT 上色
plot(df$OBSERVATION,
     pch = 19,
     col = col_vec[lot_fac],
     xlab = "樣本序 (index)",
     ylab = "OBSERVATION",
     main = "OBSERVATION 散點圖（依 LOT 著色）")

# 圖例
legend("topleft",
       legend = levels(lot_fac),
       col = col_vec,
       pch = 19,
       title = "LOT")
