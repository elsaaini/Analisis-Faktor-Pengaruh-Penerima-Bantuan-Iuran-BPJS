#### LAPORAN KP ELSA ####

# input data
library(openxlsx)
df = read.xlsx("C:/Users/HP/Downloads/data kp fix_2.xlsx", 3)    # data 1 indo/ribuan


# Variabel Independen
y = df$Penerima.PBI
# Variabel Dependen
X = df[, -c(1,8)]  # ambil data frame selain cholesterol total

# Normalisasi data
logx <- log(X)
logy <- log(y)
plot(logx, logy, main = "Scatterplot of transformed data") 

# Membuat model regresi
model = lm(logy~logx$Pengangguran+logx$`Rata-rata.Pendapatan`+logx$Jumlah.Penerima.Bansos+logx$Jumlah.Miskin+logx$Jumlah.Keluhan.Kesehatan+logx$Keluhan.Kesehatan.Tidak.Terpenuhi) 
summary(model)

library(olsrr)

# All Possible Regression
ols_step_all_possible(model)

# Best Subset Regression
ols_step_best_subset(model)

# Forward Selection berdasarkan p-value
ols_step_forward_p(model, progress = TRUE, details = TRUE, p_val = 0.05)

# Backward Elimination
ols_step_backward_p(model, progress = TRUE, details = TRUE, p_val = 0.05)

# Stepwise Elimination
ols_step_both_p(model, progress = TRUE, details = TRUE, p_enter = 0.05, p_remove = 0.05)


model1 = lm(logy~logx$Jumlah.Penerima.Bansos+logx$Jumlah.Keluhan.Kesehatan+logx$Keluhan.Kesehatan.Tidak.Terpenuhi)
summary(model1)


### ====== DIAGNOSTIC CHECKING ====== ###

## Diagnostic Checking
# Variable Independen : jumlah penerima bansos, jumlah keluhan kesehatan, keluhan kesehatan tidak terpenuhi
library(dplyr)
df1 =read.xlsx("C:/Users/HP/Downloads/data kp fix_2.xlsx", 3)
df_diagnostic <- select(df1, c("Penerima.PBI", "Jumlah.Penerima.Bansos", "Jumlah.Keluhan.Kesehatan", "Keluhan.Kesehatan.Tidak.Terpenuhi"))

#overall test 
overall_p=function(my_model){ 
  f=summary(my_model)$fstatistic 
  p=pf(f[1],f[2],f[3],lower.tail=FALSE) 
  attributes(p)=NULL 
  return(p) 
} 
overall_test=data.frame (matrix(ncol=2,nrow=1)) 
colnames(overall_test)=c("Model","Overall Test P-Value") 
overall_test$Model=c("Moodel") 
overall_test$P.Value=c(overall_p(model1)) 
print(overall_test[,-2])

# Asumsi Linearitas
y_1 = df_diagnostic$Penerima.PBI
X_1 = select(df_diagnostic, -c("Penerima.PBI"))

model2 = lm(df1$Penerima.PBI~df1$Jumlah.Penerima.Bansos+df1$Jumlah.Keluhan.Kesehatan+df1$Keluhan.Kesehatan.Tidak.Terpenuhi)
summary(model2)

# Jika terdapat lebih dari 1 variabel
pairs(df_diagnostic, main = "Matriks Scatterplot Penerima PBI", pch = 19)

## Metode ggplot2 dan GGally
library(ggplot2)
library(GGally)
ggpairs(df_diagnostic, title = "Matriks Scatterplot Penerima PBI",
        axisLabels = "show")
cor(df_diagnostic[,-1], method = "pearson")
R2 = c()

# Membuat Dataframe R-Squared
R2 = data.frame(matrix(ncol = ncol(X_1), nrow = 1))
colnames(R2) = colnames(X_1)
for (i in 1:ncol(X_1)){
  R2[i] = summary(lm(y_1 ~ X_1[,i]))$r.squared
}
print(R2)

intercept_p <- function(my_model) {
  p <- summary(my_model)$coefficients[1,4]
  return(p)
}

# uji partial
partial_test = data.frame(matrix(ncol = 2, nrow = 6))
colnames(partial_test) = c("Model", "Intercept's P-Value")
partial_test$Model = c("Model 1")
partial_test$P.Value = c(intercept_p(model1))

print(partial_test[,-2])

# Uji Parsial untuk Koefisien
coef_p <- function(my_model) {
  p <- summary(my_model)$coefficients[-1,4]
  return(round(p, 3))
}
coef_p(model1)

# NORMALITAS RESIDUAL
model1$residual
## Metode QQ-Plot
qqnorm(model1$residual, main = "Normal Q-Q Plot Residual")
qqline(model1$residual)
## Metode Inferensi, menggunakan Kolmogorov-Smirnov Test karena sampel > 50
ks.test(model1$residual,'pnorm',mean=mean(model1$residual),
        sd=sd(model1$residual))

# HOMOSKEDASTISITAS
## Metode Scatter Plot
library(lmtest)
plot(model1, 1)
bptest(model1)

# NO-AUTOKORELASI EROR 
library(car)
durbinWatsonTest(model1)

# NO MULTIKOLINEARITAS
library(car)
vif_values <- vif(model1)
tolerance <- 1 / vif_values

# Menggabungkan hasil VIF dan toleransi ke dalam satu data frame
results <- data.frame(VIF = vif_values, Tolerance = tolerance)
results

