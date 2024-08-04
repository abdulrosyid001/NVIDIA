# Muat paket yang diperlukan
library(ggthemes)
library(dplyr)
library(ggplot2)
library(caTools)
library(readr)
library(car)
library(tidyr)

# Muat file yang akan diolah
data <- read.csv("D:/file_projek_komstat/NVIDIA1.csv")
View(data)
data

# Struktur data
str(data)

# Mengubah kolom Return_Percentage
return_percen <- substr(data$Return_Percentage, 1, nchar(data$Return_Percentage) - 1)
return_percen <- as.numeric(return_percen)
View(data.frame(return_percen))

# Menggabungkan data asli dengan kolom baru sekaligus mengganti kolom lama
data <- cbind(data, return_percen)
data <- data %>% select(-Return_Percentage)
View(data)

# Memvalidasi Data
sum(is.na(data))
data <- na.omit(data)
data

# Menentukan Label Data
Adj_Close <- data$Adj_Close
Adj_Close

# Menentukan Objek Data
Features <- data %>% select(-Adj_Close, -Date)
Features

# Menelaah data
summary(Adj_Close)
summary(Features)

# Histogram masing-masing kolom
hist(Adj_Close)
hist(data$Open)
hist(data$Range)
hist(data$Volume)
hist(data$Log_Volume)
hist(data$X3_Day_Avg_AdjClose.Delay.)
hist(data$PriorDay_AdjClose)
hist(data$return_percen)

# Plot variabel "Features" untuk melihat hubungan dengan 'Close'
data_long <- data %>%
  pivot_longer(cols = c(Open, Range, Volume, Log_Volume, X3_Day_Avg_AdjClose.Delay., 
                        PriorDay_AdjClose, return_percen), names_to = "Variable", 
               values_to = "Value")

ggplot(data_long, aes(x = Value, y = Adj_Close)) + 
  geom_point() + 
  facet_wrap(~ Variable, scales = "free_x") +
  ggtitle("Various Variables vs Adj_Close")

# Menggabungkan kembali semua kolom
all_column <- cbind(Features, Adj_Close)
all_column
sum(is.null(all_column))
View(all_column)

# Menerapkan transformasi logaritma dengan sedikit penyesuaian pada data
adjustment <- abs(min(all_column)) + 1
all_column <- all_column + adjustment

features_transformed <- log(all_column)
features_transformed

# Plot Setelah Transformasi
hist(features_transformed$Adj_Close)
hist(features_transformed$Open)
hist(features_transformed$Range)
hist(features_transformed$Volume)
hist(features_transformed$Log_Volume)
hist(features_transformed$X3_Day_Avg_AdjClose.Delay.)
hist(features_transformed$PriorDay_AdjClose)
hist(features_transformed$return_percen)

# Untuk plot
target <- features_transformed$Adj_Close

fitur <- features_transformed %>% select(-Adj_Close)
fitur <- cbind(fitur, target)

# Plot antara Adj_Close dengan Variabel Lain
data_long <- fitur %>%
  pivot_longer(cols = c(Open, Range, Volume, Log_Volume, X3_Day_Avg_AdjClose.Delay., 
                        PriorDay_AdjClose, return_percen), names_to = "Variable", 
               values_to = "Value")

ggplot(data_long, aes(x = Value, y = target)) + 
  geom_point() + 
  facet_wrap(~ Variable, scales = "free_x") +
  ggtitle("Various Variables vs Adj_Close")

# Membagi Data Menjadi Dua
set.seed(123)
split <- sample.split(features_transformed$Adj_Close, SplitRatio = 0.80)
split

training_set <- subset(features_transformed, split == TRUE)
training_set
count(training_set)

testing_set <- subset(features_transformed, split == FALSE)
testing_set
count(testing_set)

# Membangun Model (Dengan Metode Regresi)
model <- lm(Adj_Close ~ Open + Range + Volume + Log_Volume + 
              X3_Day_Avg_AdjClose.Delay. + PriorDay_AdjClose + 
              return_percen, data = training_set)
summary(model)

# Memeriksa homoskedastisitas
plot(model$fitted.values, rstandard(model))
abline(h = 0, col = "red")

# Memeriksa normalitas residual
qqnorm(rstandard(model))
qqline(rstandard(model), col = "red")

# Prediksi dengan Model Yang Terbentuk
predictions <- predict(model, testing_set)
predictions

# Perbandingan Y Prediksi dari Model yang Terbentuk dan Y dalam dataset
results <- as.data.frame(cbind(predictions, testing_set$Adj_Close))
colnames(results) <- c("Prediksi", "Nilai_Aktual")
results

# Perbandingan Nilai Prediksi dalam Skala yang Sebenarnya dengan Nilai Prediksi dari Model
real_predictions_value <- exp(predictions)
real_predictions_value <- real_predictions_value - adjustment

real_value_testing_test <- testing_set$Adj_Close
real_value_from_testing_test <- exp(real_value_testing_test)
real_value_from_testing_test <- real_value_from_testing_test - adjustment

comparison <- cbind(real_predictions_value,real_value_from_testing_test)
colnames(comparison)  <- c("Nilai Y dari Model","Nilai Y dalam Dataset")
comparison

# Mengevaluasi Model Menggunakan MAE, MSE, dan RMSE
mae <- mean(abs(real_predictions_value - real_value_from_testing_test))
mse <- mean((real_predictions_value - real_value_from_testing_test)^2)
rmse <- sqrt(mean((real_predictions_value - real_value_from_testing_test)^2))
r_square <- summary(model)$r.squared

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squre Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R square:", r_square)
