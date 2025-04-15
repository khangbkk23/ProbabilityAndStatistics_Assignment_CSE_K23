# ===============================
# PHÂN TÍCH & DỰ ĐOÁN TDP CỦA CPU
# ===============================

# Cài đặt & nạp thư viện cần thiết
# (chỉ cần cài lần đầu, sau đó hãy comment lại)
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("caret")
# install.packages("Metrics")
# install.packages("patchwork")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(Metrics)
library(ggplot2)
library(patchwork)

# ---------------------- HÀM LOẠI OUTLIER ----------------------
remove_outliers <- function(CPUs_data, column_name) {
  column_data <- CPUs_data[[column_name]]
  if (is.numeric(column_data)) {
    Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    CPUs_data[[column_name]] <- ifelse(
      column_data < (Q1 - 1.5 * IQR) | column_data > (Q3 + 1.5 * IQR),
      NA,
      column_data
    )
    CPUs_data <- CPUs_data[!is.na(CPUs_data[[column_name]]), ]
  }
  return(CPUs_data)
}

# ---------------------- ĐỌC VÀ CHUẨN HÓA DỮ LIỆU ----------------------
library(dplyr)
library(stringr)

# Đọc dữ liệu
CPUs <- read.csv("E:\\Thoi\\Dai_Hoc\\242\\XSTK\\ProbabilityAndStatistics_Assignment_CSE_K23\\Data\\Intel_CPUs.csv")
CPUs_data <- CPUs[, c(
  "Vertical_Segment", "Launch_Date", "Lithography",
  "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency",
  "Cache", "TDP", "Max_Memory_Bandwidth", "Embedded_Options_Available"
)]
rm(CPUs)

# Kiểm tra missing value
apply(is.na(CPUs_data), 2, sum)
apply(is.na(CPUs_data), 2, mean)

# Xoá dòng chứa NA
CPUs_data <- na.omit(CPUs_data)

# ---------------------- CHUẨN HÓA CỘT CACHE (KB -> MB) ----------------------
split_CPUs_data <- str_match(CPUs_data$Cache, "([0-9.]+)\\s*(KB|MB)")
split_CPUs_data <- as.data.frame(split_CPUs_data)
names(split_CPUs_data) <- c("full", "number", "unit")
split_CPUs_data$number <- as.numeric(split_CPUs_data$number)
split_CPUs_data <- split_CPUs_data %>%
  mutate(
    number = ifelse(unit == "KB", number / 1024, number),
    unit = "MB"
  )
CPUs_data$Cache <- split_CPUs_data$number
CPUs_data$Cache_type <- split_CPUs_data$unit

# ---------------------- CHUẨN HÓA CỘT Processor_Base_Frequency ----------------------
split_CPUs_data <- str_match(CPUs_data$Processor_Base_Frequency, "([0-9.]+)\\s*(MHz|GHz)")
split_CPUs_data <- as.data.frame(split_CPUs_data)
names(split_CPUs_data) <- c("full", "number", "unit")
split_CPUs_data$number <- as.numeric(split_CPUs_data$number)
split_CPUs_data <- split_CPUs_data %>%
  mutate(
    number = ifelse(unit == "MHz", number / 1000, number),
    unit = "GHz"
  )
CPUs_data$Processor_Base_Frequency <- split_CPUs_data$number

# ---------------------- CHUẨN HÓA CÁC CỘT ĐƠN VỊ ----------------------
columns_to_clean <- c("Lithography", "TDP", "Max_Memory_Bandwidth")

for (column_name in columns_to_clean) {
  split_CPUs_data <- str_match(CPUs_data[[column_name]], "([0-9.]+)\\s*([a-zA-Z/]+)")
  split_CPUs_data <- as.data.frame(split_CPUs_data)
  names(split_CPUs_data) <- c("full", "number", "unit")
  CPUs_data[[column_name]] <- as.numeric(split_CPUs_data$number)
  CPUs_data[[paste0(column_name, "_unit")]] <- split_CPUs_data$unit
}

# ---------------------- LOẠI OUTLIERS CHO NHIỀU CỘT ----------------------
columns_for_outlier_removal <- c(
  "Lithography", "nb_of_Cores", "nb_of_Threads",
  "Processor_Base_Frequency", "Cache", "TDP", "Max_Memory_Bandwidth"
)

for (col in columns_for_outlier_removal) {
  CPUs_data <- remove_outliers(CPUs_data, col)
}

#________________________COMPARE____________________
columns <- list(
  Lithography = "nm",
  nb_of_Cores = "cores",
  nb_of_Threads = "threads",
  Processor_Base_Frequency = "GHz",
  Cache = "MB",
  TDP = "W",
  Max_Memory_Bandwidth = "GB/s"
)

# Tạo danh sách biểu đồ
plot_list <- list()

for (column_name in names(columns)) {
  if (column_name == "TDP") next  # Bỏ qua chính TDP

  col_label <- columns[[column_name]]

  p <- ggplot(CPUs_data, aes(x = .data[[column_name]], y = TDP, color = Vertical_Segment)) +
    geom_point(shape = 16, size = 2) +
    labs(
      title = paste("Mối quan hệ giữa", column_name, "và TDP"),
      x = paste(column_name, "(", col_label, ")"),
      y = "TDP (W)"
    ) +
    theme_minimal()

  # Thêm vào danh sách
  plot_list[[column_name]] <- p
}

# Hiển thị toàn bộ trong một khung (3 cột)
print(wrap_plots(plot_list, ncol = 3))

#________________KIEMDINH_______________
# cat("Trung bình TDP:", mean(CPUs_data$TDP), "\n")
# print(shapiro.test(CPUs_data$TDP))  # Kiểm định chuẩn

# # Tính toán tham số thống kê mẫu
# n <- length(CPUs_data$TDP)
# x <- mean(CPUs_data$TDP)
# s <- sd(CPUs_data$TDP)
# print(data.frame(n, x, s))

# # Kiểm định giả thuyết: H0: μ = 60
# z0 <- (x - 60) / (s / sqrt(n))
# z_alpha <- qnorm(p = 0.05, lower.tail = TRUE)
# cat(sprintf("z0 = %.4f\n", z0))
# cat(sprintf("z_alpha = %.4f\n", z_alpha))
# cat("Bác bỏ H0:", z0 < z_alpha, "\n\n")

# # Chuẩn bị dữ liệu cho mô hình hồi quy

#____________________________HOIQUY________________
CPUs_data <- subset(CPUs_data, select = c("TDP", "nb_of_Cores", "Max_Memory_Bandwidth", 
                                   "Processor_Base_Frequency", "Lithography"))
# Tách dữ liệu thành train/test
set.seed(1000)
training_indices <- createDataPartition(CPUs_data$TDP, p = 0.8, list = FALSE)
train.data <- CPUs_data[training_indices, ]
test.data <- CPUs_data[-training_indices, ]

# Huấn luyện mô hình hồi quy tuyến tính
lm1 <- lm(TDP ~ ., data = train.data)
print( summary(lm1))
# Dự đoán trên tập test
prediction <- predict(lm1, test.data)

# Vẽ biểu đồ thực tế vs dự đoán
plot(test.data$TDP, prediction,
     xlab = "TDP thực tế",
     ylab = "TDP dự đoán",
     main = "TDP thực tế vs TDP dự đoán",
     pch = 19, col = "blue")
abline(a = 1, b = 1, col = "red", lwd = 2)

# Tính các chỉ số đánh giá mô hình
results <- data.frame(
  R2   = round(R2(prediction, test.data$TDP), 4),
  RMSE = round(rmse(test.data$TDP, prediction), 4),
  MAE  = round(mae(test.data$TDP, prediction), 4)
)

cat("\n=== Kết quả đánh giá mô hình ===\n")
#print(results)

#plot(lm1, which = 1)  # Residuals vs Fitted
#plot(lm1, which = 2)  # Normal Q-Q
#plot(lm1, which = 3)  # Scale-Location
#plot(lm1, which = 4)  # Residuals vs Leverage

#print(head(CPUs_data))