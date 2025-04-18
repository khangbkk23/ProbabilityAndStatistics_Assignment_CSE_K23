# Cài đặt gói cần thiết (chỉ cần cài 1 lần, rồi comment để chạy cho các lần sau)
setwd("C:/Users/vandu/ProbabilityAndStatistics_Assignment_CSE_K23/Data")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("purrr")
# install.packages("car")
# Import thư viện
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ----------- Các hàm hỗ trợ xử lý ---------- #

# Hàm kiểm tra dữ liệu bị thiếu
is_missing <- function(x) {
  is.na(x) | x == "" | toupper(x) == "N/A"
}

# Hàm lọc văn bản khỏi các ký tự không chuẩn (ASCII)
clean_text_data <- function(text_vector) {
  sapply(text_vector, function(text) {
    if (is.na(text)) {
      return(NA)
    } else {
      return(str_replace_all(text, "[^[:alnum:]]", " ") %>% str_squish())
    }
  })
}

# Hàm chuyển đổi đơn vị sang đơn vị chuẩn
convert_unit <- function(x, base_unit = "MB") {
  if (is_missing(x)) return(NA)
  
  # Lấy số và chuẩn hóa đơn vị (case insensitive)
  num <- as.numeric(str_extract(x, "[0-9.]+"))
  unit <- str_to_upper(str_extract(x, regex("([kKmMgGtT][bB]|[mMgG][hH][zZ]|[gG][tT]/[sS])", ignore_case = TRUE)))
  
  if (is.na(num)) return(NA)
  
  # Bổ sung thêm các đơn vị thiếu
  unit <- case_when(
    unit %in% c("GHZ", "Ghz") ~ "GHz",
    unit %in% c("MHZ", "Mhz") ~ "MHz",
    unit %in% c("GT/S", "Gt/s") ~ "GT/s",
    TRUE ~ unit
  )
  
  # Bảng chuyển đổi mở rộng
  conversion_table <- list(
    "MB" = list("KB" = 1/1024, "MB" = 1, "GB" = 1024, "TB" = 1024^2),
    "GHz" = list("MHz" = 1/1000, "GHz" = 1, "GT/s" = 1),
    "MHz" = list("MHz" = 1, "GHz" = 1000)
  )
  
  if (base_unit %in% names(conversion_table) && unit %in% names(conversion_table[[base_unit]])) {
    factor <- conversion_table[[base_unit]][[unit]]
    return(round(num * factor, 2))
  }
  
  return(NA)
}

# ----Hàm xử lý dữ liệu dành cho cột riêng biệt ------#

# Chuẩn hóa dữ liệu cột Cache
normalize_cache <- function(cache_str) {
  if (is_missing(cache_str)) return(list(Cache_Normalized = NA, Cache_Value_MB = NA, Cache_Type = NA))
  
  num <- convert_unit(cache_str, base_unit = "MB")
  type <- str_extract(cache_str, "(SmartCache|L2|L3|Last Level Cache)")
  
  result <- paste0(num, " MB")
  if (!is.na(type)) result <- paste(result, type)
  
  return(list(Cache_Normalized = result, Cache_Value_MB = num, Cache_Type = type))
}


# ---------------- Hàm xử lý dữ liệu ---------------- #

# Hàm load dữ liệu từ file CSV
load_dataset <- function(file_path) {
  dataset <- read_csv(file_path)
  return(dataset)
}

# Chọn cột
select_columns <- function(dataset, columns) {
  return(dataset[, columns])
}

# Hàm làm sạch dữ liệu
clean_data <- function(data) {
  data <- data %>%
    mutate(across(where(is.character), ~na_if(., "N/A")),
           across(where(is.character), ~na_if(., ""))) %>%
    
    mutate(
      Product_Collection = if ("Product_Collection" %in% names(.)) {
        clean_text_data(Product_Collection)
      } else Product_Collection,
      cache_data = map(Cache, normalize_cache),
      Cache = map_chr(cache_data, "Cache_Normalized"),
      Cache_Value_MB = map_dbl(cache_data, "Cache_Value_MB"),
      Cache_Type = map_chr(cache_data, "Cache_Type"),
      
      Launch_Quarter = str_extract(Launch_Date, "Q[1-4]"),
      Launch_Year = ifelse(!is.na(str_extract(Launch_Date, "\\d{2}$")),
                           paste0("20", str_extract(Launch_Date, "\\d{2}$")),
                           NA),
      Processor_Base_Frequency = sapply(Processor_Base_Frequency, convert_unit, base_unit = "GHz")
    ) %>%
    select(-cache_data)
  
  desired_columns <- c(
    "Product_Collection",
    "Vertical_Segment",
    "Launch_Date",
    "Launch_Year",
    "Lithography",
    "nb_of_Cores",
    "nb_of_Threads",
    "Processor_Base_Frequency",
    "Cache",
    "Cache_Value_MB",
    "Cache_Type",
    "TDP",
    "Max_Memory_Bandwidth",
    "Embedded_Options_Available"
  )
  
  existing_columns <- desired_columns[desired_columns %in% names(data)]
  data <- data %>% select(all_of(existing_columns))
  
  return(data)
}

# Hàm tính toán số lượng dữ liệu bị thiếu
check_missing <- function(data) {
  total_rows <- nrow((data))
  missing_counts <- sapply(data, function(col) sum(is_missing(col)))
  missing_percent <- round(missing_counts / total_rows * 100, 2)
  
  return(data.frame(
    Variable = names(data),
    Missing_Count = missing_counts,
    Missing_Percent = missing_percent
  ) %>% arrange(desc(Missing_Count)))
}

# Hàm điền dữ liệu bị thiếu
impute_missing <- function(data) {
  integer_cols <- c("nb_of_Cores", "nb_of_Threads")
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  for (col in numeric_cols) {
    missing_idx <- is_missing(data[[col]])
    if (any(missing_idx)) {
      mean_val <- mean(data[[col]][!missing_idx], na.rm = TRUE)
      if (col %in% integer_cols)
        data[[col]][missing_idx] <- round(mean_val)
      else
        data[[col]][missing_idx] <- round(mean_val, 2)
    }
  }
  
  char_cols <- names(data)[sapply(data, is.character)]
  for (col in char_cols) {
    missing_idx <- is_missing(data[[col]])
    if (any(missing_idx)) {
      non_missing <- data[[col]][!missing_idx]
      mode_val <- names(which.max(table(non_missing)))
      data[[col]][missing_idx] <- mode_val
    }
  }
  
  return(data)
}

# ----------- Thực thi --------------- #

# File CSV đầu vào
file_path <- "C:\\Users\\vandu\\ProbabilityAndStatistics_Assignment_CSE_K23\\Data\\Intel_CPUs.csv"

# Các cột cần lấy
selected_cols <- c(
  "Product_Collection",
  "Vertical_Segment",
  "Launch_Date",
  "Lithography",
  "nb_of_Cores",
  "nb_of_Threads",
  "Processor_Base_Frequency",
  "Cache",
  "Cache_Value_MB",
  "Cache_Type",
  "TDP",
  "Max_Memory_Bandwidth",
  "Embedded_Options_Available"
)

# Chọn lọc một số biến cần phân tích

raw_data <- load_dataset(file_path)
# Nếu không chỉ định columns, sử dụng danh sách mặc định (chỉ những cột có trong raw_data)
if (is.null(selected_cols)) {
  selected_cols <- c(
    "Product_Collection",
    "Vertical_Segment",
    "Launch_Date",
    "Lithography",
    "nb_of_Cores",
    "nb_of_Threads",
    "Processor_Base_Frequency",
    "Cache",
    "TDP",
    "Max_Memory_Bandwidth",
    "Embedded_Options_Available"
  )
}

available_cols <- selected_cols[selected_cols %in% names(raw_data)]
selected_data <- select_columns(raw_data, available_cols)

# Làm sạch dữ liệu
cleaned_data <- clean_data(selected_data)

# Kiểm tra số lượng và tỷ lệ dữ liệu bị khuyết
missing_summary <- check_missing(cleaned_data)
print(missing_summary)

# Xử lý dữ liệu bị khuyết bằng chiến thuật lấy trung bình và trung vị
perform_imputation = TRUE
final_data <- if (perform_imputation) {
  impute_missing(cleaned_data)
} else {
  cleaned_data
}

# Xem dữ liệu đầu ra
print(head(final_data))
str(final_data)

# Xuất dữ liệu ra file để kiểm thử
# write_csv(final_data, "./Test/processed_Intel_CPUs.csv")

#__________________________KIỂM ĐỊNH 2 MẪU______________________
# Kiểm định giả thuyết:
# H0: Trung bình TDP hai nhóm bằng nhau
# H1: Trung bình TDP hai nhóm khác nhau
# Hàm chuẩn hóa TDP (tách số và đơn vị)
normalize_tdp <- function(tdp_str) {
  if (is_missing(tdp_str)) return(NA)
  num <- as.numeric(str_extract(tdp_str, "[0-9.]+"))  
  return(num)
}

# Áp dụng hàm chuẩn hóa TDP cho cột `TDP`
final_data$TDP <- sapply(final_data$TDP, normalize_tdp)
# Phân dữ liệu thành 2 nhóm:
final_data$Group <- ifelse(final_data$Embedded_Options_Available == "Yes", "Group_Yes", "Group_No")
Embedded__Option <- subset(final_data, Group == "Group_Yes")
Non_Embedded__Option  <- subset(final_data, Group == "Group_No")
# Thống kê cho 2 mẫu:
n1 <- length(Embedded__Option$TDP)
x1 <- mean(Embedded__Option$TDP)
s1 <- sd(Embedded__Option$TDP)

n2 <- length(Non_Embedded__Option$TDP)
x2 <- mean(Non_Embedded__Option$TDP)
s2 <- sd(Non_Embedded__Option$TDP)
data.frame(n1,x1,s1,n2,x2,s2)

# Kiểm định chuẩn cho từng nhóm
# Vẽ Q-Q plot cho nhóm "Group_Yes"
qqnorm(Embedded__Option$TDP)
qqline(Embedded__Option$TDP)
# Shapiro test 
shapiro.test(Embedded__Option$TDP)
# Vẽ Q-Q plot cho nhóm "Group_No"
qqnorm(Non_Embedded__Option$TDP)
qqline(Non_Embedded__Option$TDP)
#Shapiro test
shapiro.test(Non_Embedded__Option$TDP)
#Tính giá trị kiểm định và xác định vùng bác bỏ
z0 <- (x1 - x2) / sqrt(s1^2/n1 + s2^2/n2)
z_half_alpha <- qnorm(p = 0.025, lower.tail = FALSE)  
cat("z0 =", z0, " | z_alpha =", z_half_alpha, "\n")

# Kiểm tra xem z0 có nằm trong miền bác bỏ không
if (abs(z0) > z_half_alpha) {
  cat("→ Bác bỏ H0. Có sự khác biệt TDP giữa hai nhóm.\n")
} else {
  cat("→ Không đủ bằng chứng để bác bỏ H0. Trung bình TDP có thể giống nhau.\n")
}

#______________ANOVA_______________________________
# Mô hình ANOVA 1 yếu tố: So sánh TDP giữa các phân khúc CPU:
# Giả thuyết H0: Trung bình TDP ở các phân khúc là bằng nhau
# Giả thuyết H1: Có ít nhất hai phân khúc có TDP trung bình khác nhau
# Kiểm tra các giả định cho mô hình ANOVA:
# Giả định 1: Dữ liệu tuân theo phân phối chuẩn
# Mobile 
Mobile_data <- subset(final_data,final_data$Vertical_Segment=="Mobile")
qqnorm(Mobile_data$TDP)
qqline(Mobile_data$TDP)
shapiro.test(Mobile_data$TDP)
# Server
Server_data <- subset(final_data,final_data$Vertical_Segment=="Server")
qqnorm(Server_data$TDP)
qqline(Server_data$TDP)
shapiro.test(Server_data$TDP)
# Desktop 
Desktop_data <- subset(final_data,final_data$Vertical_Segment=="Desktop")
qqnorm(Desktop_data$TDP)
qqline(Desktop_data$TDP)
shapiro.test(Desktop_data$TDP)
# Embedded 
Embedded_data <- subset(final_data,final_data$Vertical_Segment=="Embedded")
qqnorm(Embedded_data$TDP)
qqline(Embedded_data$TDP)
shapiro.test(Embedded_data$TDP)
# Giả định 2: Phương sai giữa các nhóm là bằng nhau
library(car)
leveneTest(TDP~as.factor(Vertical_Segment),final_data)
# Giả sử thỏa 
anova_vs <- aov(TDP ~ Vertical_Segment, data = final_data)
summary(anova_vs)
# Thực hiện so sánh bội
tukey_result <- TukeyHSD(anova_vs)
print(tukey_result)

# Tạo dataframe từ kết quả Tukey
tukey_df <- as.data.frame(tukey_result$Vertical_Segment)
tukey_df$comparison <- rownames(tukey_result$Vertical_Segment)

# Rút gọn tên nhóm (Server → S, Embedded → E, Mobile → M, Desktop → D)
tukey_df$short_label <- tukey_df$comparison
tukey_df$short_label <- gsub("Server", "S", tukey_df$short_label)
tukey_df$short_label <- gsub("Embedded", "E", tukey_df$short_label)
tukey_df$short_label <- gsub("Mobile", "M", tukey_df$short_label)
tukey_df$short_label <- gsub("Desktop", "D", tukey_df$short_label)

# Vẽ biểu đồ bằng ggplot2
library(ggplot2)

ggplot(tukey_df, aes(x = diff, y = reorder(short_label, diff))) +
  geom_point(size = 3, color = "#1f77b4") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.3, color = "#1f77b4") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Differences in mean levels of Vertical_Segment"
  ) +
  theme_minimal(base_size = 14)


