# Cài đặt gói cần thiết (chỉ cần cài 1 lần, rồi comment để chạy cho các lần sau)
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("purrr")

# Import thư viện
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ----------- Các hàm hỗ trợ xử lý ---------- #

# Check for missing data
is_missing <- function(x) {
  is.na(x) | x == "" | toupper(x) == "N/A"
}

# Filter strings from non-standard characters (ASCII)
clean_text_data <- function(text_vector) {
  sapply(text_vector, function(text) {
    if (is_missing(text)) return(NA)
    else return(str_replace_all(text, "[^[:alnum:]]", " ") %>% str_squish())
  })
}

# Convert the unit into standard
convert_unit <- function(x, base_unit = "MB") {
  if (is_missing(x)) return(NA)
  
  num <- as.numeric(str_extract(x, "[0-9.]+"))
  unit <- str_to_upper(str_extract(x, regex("([kKmMgGtT][bB]|[mMgG][hH][zZ]|[gG][tT]/[sS])", ignore_case = TRUE)))
  
  if (is.na(num)) return(NA)
  
  unit <- case_when(
    unit %in% c("GHZ", "Ghz") ~ "GHz",
    unit %in% c("MHZ", "Mhz") ~ "MHz",
    unit %in% c("GT/S", "Gt/s") ~ "GT/s",
    TRUE ~ unit
  )
  
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

# Normalize the format of data in Cache
normalize_cache <- function(cache_str) {
  if (is_missing(cache_str)) return(list(Cache_Normalized = NA, Cache_Value_MB = NA, Cache_Type = NA))
  
  num <- convert_unit(cache_str, base_unit = "MB")
  type <- str_extract(cache_str, "(SmartCache|L2|L3|Last Level Cache)")
  
  result <- paste0(num, " MB")
  if (!is.na(type)) result <- paste(result, type)
  
  return(list(Cache_Normalized = result, Cache_Value_MB = num, Cache_Type = type))
}

# ---------------- Hàm xử lý dữ liệu ---------------- #


# Hàm tính toán số lượng dữ liệu bị thiếu
check_missing <- function(data) {
  total_rows <- nrow(data)
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
file_path <- "./Data/Intel_CPUs.csv"



# Đọc dữ liệu
CPU_data <- load_dataset(file_path)
head(CPU_data, 10)

# Chọn lọc một số biến cần phân tích

# Define required columns
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

# Choose selected column and read data in these
selected_data <- CPU_data[, intersect(selected_cols, names(CPU_data))]
str(selected_data)



# Clean the raw data
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
write_csv(final_data, "./Test/processed_Intel_CPUs.csv")