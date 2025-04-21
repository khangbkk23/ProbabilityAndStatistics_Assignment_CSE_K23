# Section 1: Data Preprocessing - Intel CPU Data Cleaning
##########################################################

# ========== Install and load required packages ==========
# Only run once, then comment out
# install.packages(c("readr", "dplyr", "tidyr", "stringr", "purrr"))
#setwd("C:/Users/vandu/ProbabilityAndStatistics_Assignment_CSE_K23/Data")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ========== Utility functions for cleaning ==========

# Check for missing or invalid values
is_missing <- function(x) {
  is.na(x) | x == "" | toupper(x) == "N/A"
}

# Clean non-standard characters from text fields
clean_text_data <- function(text_vector) {
  sapply(text_vector, function(text) {
    if (is_missing(text)) return(NA)
    else return(str_replace_all(text, "[^[:alnum:]]", " ") %>% str_squish())
  })
}

# Convert units to standard form
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

# Extract numeric value
extract_numeric <- function(x) {
  if (is_missing(x)) return(NA)
  num <- as.numeric(str_extract(x, "[0-9.]+"))
  return(num)
}

# Normalize cache data into standard format
normalize_cache <- function(cache_str) {
  if (is_missing(cache_str)) return(list(Cache_Normalized = NA, Cache_Value_MB = NA, Cache_Type = NA))
  
  num <- convert_unit(cache_str, base_unit = "MB")
  type <- str_extract(cache_str, "(SmartCache|L2|L3|Last Level Cache)")
  result <- paste0(num, " MB")
  if (!is.na(type)) result <- paste(result, type)
  
  return(list(Cache_Normalized = result, Cache_Value_MB = num, Cache_Type = type))
}

# ========== Main preprocessing functions ==========

# Select relevant columns from dataset
select_columns <- function(dataset, columns) {
  return(dataset[, columns])
}

# Clean raw data and create new standardized columns
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
      
      TDP = sapply(TDP, extract_numeric),
      Lithography = sapply(Lithography, extract_numeric),
      Max_Memory_Bandwidth = sapply(Max_Memory_Bandwidth,extract_numeric),
      
      Launch_Quarter = str_extract(Launch_Date, "Q[1-4]"),
      Launch_Year = ifelse(!is.na(str_extract(Launch_Date, "\\d{2}$")),
                           paste0("20", str_extract(Launch_Date, "\\d{2}$")),
                           NA),
      Launch_Year = as.numeric(Launch_Year),
      Processor_Base_Frequency = sapply(Processor_Base_Frequency, convert_unit, base_unit = "GHz")
    ) %>%
    filter(is_missing(Launch_Year) | Launch_Year <= 2025) %>%  # lọc theo điều kiện miền
    select(-cache_data)
  
  desired_columns <- c(
    "Product_Collection", "Vertical_Segment", "Launch_Date", "Launch_Year", "Launch_Quarter", "Lithography",
    "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache_Value_MB",
    "Cache_Type", "TDP", "Max_Memory_Bandwidth", "Embedded_Options_Available"
  )
  
  existing_columns <- desired_columns[desired_columns %in% names(data)]
  data <- data %>% select(all_of(existing_columns))
  
  return(data)
}

# ========== Execute preprocessing ==========

# Input CSV file path
file_path <- "./Data/Intel_CPUs.csv"
#file_path <- "C:\\Users\\vandu\\ProbabilityAndStatistics_Assignment_CSE_K23\\Data\\Intel_CPUs.csv"

# Load dataset
CPU_data <- read_csv(file_path)

# Display first few rows
head(CPU_data, 10)

# Select important columns
selected_cols <- c(
  "Product_Collection", "Vertical_Segment", "Launch_Date", "Lithography",
  "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache",
  "Cache_Value_MB", "Cache_Type", "TDP", "Max_Memory_Bandwidth",
  "Embedded_Options_Available"
)

selected_data <- CPU_data[, intersect(selected_cols, names(CPU_data))]
str(selected_data)

# Clean and transform the data
cleaned_data <- clean_data(selected_data)

# Check missing values
missing_summary <- check_missing(cleaned_data)
print(missing_summary)

# Impute if enabled
perform_imputation = TRUE
final_data <- if (perform_imputation) {
  impute_missing(cleaned_data)
} else cleaned_data

# Preview final cleaned data
print(head(final_data))
str(final_data)

# Export to CSV for testing (optional)
write_csv(final_data, "./Test/processed_Intel_CPUs.csv")


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

# boxplot: TDP theo Vertical_Segment
plot1 <- ggplot(final_data, aes(x = Vertical_Segment, y = TDP, fill = Vertical_Segment)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "TDP and Vertical Segment",
       x = "Vertical Segment",
       y = "TDP (W)",
       fill = "Vertical Segment") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(plot1)

# ggsave("Boxplot_TDP_by_Vertical_Segment.png", plot1, width = 8, height = 6, dpi = 300)

# boxplot: TDP theo Vertical_Segment và Launch_Year
# Tạo nhóm năm (3-4 năm mỗi nhóm)
data_cln <- final_data %>%
  # filter(!is.na(Launch_Year) & as.numeric(Launch_Year) <= 2025) %>%
  mutate(Year_Group = case_when(
    Launch_Year >= 2000 & Launch_Year <= 2003 ~ "2000-2003",
    Launch_Year >= 2004 & Launch_Year <= 2007 ~ "2004-2007",
    Launch_Year >= 2008 & Launch_Year <= 2011 ~ "2008-2011",
    Launch_Year >= 2012 & Launch_Year <= 2015 ~ "2012-2015",
    Launch_Year >= 2016 & Launch_Year <= 2019 ~ "2016-2019",
    TRUE ~ "Other" # Cho các năm ngoài phạm vi, nhưng không mong đợi vì đã lọc <= 2025
  ))

# Lọc các nhóm năm đủ dữ liệu
year_segment_counts <- data_cln %>%
  group_by(Year_Group, Vertical_Segment) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count >= 5) # Đảm bảo mỗi nhóm có đủ dữ liệu để boxplot có ý nghĩa

data_filtered <- data_cln %>%
  semi_join(year_segment_counts, by = c("Year_Group", "Vertical_Segment"))

plot2 <- ggplot(data_filtered, aes(x = Year_Group, y = TDP, fill = Vertical_Segment)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.4, alpha = 0.7) +
  scale_fill_brewer(palette = "Set2", name = "Segment") +
  theme_minimal() +
  labs(
    title = "TDP and Vertical Segment Across Year Groups",
    x = "Year Group",
    y = "TDP (W)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(plot2)

# ggsave("Boxplot_TDP_by_Segment_Year_Groups.png", plot2, width = 10, height = 6, dpi = 300)

# Hàm tạo scatter plot
create_scatter_plot <- function(data, x_var, x_label, filename) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = TDP)) +
    geom_point( color = " olivedrab4 ", alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("TDP and", x_label),
      x = x_label,
      y = "TDP (W)"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "none"
    )
  print(p)

  # ggsave(filename, p, width = 8, height = 6, dpi = 300)
}

# Vẽ các scatter plot
create_scatter_plot(final_data, "Vertical_Segment", "Vertical Segment", "Scatter_TDP_vs_Vertical_Segment.png")
create_scatter_plot(final_data, "Launch_Year", "Launch Year", "Scatter_TDP_vs_Launch_Year.png")
create_scatter_plot(final_data, "Lithography", "Lithography (nm)", "Scatter_TDP_vs_Lithography.png")
create_scatter_plot(final_data, "nb_of_Cores", "Number of Cores", "Scatter_TDP_vs_nb_of_Cores.png")
create_scatter_plot(final_data, "nb_of_Threads", "Number of Threads", "Scatter_TDP_vs_nb_of_Threads.png")
create_scatter_plot(final_data, "Processor_Base_Frequency", "Processor Base Frequency (GHz)", "Scatter_TDP_vs_Processor_Base_Frequency.png")
create_scatter_plot(final_data, "Cache_Value_MB", "Cache (MB)", "Scatter_TDP_vs_Cache_Value_MB.png")
create_scatter_plot(final_data, "Embedded_Options_Available", "Embedded Options Available", "Scatter_TDP_vs_Embedded_Options_Available.png")
create_scatter_plot(final_data, "Max_Memory_Bandwidth", "Max Memory Bandwidth (GB/s)", "Scatter_TDP_vs_Max_Memory_Bandwidth.png")