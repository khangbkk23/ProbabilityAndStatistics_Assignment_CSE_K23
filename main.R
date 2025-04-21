# xử lý dữ liệu
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
library(ggplot2)
library(corrplot)


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
      
      Processor_Base_Frequency = sapply(Processor_Base_Frequency, convert_unit, base_unit = "GHz")
    ) %>%
    select(-cache_data)
  
  desired_columns <- c(
    "Product_Collection", "Vertical_Segment", "Launch_Date", "Launch_Year", "Lithography",
    "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Cache_Value_MB",
    "Cache_Type", "TDP", "Max_Memory_Bandwidth", "Embedded_Options_Available"
  )
  
  existing_columns <- desired_columns[desired_columns %in% names(data)]
  data <- data %>% select(all_of(existing_columns))
  
  return(data)
}

# Check summary of missing values
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

# Impute missing values: mean for numeric, mode for categorical
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

# boxplot: TDP theo Vertical_Segment
plot1 <- ggplot(data_clean, aes(x = Vertical_Segment, y = TDP, fill = Vertical_Segment)) +
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
data_cln <- data_clean %>%
  filter(!is.na(Launch_Year) & as.numeric(Launch_Year) <= 2025) %>%
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
create_scatter_plot(data_clean, "Vertical_Segment", "Vertical Segment", "Scatter_TDP_vs_Vertical_Segment.png")
create_scatter_plot(data_clean, "Launch_Year", "Launch Year", "Scatter_TDP_vs_Launch_Year.png")
create_scatter_plot(data_clean, "Lithography", "Lithography (nm)", "Scatter_TDP_vs_Lithography.png")
create_scatter_plot(data_clean, "nb_of_Cores", "Number of Cores", "Scatter_TDP_vs_nb_of_Cores.png")
create_scatter_plot(data_clean, "nb_of_Threads", "Number of Threads", "Scatter_TDP_vs_nb_of_Threads.png")
create_scatter_plot(data_clean, "Processor_Base_Frequency", "Processor Base Frequency (GHz)", "Scatter_TDP_vs_Processor_Base_Frequency.png")
create_scatter_plot(data_clean, "Cache_Value_MB", "Cache (MB)", "Scatter_TDP_vs_Cache_Value_MB.png")
create_scatter_plot(data_clean, "Embedded_Options_Available", "Embedded Options Available", "Scatter_TDP_vs_Embedded_Options_Available.png")
create_scatter_plot(data_clean, "Max_Memory_Bandwidth", "Max Memory Bandwidth (GB/s)", "Scatter_TDP_vs_Max_Memory_Bandwidth.png")