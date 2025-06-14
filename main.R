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
    filter(is_missing(Launch_Year) | Launch_Year <= 2025) %>%
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
  integer_cols <- c("Launch_Year","nb_of_Cores", "nb_of_Threads")
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

# Preview final cleaned data
print(head(final_data))
str(final_data)

# Export to CSV for testing (optional)
write_csv(final_data, "./Test/processed_Intel_CPUs.csv")