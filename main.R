# Install essential libraries (please comment them after installing)
# Using Ctrl + Shift + C
#
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")

# Required libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Function to remove rows with missing values in a specific column
CleanData_rm <- function(dataset, column_name) {
  dataset %>% filter(
    !is.na({{ column_name }}) &
      {{ column_name }} != "N/A" &
      {{ column_name }} != ""
  )
}

# Function to extract numeric values from a string
get_num <- function(x) {
  if (is.na(x) || x == "N/A" || x == "") return(NA)
  
  num <- as.numeric(str_extract(x, "[0-9.]+"))
  unit <- str_extract(x, "[A-Za-z]+") %>% toupper()
  
  if (!is.na(unit) && unit == "KB")
    num <- num / 1024
  
  return(num)
}

# Function to split value and unit
split_unit_value <- function(x) {
  if (is.na(x) || x == "N/A" || x == "") return(list(value = NA, unit = NA))
  
  num_part <- str_extract(x, "[0-9.]+")
  unit_part <- str_replace(x, num_part, "") %>% str_trim()
  
  list(value = as.numeric(num_part), unit = unit_part)
}

# Function to convert memory units to megabytes
unit_to_M <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.na(x) || x == "N/A" || x == "") return(NA)
  
  num <- get_num(x)
  if (is.na(num)) return(NA)
  
  unit <- x %>% gsub("[0-9. ]", "", .) %>% substr(1, 1) %>% toupper()
  fac <- switch(unit,
                K = 1 / 1000, M = 1, G = 1000, T = 1000000, 1
  )
  return(num * fac)
}

# Fixed clean_text_data function to handle vectors
clean_text_data <- function(text_vector) {
  sapply(text_vector, function(text) {
    if (is.na(text)) {
      return(NA)
    } else {
      return(str_replace_all(text, "[^[:alnum:]]", " ") %>% str_squish())
    }
  })
}

# Function to load dataset
load_dataset <- function(file_path) {
  dataset <- read_csv(file_path)
  return(dataset)
}

# Function to select columns
select_columns <- function(dataset, columns) {
  dataset[, columns]
}

# Function to clean data
clean_data <- function(data) {
  # Process data with pipe
  data <- data %>%
    # Standard cleaning for character columns
    mutate(across(where(is.character), ~na_if(., "N/A")),
           across(where(is.character), ~na_if(., ""))) %>%
    
    mutate(
      Cache_Size = sapply(Cache, function(x) {
        if (is.na(x)) return(NA)
        num <- as.numeric(str_extract(x, "[0-9.]+"))
        unit <- str_extract(x, "(?i)(KB|MB|GB)")
        factor <- case_when(
          grepl("KB", unit, ignore.case = TRUE) ~ 1/1024,
          grepl("MB", unit, ignore.case = TRUE) ~ 1,
          grepl("GB", unit, ignore.case = TRUE) ~ 1024,
          TRUE ~ 1
        )
        if (is.na(num))
          return(NA)
        return(round(num * factor, 2))
      }),
      Cache_Type = sapply(Cache, function(x) {
        if (is.na(x)) return(NA)
        str_extract(x, "(?i)(SmartCache|L[23]|Last Level Cache)")
      })
    ) %>%
    
    mutate(
      Product_Collection = if ("Product_Collection" %in% names(.)) {
        clean_text_data(Product_Collection)
      } else Product_Collection,
      
      Launch_Quarter = str_extract(Launch_Date, "Q[1-4]"),
      Launch_Year = ifelse(!is.na(str_extract(Launch_Date, "\\d{2}$")),
                           paste0("20", str_extract(Launch_Date, "\\d{2}$")),
                           NA),
      
      Bus_Speed_Value = sapply(Bus_Speed, function(x) split_unit_value(x)$value),
      Bus_Speed_Unit = sapply(Bus_Speed, function(x) split_unit_value(x)$unit),
      
      Lithography = sapply(Lithography, get_num),
      Max_Memory_Bandwidth = sapply(Max_Memory_Bandwidth, unit_to_M),
      Max_Memory_Size = sapply(Max_Memory_Size, unit_to_M),
      Processor_Base_Frequency = sapply(Processor_Base_Frequency, get_num),
      Recommended_Customer_Price = as.numeric(gsub("[\\$,]", "", Recommended_Customer_Price)),
      TDP = sapply(TDP, get_num)
    ) %>%
    
    # Remove Bus_Speed if it exists
    {
      if ("Bus_Speed" %in% names(.)) {
        select(., -Bus_Speed)
      } else {
        .
      }
    } %>%
    # Rearrange columns in preferred order
    {
      desired_order <- c(
        "Product_Collection", "Vertical_Segment", "Launch_Date",
        "Launch_Quarter", "Launch_Year",
        "Bus_Speed_Value", "Bus_Speed_Unit",
        "Cache", "Cache_Size", "Cache_Type",
        "Lithography", "Max_Memory_Bandwidth", "Max_nb_of_Memory_Channels",
        "Max_Memory_Size", "nb_of_Cores",
        "Processor_Base_Frequency", "Recommended_Customer_Price",
        "TDP", "DirectX_Support", "PCI_Express_Revision"
      )
      existing <- desired_order[desired_order %in% names(.)]
      select(., all_of(existing), everything())
    }
  
  return(data)
}

check_missing <- function(data) {
  missing_counts <- colSums(is.na(data))
  missing_df <- data.frame(
    Variable = names(missing_counts),
    Missing_Count = missing_counts
  ) %>%
    arrange(desc(Missing_Count))
  
  return(missing_df)
}

# Function to impute missing values
impute_missing <- function(data) {
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  for (col in numeric_cols) {
    if (any(is.na(data[[col]]))) {
      median_val <- median(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- median_val
    }
  }
  
  char_cols <- names(data)[sapply(data, is.character)]
  for (col in char_cols) {
    if (any(is.na(data[[col]]))) {
      mode_val <- names(which.max(table(data[[col]][!is.na(data[[col]])])))
      data[[col]][is.na(data[[col]])] <- mode_val
    }
  }
  
  return(data)
}

# Main processing function
process_data <- function(file_path, selected_cols = NULL, perform_imputation = TRUE) {
  if (is.null(selected_cols)) {
    selected_cols <- c(
      "Product_Collection",
      "Vertical_Segment",
      "Launch_Date",
      "Bus_Speed",
      "Cache",
      "Lithography",
      "Max_Memory_Bandwidth",
      "Max_nb_of_Memory_Channels",
      "Max_Memory_Size",
      "nb_of_Cores",
      "Processor_Base_Frequency",
      "Recommended_Customer_Price",
      "TDP",
      "DirectX_Support",
      "PCI_Express_Revision"
    )
  }
  
  # Step 1: Load data
  raw_data <- load_dataset(file_path)
  
  # Step 2: Select columns
  selected_data <- select_columns(raw_data, selected_cols)
  
  # Step 3: Clean data (simplified)
  cleaned_data <- clean_data(selected_data)
  
  # Step 4: Check missing values
  missing_summary <- check_missing(cleaned_data)
  
  # Step 5: Impute missing values (optional)
  final_data <- if (perform_imputation) {
    impute_missing(cleaned_data)
  } else {
    cleaned_data
  }
  
  # Return results
  results <- list(
    raw_data = raw_data,
    selected_data = selected_data,
    cleaned_data = cleaned_data,
    final_data = final_data,
    missing_summary = missing_summary
  )
  
  return(results)
}

# Sample choosing
file_path <- "./Data/Intel_CPUs.csv"
selected_cols <- c(
  "Product_Collection",
  "Vertical_Segment",
  "Launch_Date",
  "Bus_Speed",
  "Cache",
  "Lithography",
  "Max_Memory_Bandwidth",
  "Max_nb_of_Memory_Channels",
  "Max_Memory_Size",
  "nb_of_Cores",
  "Processor_Base_Frequency",
  "Recommended_Customer_Price",
  "TDP",
  "DirectX_Support",
  "PCI_Express_Revision"
)

# Process the data
results <- process_data(file_path, selected_cols)
final_data <- results$final_data

# View the simplified results
head(final_data)
str(final_data)

# Export data preprocessed for checking
write_csv(final_data, "./Test/processed_Intel_CPUs.csv")