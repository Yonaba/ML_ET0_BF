Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(moments)

# Define function to calculate summary statistics
calculate_summary_statistics <- function(station, sheets, data) {
  summary_stats <- data.frame(
    Station = station,
    Vars = sheets,
    Mean = apply(data, 2, mean, na.rm = T),
    Minimum = apply(data, 2, min, na.rm = T),
    First_Quartile = apply(data, 2, quantile, probs = 0.25, na.rm = T),
    Median = apply(data, 2, median, na.rm = T),
    Third_Quartile = apply(data, 2, quantile, probs = 0.75, na.rm = T),
    Maximum = apply(data, 2, max, na.rm = T),
    Std_Dev = apply(data, 2, sd, na.rm = T),
    Kurtosis = apply(data, 2, kurtosis, na.rm = T),
    Skewness = apply(data, 2, skewness, na.rm = T),
    Spearman_Correlation = apply(data, 2, function(x) 
      cor.test(x, data[,6], method = "spearman", conf.level = 0.95, 
               alternative = "two.sided", continuity = T)$estimate),
    Spearman_Pval = apply(data, 2, function(x) 
      cor.test(x, data[,6], method = "spearman", conf.level = 0.95, 
               alternative = "two.sided", continuity = T)$p.value)    
  )
  
  return (summary_stats)
}

# Read Excel file
file_path <- "Data/cli_data_bf.xlsx"
sheets <- c("tx", "tn", "rs", "rh", "ws", "et0")
data_list <- lapply(sheets, function(sheet) read_xlsx(file_path, sheet = sheet))
names(data_list) <- sheets

# Initialize list to store reorganized data
reorganized_data <- list()

# Reorganize data by station
for (station in colnames(data_list[[1]])[-1]) { # Exclude the date column
  station_data <- list()
  
  for (sheet_name in sheets) {
    sheet_data <- data_list[[sheet_name]]
    station_data[[sheet_name]] <- sheet_data[[station]]
  }
  
  reorganized_data[[station]] <- do.call(cbind, station_data)
  colnames(reorganized_data[[station]]) <- sheets
}

# Calculate summary statistics for each station
summary_statistics_list <- list()

for (station in names(reorganized_data)) {
  print(paste0("Processing: ",station))
  station_data <- reorganized_data[[station]]
  
  stats <- calculate_summary_statistics(station, sheets, station_data)
  summary_statistics_list[[station]] <- stats
}

# Combine all summary statistics into a single data frame
summary_statistics_df <- do.call(rbind, summary_statistics_list)
write.csv(summary_statistics_df, file = paste0("tables/summary_stat.csv"), row.names = F)

print("finished.")
