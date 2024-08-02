Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(ggpubr)

bf.stations <- read.csv(file = paste0("bf_stations.csv"),header = T)
bf.stations <- bf.stations[order(bf.stations$Latitude, decreasing=T),]$shName

# Define a function to read and process each sheet
process_sheet_annual <- function(sheet_name, file_path) {
  # Read the data
  data <- read_excel(file_path, sheet = sheet_name)
  
  # Convert the date column to Date type
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  
  # Extract year from the date
  data$Year <- year(data$Date)
  
  # Summarize data to annual values
  annual_data <- data %>%
    group_by(Year) %>%
    summarise(across(bobo:po, mean, na.rm = TRUE))
  
  colnames(annual_data)[2:length(colnames(annual_data))] <- 
    toupper(colnames(annual_data)[2:length(colnames(annual_data))])
  
  return(annual_data)
}

process_sheet_monthly <- function(sheet_name, file_path) {
  # Read the data
  data <- read_excel(file_path, sheet = sheet_name)
  
  # Convert the date column to Date type
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  
  # Extract month from the date
  data$Month <- month(data$Date, label = TRUE)
  
  # Summarize data to average annual monthly values
  monthly_data <- data %>%
    group_by(Month) %>%
    summarise(across(bobo:po, mean, na.rm = TRUE))
  
  colnames(monthly_data)[2:length(colnames(monthly_data))] <- 
    toupper(colnames(monthly_data)[2:length(colnames(monthly_data))])
  
  return(monthly_data)
}

# Path to the Excel file
file_path <- "Data/cli_data_bf.xlsx"

# Process each sheet
sheets <- c("tx", "tn", "rs", "rh", "ws", "et0")
sheet_units <- c("degree*C", "degree*C", "MJ~m^-2~day^-1", "'%'", "m~s^-1", "mm~day^-1")
names(sheet_units) <- sheets
period <- 1988:2017
lperiod <- seq(min(period),max(period),by=2)
cols <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#B3DE69","#A65628","#F781BF","#999999") #brewer.pal(9, "Set1")

annual_data <- lapply(sheets, process_sheet_annual, file_path = file_path)
monthly_data <- lapply(sheets, process_sheet_monthly, file_path = file_path)

ctheme.plot <-  theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
                      axis.title.y = element_text(size = 14, margin = margin(r = 15)),        
                      axis.text = element_text(color = "black", size = 14),
                      axis.text.x = element_text(angle=90, vjust = 0.5),
                      legend.title = element_blank(),
                      legend.justification = "center",
                      legend.spacing = unit(0.25,"cm"),                      
                      legend.key.size = unit(2,"cm"),
                      legend.text = element_text(size = 14, color = "black"))

# Create a function to plot data
ann_plot_data <- function(data, variable_name, ll,stations) {
  ann_data_long <- data %>%
    pivot_longer(cols = -Year, names_to = "Station", values_to = "Value")
  
  ann_data_long$Station <- ordered(ann_data_long$Station, levels = toupper(stations))
  unit.label <- parse(text=sheet_units[variable_name])[[1]]
  tvarname <- ifelse(variable_name=="et0","ETo",variable_name)
  
  ggplot(ann_data_long, aes(x = Year, y = Value, color = Station)) +
    geom_line(linewidth = 0.6) + 
    scale_x_continuous(breaks=lperiod, labels=lperiod) +
    scale_color_manual(values = cols) +
    labs(title = bquote("("*.(ll)*") Daily annual average -"~italic(.(tvarname))),
         x = "", y = bquote(italic(.(tvarname))~"["*.(unit.label)*"]")) +
    theme_bw() + ctheme.plot +
    guides(color = guide_legend(nrow = 2))
}

mon_plot_data <- function(data, variable_name, ll, stations) {
  mon_data_long <- data %>%
    pivot_longer(cols = -Month, names_to = "Station", values_to = "Value")
  
  mon_data_long$Station <- ordered(mon_data_long$Station, levels = toupper(stations))
  unit.label <- parse(text=sheet_units[variable_name])[[1]]
  tvarname <- ifelse(variable_name=="et0","ETo",variable_name)
  
  ggplot(mon_data_long, aes(x = Month, y = Value, color = Station, group = Station)) +
    geom_line(linewidth = 0.6) + 
    scale_x_discrete(labels=month.abb) +
    scale_color_manual(values = cols) +
    labs(title = bquote("("*.(ll)*") Daily monthly average -"~italic(.(tvarname))),
         x = "", y = bquote(italic(.(tvarname))~"["*.(unit.label)*"]")) +
    theme_bw() + ctheme.plot +
    guides(color = guide_legend(nrow = 2))
}

# Plot each variable
ann_plot_list <- lapply(seq_along(annual_data), function(i) {
  ann_plot_data(annual_data[[i]], sheets[i], letters[i], bf.stations)
})

mon_plot_list <- lapply(seq_along(monthly_data), function(i) {
  mon_plot_data(monthly_data[[i]], sheets[i], letters[i], bf.stations)
})

#ann_plot_list[[4]]

grob <- ggarrange(plotlist = ann_plot_list, ncol = 2, nrow = 3,common.legend = T, legend = "bottom")
ggsave(paste0("graphs/0_cli_data_ann.png"), plot = grob, 
       width = 25, height = 25, dpi = 400, scale = 0.5, bg = "white")

grob <- ggarrange(plotlist = mon_plot_list, ncol = 2, nrow = 3,common.legend = T, legend = "bottom")
ggsave(paste0("graphs/0_cli_data_mon.png"), plot = grob, 
       width = 25, height = 25, dpi = 400, scale = 0.5, bg = "white")

