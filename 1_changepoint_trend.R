Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(changepoint.geo)
library(trend)

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
    summarise(across(bobo:po, mean, na.rm = TRUE)) %>%
    select(-c(Year)) %>%
    rename_with(toupper)
  
  return(annual_data)
}

make_mvset <- function(station, annual_data, nperiod, cnames) {
  df <- data.frame(matrix(nrow=nperiod,ncol=0))
  #station <- "BOBO"
  for (i in 1:length(annual_data)) {
    ann <- annual_data[[i]]
    df <- cbind(df, ann[,station])
  }
  colnames(df) <- cnames
  return (df)
}

# Path to the Excel file
file_path <- "Data/cli_data_bf.xlsx"

# Process each sheet
sheets <- c("tx", "tn", "rs", "rh", "ws", "et0")
sheet_units <- c("degree*C", "degree*C", "MJ~m^-2~day^-1", "'%'", "m~s^-1", "mm~day^-1")
names(sheet_units) <- sheets

period <- 1988:2017
cols <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#B3DE69","#A65628","#F781BF","#999999") #brewer.pal(9, "Set1")

annual_data <- lapply(sheets, process_sheet_annual, file_path = file_path)

combs <- expand.grid(head(sheets,5), "et0")
combs <- apply(combs, 2, paste)
stations <- colnames(annual_data[[1]])

cp.df <- data.frame(matrix(nrow=0, ncol=nrow(combs)+1))
combvar <- apply(combs, 1, paste, collapse = "-" )
colnames(cp.df) <- c("station",combvar)

trend.df <- data.frame(matrix(nrow=0, ncol=(ncol(cp.df)-1)*2+1))
colnames(trend.df) <- c("station",apply(expand.grid(c("rho","pmk"), combvar),1,paste,collapse=" "))

for (s in toupper(bf.stations)) {
  #s <- "BOBO"
  print(paste0("Processing: ", s))
  df <- make_mvset(s, annual_data, length(period), sheets)
  scp <- c(s)
  trp <- c(s)
  for (i in 1:nrow(combs)) {
    #i <- 1
    #print(i)
    comb <- combs[i,]
    df0 <- df[, comb]
    cp <- geomcp(as.matrix(df0), penalty = "MBIC", pen.value = 0, test.stat = "Normal", msl = 5,
                 nquantiles = 1,MAD=FALSE,ref.vec='Default',ref.vec.value=0)
    cps <- c()
    if (length(cp@dist.cpts)>0) {
      cps <- cp@dist.cpts + 1988-1
      pl <- plot(cp,plot.type='series',show.series=1:2,add.mappings=TRUE) + theme_bw()
      ggsave(plot = pl, paste0("graphs/changepoint/",s,"_",paste0(comb,collapse="_"),"_cps.png"),
             width = 15, height = 10, units = "in", dpi = 400, scale = 0.8,bg="white")
    }
    cps <- paste(cps, collapse=", ")
    scp <- append(scp, cps)
    
    ct <- cor.test(df0[,1], df0[,2], method = "spearman", 
             alternative= "two.sided", conf.level = 0.95, continuity = T)
    ct0 <- sprintf("%.2f",ct$estimate)
    ct1 <- sprintf("%.4f",ct$p.value)
    ct1 <- ifelse(ct1=="0.0000","<0.0001",ct1)
    ct <- paste0(ct0," (",ct1,")")
    
    pmk <- partial.mk.test(df0[,1], df0[,2], alternative = "two.sided")
    pt0 <- sprintf("%.2f",pmk$estimates[1])
    pt1 <- sprintf("%.4f",pmk$p.value)
    pt1 <- ifelse(pt1=="0.0000","<0.0001",pt1)
    pt <- paste0(pt0," (",pt1,")")
    
    trp <- append(trp, c(ct, pt))
  }
  cp.df[nrow(cp.df)+1,] <- scp
  trend.df[nrow(trend.df)+1,] <- trp
}

write.csv(cp.df, file = paste0("tables/mvar_changepoint.csv"), row.names = F)
write.csv(trend.df, file = paste0("tables/mvar_trend.csv"), row.names = F)

print("finished.")
