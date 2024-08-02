Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

library(readxl)
library(lubridate)
library(tidyverse)
library(tidymodels)
library(lightgbm)
library(bonsai)
library(kknn)
library(elmNNRcpp)
library(earth)
library(glmnet)
library(kernelshap)
library(shapviz)

# library(kernlab)
# library(DALEX)
# library(baguette)
# library(shapper)

# library(reticulate)
# use_python("C:/Users/ousmane.yonaba/AppData/Local/Programs/Python/Python311/")
# shapper::install_shap()

# Example dataset
#data(iris)
#data(ames, package = "modeldata")

lnames <- c("BT" = "Bagged Trees (BT)",
            #"DT" = "Decision Trees (DT)",
            #"BART" = "Bayesian Additive Reg. Trees (BART)",
            "KNN" = "k-Nearnest Neighbours (KNN)", 
            "GLM" = "Generalized Linear Model (GLM)",   
            "GAM" = "Generalized Additive Model (GAM)",             
            "MARS" = "Mult. Adaptative Reg. Splines (MARS)",            
            "GBoost" = "Gradient Boosting (GBoost)",
            "XGBoost" = "Extreme Gradient Boosting (XGBoost)",
            "ELM" = "Extreme Learning Machine (ELM)",
            "MLP" = "Multilater Perceptron (MLP)",
            "RF" = "Random Forest (RF)",
            "SVM" = "Support Vector Machine (SVM)",           
            "BNN" = "Bagging Neural Networks (BNN)")

bf.stations <- read.csv(file = paste0("bf_stations.csv"),header = T)
bf.stations <- bf.stations[order(bf.stations$Latitude, decreasing=T),]$shName

read_station <- function(fpath, station) {
  #fpath <- "Data/cli_data_bf.xlsx"
  #station <- "bobo"
  vars <- excel_sheets(fpath)
  df <- data.frame(Date = seq(ymd("1988-1-1"), ymd("2017-12-31"), by = "day"))
  df$stations <- station
  for (var in vars) {
    #var <- "tx"
    data <- read_xlsx(fpath, sheet = var)[,station]
    df <- cbind(df, data)
  }
  colnames(df)[3:8] <- vars
  return (df)
}

read_data <- function(fpath, stations) {
  vars <- excel_sheets(fpath)
  df <- data.frame(matrix(nrow=0, ncol=length(vars)+2))
  colnames(df) <- c("Date","stations",vars)
  for (station in stations) {
    #print(paste0("rr ",station))
    df <- rbind(df, read_station(fpath, station))
  }
  return (df)
}

data <- read_data("Data/cli_data_bf.xlsx", bf.stations)
data$j <- yday(data$Date)
data <- data[,-1]
data <- aggregate(.~j+stations, data, mean)

vars <- tail(colnames(data),6)
ml.models <- names(lnames)
set.seed(123)

cp <- 0
sv.plist <- list()

for (model in ml.models) {
  #model <- "ELM"
  print(model)
  rds <- list.files(path = paste0("ML/",model,"/"), pattern=".rds")
  
  cp <- cp + 1
  pshapl <- list()
  
  for (station in unique(data$stations)) {
    #station <- "ouaga"
    print(paste0("Model: ",model, " / Shapviz: ",station))
    model.rds <- rds[grep(station,rds)]
    model.rds <- readRDS(paste0("ML/",model,"/",model.rds))
    
    sdata <- data[data$stations == station, vars]
    elmdata <- data.matrix(apply(sdata[,1:5],2,scale))
    bgelmdata <- elmdata[sample(1:nrow(elmdata),100),]
    
    if (model == "ELM") {
      pshap <- permshap(model.rds$model, X = elmdata, 
                        bg_X = bgelmdata, 
                        pred_fun = elm_predict, verbose = T)
    } else {
      pshap <- permshap(model.rds, X = sdata[,1:5], 
                        bg_X = sdata[sample(1:nrow(sdata),100),], 
                        pred_fun = predict, verbose = T)  
    }
  
    
    pshapl[[station]] <- shapviz(pshap)
    
  }
  
  mshap <- mshapviz(pshapl)
  saveRDS(mshap, file = paste0("SHAP_ML/",model,"_mshapviz.rds"))
  
  # sv.plist[[length(sv.plist)+1]] <- sv_importance(mshap, kind = "bar") +
  #   labs(title = paste0("(",letters[cp],") ",model)) + theme_bw() + 
  #   theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
  #         axis.text = element_text(color = "black", size = 12),
  #         axis.title.y = element_text(size = 12, margin = margin(r = 12)),        
  #         axis.title.x = element_text(size = 12, margin = margin(t = 12)))
}


