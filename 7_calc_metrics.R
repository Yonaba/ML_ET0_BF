Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(stringr)
library(ggplot2)
library(hydroGOF)

#ml.models <- list.files(path="ML/", pattern = ".csv")
#ml.models <- str_extract(ml.models, '([[:alpha:]]+)')

d2m <- function(df) {
  df$agg <- strftime(df$Date, format="%m-%Y")
  df$Date <- NULL
  df <- aggregate(.~agg, data=df, FUN=sum)
  return (df)
}

d2ann <- function(df) {
  df$agg <- strftime(df$Date, format="%Y")
  df$Date <- NULL
  df <- aggregate(.~agg, data=df, FUN=sum)
  return (df)
}

metrics <- c("R2","MAE","RMSE","NRMSE %","KGE")
lnames <- c("BT" = "Bagging Trees (BT)",
            #"DT" = "Decision Trees (DT)",
            #"BART" = "Bayesian Additive Reg. Trees (BART)",
            "KNN" = "k-Nearnest Neighbours (KNN)", 
            
            "GLMNET" = "Generalized Linear Model (GLM)",   
            "GAM" = "Generalized Additive Model (GAM)",             
            "MARS" = "Mult. Adaptive Reg. Splines (MARS)",            
            "GBoost" = "Gradient Boosting (GBoost)",
            "XGBoost" = "Extreme Gradient Boosting (XGBoost)",
            "ELM" = "Extreme Learning Machine (ELM)",
            "MLP" = "Multilater Perceptron (MLP)",
            "RF" = "Random Forest (RF)",
            "SVM" = "Support Vector Machine (SVM)",           
            "BNN" = "Bagging Neural Networks (BNN)")

ml.models <- names(lnames)
diffrange <- function(v) return (max(v) - min(v))

obs <- read_xlsx("Data/cli_data_bf.xlsx", sheet = "et0")
dobs <- stack(obs[,-1])$values
mobs <- stack(d2m(obs)[,-1])$values
yobs <- stack(d2ann(obs)[,-1])$values
            

ddf <- mdf <- ydf <- data.frame(matrix(nrow = 0, ncol = length(metrics)+2), check.names = F)
colnames(ddf) <- colnames(mdf) <- colnames(ydf) <- c("Timescale", "model", metrics)

for (model in ml.models) {
  #model <- ml.models[1]
  print(paste0("Processing: ", model))
  
  sim <- read.csv(file = paste0("ML/", model,"_predictions.csv"),header = T)
  sim <- sim[1:nrow(obs),]
  sim <- sim[,colnames(obs)]
  
  dsim <- stack(sim[,-1])$values
  msim <- stack(d2m(sim)[,-1])$values
  ysim <- stack(d2ann(sim)[,-1])$values
  
  dgofs <- gof(dsim, dobs, na.rm=T, norm="maxmin", method="2012")
  mgofs <- gof(msim, mobs, na.rm=T, norm="maxmin", method="2012")
  ygofs <- gof(ysim, yobs, na.rm=T, norm="maxmin", method="2012")
  
  dgofs <- dgofs[metrics,]
  mgofs <- mgofs[metrics,]
  ygofs <- ygofs[metrics,]
  
  dgofs <- sprintf("%0.2f", dgofs)
  mgofs <- sprintf("%0.2f", mgofs)
  ygofs <- sprintf("%0.2f", ygofs)
  
  names(dgofs) <- names(mgofs) <- names(ygofs) <- metrics
  
  ddf[nrow(ddf)+1,] <- c("Daily",model,dgofs)
  mdf[nrow(mdf)+1,] <- c("Monthly",model,mgofs)
  ydf[nrow(ydf)+1,] <- c("Annual",model,ygofs)  
}

df <- rbind(ddf, mdf, ydf)
write.csv(df, file = paste0("tables/eval_perf_ml.csv"), row.names = F)

print("finished.")
