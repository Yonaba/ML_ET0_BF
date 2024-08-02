Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)

lnames <- c("BT" = "Bagging Trees (BT)",
            #"DT" = "Decision Trees (DT)",
            #"BART" = "Bayesian Additive Reg. Trees (BART)",
            "KNN" = "k-Nearnest Neighbours (KNN)", 
            "GLM" = "Generalized Linear Model (GLM)",   
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

bf.stations <- read.csv(file = paste0("bf_stations.csv"),header = T)
bf.stations <- bf.stations[order(bf.stations$Latitude, decreasing=T),]$shName

# ml.models <- list.files(path="ML/", pattern = ".csv")
# ml.models <- str_extract(ml.models, '([[:alpha:]]+)')

d2ann <- function(df) {
  df$agg <- strftime(df$Date, format="%Y")
  df$Date <- NULL
  df <- aggregate(.~agg, data=df, FUN=sum)
  return (df)
}

period <- 1988:2017
lperiod <- seq(min(period), max(period),by=2)

obs <- read_xlsx("Data/cli_data_bf.xlsx", sheet = "et0")
obs <- d2ann(obs)[,-1]
cobs <- colnames(obs)
obs <- data.frame(source = "OBS", 
                  ann = rep(period,n = length(colnames(obs))-1), 
                  stack(obs))

mav.df <- data.frame(matrix(ncol=4, nrow=0))
colnames(mav.df) <- c("source", "ann", "values", "station")
colnames(obs) <- colnames(mav.df)

plist <- list()
cp <- 0
for (model in ml.models) {
  #model <- ml.models[1]
  print(paste0("Processing: ", model))

  sim <- read.csv(file = paste0("ML/", model,"_predictions.csv"),header = T)
  sim <- sim[,c("Date",cobs)]
  sim <- d2ann(sim)[,-1]
  sim <- data.frame(source = model, 
                    ann = rep(period,n = length(colnames(obs))-1), 
                    stack(sim))
  colnames(sim) <- colnames(mav.df)
  mav.df <- rbind(mav.df, sim)
}

mav.df <- rbind(mav.df, obs)

mav.df$source <- ordered(mav.df$source, levels = c(ml.models, "OBS"))
mav.df$ann <- ordered(mav.df$ann, levels = period)

plist <- list()
cp <- 0
#cols <- c("#DD55C3","#86BBDA","#9055E5","#D9D565","#C496D0","#DB8171","#D9D3C0","#83E36F","#86DFBC")
cols <- c("#80E365","#A18BDD","#86BBDA",
          "#D9D964","#7ADDAF","#D8DBBA",
          "#B44DE2","#D8855C","#D4ADC7",
          "#DB64A8","#89C8D8","#C25CD6",
          "black")

for (station in bf.stations) {
  #station <- "ouaga"
  df <- mav.df[mav.df$station == station,]
  cp <- cp+1
  
  plist[[cp]] <- ggplot(df, aes(x=ann, y=values, group = source)) +
    geom_line(data=df[df$source!="OBS",],aes(x=ann, y=values,colour=source), linewidth=0.8) + 
    geom_line(data=df[df$source=="OBS",],aes(x=ann,y=values,colour="OBS"),linetype="twodash",linewidth=1) +      
    xlab(ifelse(cp>=7,"Years","")) + 
    ylab(ifelse((cp %in% c(1,4,7)),expression("ETo [mm"~year^-1*"]"),"")) + ylim(1100,2100) +
    labs(title = paste0("(",letters[cp],") ",toupper(station)),
         colour = "") +
    scale_color_manual(values=cols) +
    scale_x_discrete(labels=lperiod, breaks=lperiod) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
          plot.subtitle = element_text(color = "black", size = 14, hjust=0),
          axis.text = element_text(color = "black", size = 14),
          axis.text.x = element_text(angle = 90, size = 12),          
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
          legend.position = "bottom", legend.title = element_blank(),
          legend.key.size = unit(1.25,"cm"),
          legend.text = element_text(size = 14, color = "black")) +
    guides(colour = guide_legend(nrow = 2))
  
}

grob <- ggarrange(plotlist = plist, nrow = 3, ncol = 3, common.legend = T, legend = "bottom")
ggsave(plot = grob, paste0("graphs/ann_ETo.png"),
       width = 20, height = 15, units = "in", dpi = 300, scale = 0.7,bg="white")

print("finished.")
