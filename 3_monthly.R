Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)

# ml.models <- list.files(path="ML/", pattern = ".csv")
# ml.models <- str_extract(ml.models, '([[:alpha:]]+)')

bf.stations <- read.csv(file = paste0("bf_stations.csv"),header = T)
bf.stations <- bf.stations[order(bf.stations$Latitude, decreasing=T),]$shName

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

d2m <- function(df) {
  df$agg <- strftime(df$Date, format="%m-%Y")
  df$Date <- NULL
  df <- aggregate(.~agg, data=df, FUN=sum)
  return (df)
}

d2mav <- function(df) {
  df$agg <- strftime(df$Date, format="%m-%Y")
  df$Date <- NULL
  df <- aggregate(.~agg, data=df, FUN=sum)
  df$agg <- parse_date_time(df$agg, "my")
  df$agg <- strftime(df$agg, format="%m")
  df <- aggregate(.~agg, data=df, FUN=mean)
  return (df)
}

obs <- read_xlsx("Data/cli_data_bf.xlsx", sheet = "et0")
nobs <- nrow(obs)

mobs <- d2m(obs)
mavobs <- d2mav(obs)[,-1]


mav.df <- data.frame(matrix(ncol=4, nrow=0))
colnames(mav.df) <- c("source", "mon", "values", "station")
mavobs <- data.frame(source = "OBS", 
                     mon = rep(1:12,n = length(colnames(obs))-1), 
                     stack(mavobs))
colnames(mavobs) <- colnames(mav.df)

plist <- list()
cp <- 0

for (model in ml.models) {
  #model <- ml.models[1]
  print(paste0("Processing: ", model))

  sim <- read.csv(file = paste0("ML/", model,"_predictions.csv"),header = T)
  sim <- sim[1:nobs,]
  sim <- sim[,c("Date",colnames(obs)[-1])]
  
  mavsim <- d2mav(sim)[,-1]
  mavsim <- data.frame(source = model, 
                   mon = rep(1:12,n = length(colnames(obs))-1), 
                   stack(mavsim))
  colnames(mavsim) <- colnames(mav.df)
  mav.df <- rbind(mav.df, mavsim)
  
  msim <- d2m(sim)  
  nstations <- length(colnames(msim[,-1]))
  
  msim$agg <- parse_date_time(msim$agg, "my")
  msim$agg <- strftime(msim$agg, format="%m")
  agg <- as.numeric(msim$agg)
  msim <- stack(msim[,-1])$values
  msim <- data.frame(source = "ML Model", mon = rep(agg, nstations), values = msim)
  msim <- rbind(msim, data.frame(source = "Observations", mon = msim$mon, values = stack(mobs[,-1])$values))
  msim$mon <- ordered(msim$mon, levels = 1:12)
  
  #msim[,-1] <- (msim[,-1] - mobs[,-1])
  # nstations <- length(colnames(msim[,-1]))
  # msim$agg <- parse_date_time(msim$agg, "my")
  # msim$agg <- strftime(msim$agg, format="%m")
  # agg <- as.numeric(msim$agg)
  # msim <- stack(msim[,-1])$values
  # msim <- data.frame(mon = rep(agg, nstations), sim = msim)
  # msim$mon <- ordered(msim$mon, levels = 1:12)
  
  cp <- cp + 1
  plist[[cp]] <- ggplot(msim, aes(x = mon, y = values, fill = source)) +
    geom_violin(position=position_dodge(0.75), alpha = 0.5, width = 1.6, bw = 5) +
    geom_point(position=position_jitterdodge(0.15,0,0.5), size = 0.1, colour = "darkred", shape=20) +
    geom_boxplot(position=position_dodge(0.75), width=0.3, color="black", alpha=0.2, outlier.shape = NA) +
    scale_fill_manual(values=c("green","orange")) + ylim(0,250) +
    xlab(ifelse(cp>=10,"Months","")) +     
    ylab(ifelse(cp %in% c(1,4,7,10),expression("ETo [mm "*month^-1*"]"),"")) +
    scale_x_discrete(labels = month.abb, breaks=1:12) +
    labs(title = paste0("(",letters[cp],") ",lnames[model]), fill = "") +    
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
          axis.text = element_text(color = "black", size = 16),
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(size = 16, margin = margin(r = 12)),        
          axis.title.x = element_text(size = 16, margin = margin(t = 12)),
          legend.text = element_text(size = 16, colour = "black"),
          legend.key.size = unit(1.25,"cm"))    

}

grob <- ggarrange(plotlist = plist, nrow = 4, ncol = 3, common.legend = T, legend = "bottom")
ggsave(plot = grob, paste0("graphs/monthly_ETo.png"),
       width = 36, height = 27, units = "in", dpi = 300, scale = 0.55,bg="white")

mav.df <- rbind(mav.df, mavobs)
mav.df$source <- ordered(mav.df$source, levels = c(ml.models, "OBS"))
mav.df$mon <- ordered(mav.df$mon, levels = 1:12)

mavplist <- list()
cp <- 0

cols <- c("#80E365","#A18BDD","#86BBDA",
          "#D9D964","#7ADDAF","#D8DBBA",
          "#B44DE2","#D8855C","#D4ADC7",
          "#DB64A8","#89C8D8","#C25CD6",
          "black")

for (station in bf.stations) {
  #station <- "ouaga"
  df <- mav.df[mav.df$station == station,]
  cp <- cp+1
  
  mavplist[[cp]] <- ggplot(df, aes(x=mon, y=values, group = source)) +
    geom_line(data=df[df$source!="OBS",],aes(x=mon, y=values,colour=source), linewidth=0.8) + 
    geom_line(data=df[df$source=="OBS",],aes(x=mon,y=values,colour="OBS"),linetype="twodash",linewidth=1.1) +      
    xlab(ifelse(cp>=7,"Months","")) + 
    ylab(ifelse((cp %in% c(1,4,7)),expression("ETo [mm"~month^-1*"]"),"")) + ylim(80,200) +
    labs(title = paste0("(",letters[cp],") ",toupper(station)),
         colour = "") +
    scale_color_manual(values=cols) +
    scale_x_discrete(labels=month.abb, breaks=1:12) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
          #plot.subtitle = element_text(color = "black", size = 12, hjust=0),
          axis.text = element_text(color = "black", size = 14),
          axis.text.x = element_text(angle = 90),          
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
          legend.position = "bottom", legend.title = element_blank(),
          legend.key.size = unit(1.25,"cm"),
          legend.text = element_text(size = 14, color = "black")) +
    guides(colour = guide_legend(nrow = 2))
  
}

grob <- ggarrange(plotlist = mavplist, nrow = 3, ncol = 3, common.legend = T, legend = "bottom")
ggsave(plot = grob, paste0("graphs/avg_monthly_ETo.png"),
       width = 20, height = 15, units = "in", dpi = 300, scale = 0.7,bg="white")

print("finished.")
