Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

# Load necessary libraries
library(readxl)
library(stringr)
library(ggplot2)
library(ggpointdensity)
library(ggpubr)
library(viridis)
library(hydroGOF)

#ml.models <- list.files(path="ML/", pattern = ".csv")
#ml.models <- str_extract(ml.models, '([[:alpha:]]+)')

metrics <- c("R2","MAE","RMSE","NRMSE %","KGE")
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
diffrange <- function(v) return (max(v) - min(v))

obs <- read_xlsx("Data/cli_data_bf.xlsx", sheet = "et0")[,-1]
sobs <- stack(obs)$values

plist <- list()
cp <- 0

for (model in ml.models) {
  #model <- ml.models[1]
  print(paste0("Processing: ", model))
  
  sim <- read.csv(file = paste0("ML/", model,"_predictions.csv"),header = T)[,-1]
  sim <- sim[1:nrow(obs),]
  sim <- sim[,colnames(obs)]
  df <- data.frame(obs = sobs, sim = stack(sim)$values)
  df$obs[df$obs<0] <- 0

  ggofs <- gof(sim, obs, na.rm=T, norm="maxmin", method="2012")
  gofs.median <- apply(ggofs,1,median)
  gofs <- gofs.median[metrics]
  gofs <- sprintf("%0.2f", gofs)
  names(gofs) <- metrics
  
  # sgofs <- gof(stack(sim)$values,stack(obs)$values,na.rm=T, norm="maxmin", method="2012")
  # maxmin <- diffrange(stack(obs)$values)
  # 
  # gpi <-  as.numeric(-1*((sgofs["NRMSE %",]/100)-(gofs.median["NRMSE %"]/100)) + 
  #         -1*((sgofs["MAE",]/1)-(gofs.median["MAE"]/1)) + 
  #          1*((sgofs["R2",])-(gofs.median["R2"])) +
  #          1*((sgofs["NSE",])-(gofs.median["NSE"])))
  # 
  # print(paste0(model," : gpi = ",sprintf("%0.3f", gpi)))
  
  msize <- 3.5
  ystart <- 12
  xstart <- 0
  ysep <- 0.8
  
  cp <- cp + 1
  plist[[cp]] <- ggplot(df, aes(x = obs, y = sim)) +
    geom_pointdensity(na.rm=T,show.legend=F) +
    geom_abline(slope = 1, intercept = 0, colour = "black", linetype = "dashed") +
    #scale_color_viridis() +
    coord_cartesian(xlim = c(0, 12), ylim = c(0, 12)) +
    xlab(ifelse(cp>=10,expression("Observed ETo [mm"~day^-1*"]"),"")) +
    ylab(ifelse(cp %in% c(1,4,7,10),expression("Simulated ETo [mm"~day^-1*"]"),"")) +    
    labs(title = paste0("(",letters[cp],") ",lnames[model])) +
    annotate(geom = "text", x = xstart, y = ystart, hjust = 0, size = msize,
             label = bquote(R^2*" = "*.(gofs["R2"]))) + 
    annotate(geom = "text", x = xstart, y = ystart-1*ysep, hjust = 0, size = msize,
           label = bquote("MAE = "*.(gofs["MAE"])*" [mm "*day^-1*"]")) +
    annotate(geom = "text", x = xstart, y = ystart-2*ysep, hjust = 0, size = msize,
             label = bquote("RMSE = "*.(gofs["RMSE"])*" [mm "*day^-1*"], NRMSE = "*.(gofs["NRMSE %"])*"%")) +    
    annotate(geom = "text", x = xstart, y = ystart-3*ysep, hjust = 0, size = msize,
             label = bquote("KGE = "*.(gofs["KGE"]))) +
    theme_bw() + 
    theme(plot.title = element_text(color = "black", size = 14, face = "bold"),
            axis.text = element_text(color = "black", size = 12),
            axis.title.y = element_text(size = 12, margin = margin(r = 12)),        
            axis.title.x = element_text(size = 12, margin = margin(t = 12)))
}

grob <- ggarrange(plotlist = plist, nrow = 4, ncol = 3)
ggsave(plot = grob, paste0("graphs/scatterETo.png"),
       width = 25, height = 25, units = "in", dpi = 300, scale = 0.58,bg="white")

print("finished.")
