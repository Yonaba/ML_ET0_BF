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
library(ggpubr)
library(patchwork)

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

make_local_dependence_plot <- function(mshap, var, title) {
  ldplot <- sv_dependence(mshap, v=var, color_var = NULL) +
    plot_layout(ncol = 1) +
    plot_annotation(title = paste0(title),caption = '',
                    theme = theme_bw() + 
                      theme(plot.title = element_text(size = 14, face = "bold", colour = "black"),
                            axis.text = element_text(color = "black", size = 12),
                            axis.title.y = element_text(size = 12, margin = margin(r = 12)),
                            axis.title.x = element_text(size = 12, margin = margin(t = 12))))
  return (ldplot)
}

plist <- list()

mshap <- readRDS(paste0("SHAP_ML/PM_mshapviz.rds"))
p <- sv_importance(mshap, kind = "bar", bar_type = "stack", 
                   viridis_args = list(begin = 0.25, end = 0.85, option = "turbo")) +
  labs(title = paste0("(a) FAO-56 PM")) + theme_bw() + 
  theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
        axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = 12)),
        axis.title.x = element_text(size = 12, margin = margin(t = 12)),
        legend.position = "bottom",
        legend.key.size = unit(0.8,"cm"),
        legend.text = element_text(size = 12, color = "black")) +
  guides(fill = guide_legend(nrow = 1))

p$data$feature <- ordered(p$data$feature, levels=rev(c("tx","tn","rh","rs", "ws")))
p$data$ind <- factor(toupper(p$data$ind))
p$data$ind <- ordered(p$data$ind, levels=toupper(bf.stations))
p$labels$x <- NULL
p$labels$y <- "Variables" 

plist[[1]] <- p
plist[[2]] <- ggplot() + theme_void()
plist[[3]] <- ggplot() + theme_void()
cp <- length(plist)

for (model in ml.models) {
  #model <- "BT"
  print(paste0("Processing: ",model))
  cp <- cp + 1
  mshap <- readRDS(paste0("SHAP_ML/",model,"_mshapviz.rds"))
  
  print(paste0("      Calculating Feature importace"))  
  p <- sv_importance(mshap, kind = "bar", bar_type = "stack", 
                     viridis_args = list(begin = 0.25, end = 0.85, option = "turbo")) +
      labs(title = paste0("(",letters[cp-2],") ",model)) + theme_bw() + 
      theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
            axis.text = element_text(color = "black", size = 12),
            axis.title.y = element_text(size = 12, margin = margin(r = 12)),
            axis.title.x = element_text(size = 12, margin = margin(t = 12)),
            legend.position = "bottom",
            legend.key.size = unit(0.8,"cm"),
            legend.text = element_text(size = 12, color = "black")) +
    guides(fill = guide_legend(nrow = 1))
  
  p$data$feature <- ordered(p$data$feature, levels=rev(c("tx","tn","rh","rs", "ws")))
  p$data$ind <- factor(toupper(p$data$ind))
  p$data$ind <- ordered(p$data$ind, levels=toupper(bf.stations))
  
  if (cp<13) p$labels$x <- NULL
  if (cp %in% c(4,7,10,13)) p$labels$y <- "Variables" 
  plist[[cp]] <- p
  
  mm <- sapply(toupper(bf.stations),function(x) NULL)
  for (i in names(mm)) mm[[i]] <- mshap[[tolower(i)]]
  mm <- mshapviz(mm)
  
  pi <- sv_importance(mm, kind = "beeswarm", bar_type = "dodge")
  pi <- pi + plot_annotation(title = paste0(lnames[model]),
                    caption = '',
                    theme = theme_bw() +
                      theme(plot.title = element_text(size = 14, face = "bold", colour = "black"),
                            axis.text = element_text(color = "black", size = 12),
                            axis.title.y = element_text(size = 12, margin = margin(r = 12)),
                            axis.title.x = element_text(size = 12, margin = margin(t = 12))))
  ggsave(plot = pi, paste0("graphs/beeswarm/",model,"_beeswarm.png"),
         width = 20, height = 20, units = "in", dpi = 300, scale = 0.5,bg="white")

  print(paste0("      Drawing Local dependence plots"))
  ld_tx <- sv_dependence(mm, v="tx", color_var = NULL) + plot_layout(ncol = 1)
  ld_tn <- sv_dependence(mm, v="tn", color_var = NULL) + plot_layout(ncol = 1)
  ld_rh <- sv_dependence(mm, v="rh", color_var = NULL) + plot_layout(ncol = 1)
  ld_rs <- sv_dependence(mm, v="rs", color_var = NULL) + plot_layout(ncol = 1)
  ld_ws <- sv_dependence(mm, v="ws", color_var = NULL) + plot_layout(ncol = 1)

  ld_grob <- wrap_plots(ld_tx, ld_tn, ld_rh, ld_rs, ld_ws, nrow = 1, ncol= 5) +
    plot_annotation(title = paste0(lnames[model]),
                    theme = theme_bw() +
                      theme(plot.title = element_text(size = 20, face = "bold", colour = "black"),
                            axis.text = element_text(color = "black", size = 12),
                            axis.title.y = element_text(size = 12, margin = margin(r = 12)),
                            axis.title.x = element_text(size = 12, margin = margin(t = 12))))

  ggsave(plot = ld_grob, paste0("graphs/local_dependence/",model,"_ldplot.png"),
         width = 30, height = 45, units = "in", dpi = 300, scale = 0.5,bg="white")
  
}

print(paste0("Drawing Overall importance")) 
hp <- 0.194
grob <- ggarrange(plotlist = plist, nrow = 5, ncol = 3, common.legend = T, legend = "bottom",
                  widths = c(0.33,0.33,0.33), heights = c(rep(hp,4),1-4*hp))
ggsave(plot = grob, paste0("graphs/mean_shap.png"),
       width = 20, height = 25, units = "in", dpi = 300, scale = 0.5,bg="white")
