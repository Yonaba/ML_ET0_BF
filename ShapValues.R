Sys.setenv(TZ="UTC")
setwd("D:/Recherche/Article_ET0_ML/")

library(tidyverse)
library(tidymodels)
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

ml_data <- read.csv(file = "Data/stations_clim_data.csv")%>%
  dplyr::filter(stations=="bobo")%>%
  select(-Date,-stations)

set.seed(123)
data_split <- initial_split(ml_data, prop = 0.5)
train_data <- training(data_split)
test_data <- testing(data_split)

m1 <- readRDS("ML/BT/bagging_model_bobo.rds")
m2 <- readRDS("ML/BT/bagging_model_ouaga.rds")
m3 <- readRDS("ML/BT/bagging_model_dori.rds")

pshap1 <- permshap(m1, X = train_data[,1:5], 
         bg_X = train_data[sample(1:nrow(train_data),50),], 
         pred_fun = predict,
         verbose = T)

pshap2 <- permshap(m2, X = train_data[,1:5], 
                   bg_X = train_data[sample(1:nrow(train_data),50),], 
                   pred_fun = predict,
                   verbose = T)

pshap3 <- permshap(m2, X = train_data[,1:5], 
                   bg_X = train_data[sample(1:nrow(train_data),50),], 
                   pred_fun = predict,
                   verbose = T)

shap1 <- shapviz(pshap1)
shap2 <- shapviz(pshap2)
shap3 <- shapviz(pshap3)

mshap <- c("bobo" = shap1, "ouaga" = shap2, "dori" = shap3)

sv_importance(mshap, kind = "beeswarm")
sv_force(mshap, row_id = 1:273, max_display = 5)
sv_dependence(mshap, v=c("rh", "rs","ws"))

individual_variable_effect(final_model, data = train_data[,-6], 
                           predict_function = predict,
                           new_observation = test_data[1:10,-6], 
                           nsamples = 50)

explainer <- DALEX::explain(
  model = final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$et0,
  predict_function_target_column="et0",
  type="regression"
)

library(shapviz)
bd <- explainer |> 
  predict_parts(test_data, N = NULL, keep_distributions = FALSE) |> 
  shapviz()

model_performance(explainer)

shapviz(explainer, X_pred = test_data[,1:5], X = test_data)

sv_importance(bd, kind = "beeswarm")

sv_waterfall(bd)
sv_force(bd)
sv_importance(bd)
sv_dependence(bd,v = "et0")
#sv_dependence2D(bd)
#sv_interaction(bd)
DALEX::variable_importance(explainer)

