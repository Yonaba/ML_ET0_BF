# Chargement des bibliothèques
library(tidymodels)
library(xgboost)
library(tidyverse)

# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# TRANING
stations_names=unique(stations_data$stations)
# Boucle sur chaque station
i=0
ns=length(stations_names)
for (station in stations_names) {
  i=i+1
  cat("Processing station:", station, " ( ", i,"/",ns," ) ","\n")
  
  # Filtrer les données pour la station courante
  station_data <- stations_data %>%
    filter(stations == station)
  
  # Division des données en ensembles d'entraînement et de test
  set.seed(123)
  data_split <- initial_split(station_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Prétraitement des données
  recipe <- recipe(et0 ~ tx + tn + rh + rs + ws, data = train_data) %>%
    step_normalize(all_predictors())
  
  # Définir le modèle XGBoost
  xgboost_model <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = 0.01,
    sample_size = 0.8,
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
    set_engine("xgboost") %>%
    set_mode("regression")
  
  # Créer le workflow
  xgboost_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(xgboost_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  # Finalize the mtry parameter
  mtry_final <- finalize(mtry(), train_data)
  xgboost_grid <- grid_regular(trees(), tree_depth(), min_n(), mtry_final, levels = 5)
  
  cat("Grid length:", nrow(xgboost_grid))
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  xgboost_fit <- tune_grid(
    xgboost_workflow,
    resamples = cv_folds,
    grid = xgboost_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(xgboost_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_xgboost <- finalize_workflow(xgboost_workflow, best_params)
  xgboost_final_model <- fit(final_xgboost, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(xgboost_final_model, file = paste0("xgboost_model_", station, ".rds"))
}