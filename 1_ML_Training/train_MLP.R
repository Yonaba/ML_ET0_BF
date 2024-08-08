# Chargement des bibliothèques
library(tidymodels)
library(nnet)
library(tidyverse)


# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# TRAINING
stations_names=unique(stations_data$stations)

# Boucle sur chaque station
i=0
ns=length(stations_names)
for (station in stations_names) {
  i=i+1
  cat("Processing station:", station, " ( ", i,"/",ns," )\n" )
  
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
  
  # Définir le modèle MLP
  mlp_model <- mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("regression")
  
  # Créer le workflow
  mlp_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(mlp_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  # Finalize the parameters
  
  mlp_grid <- grid_regular(hidden_units(), penalty(),epochs(), levels = 10)
  
  cat("Grid length:", nrow(mlp_grid), "\n")
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  mlp_fit <- tune_grid(
    mlp_workflow,
    resamples = cv_folds,
    grid = mlp_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(mlp_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_mlp <- finalize_workflow(mlp_workflow, best_params)
  mlp_final_model <- fit(final_mlp, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(mlp_final_model, file = paste0("mlp_model_", station, ".rds"))
}