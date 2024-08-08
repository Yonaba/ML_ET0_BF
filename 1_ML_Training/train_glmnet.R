# Chargement des bibliothèques
library(tidymodels)
library(tidyverse)
library(glmnet)

# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# TRANING
stations_names <- unique(stations_data$stations)  # Définir les noms des stations

# Boucle sur chaque station
i <- 0
ns <- length(stations_names)

for (station in stations_names) {
  i <- i + 1
  cat("Processing station:", station, " (", i, "/", ns, ")\n")
  
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
  
  # Définir le modèle glmnet
  glmnet_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  # Créer le workflow
  glmnet_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(glmnet_model)
  
  # Définir la grille de recherche pour les hyperparamètres
  glmnet_grid <- grid_regular(penalty(), mixture(), levels = 10)
  
  cat("Grid length:", nrow(glmnet_grid), "\n")
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  glmnet_fit <- tune_grid(
    glmnet_workflow,
    resamples = cv_folds,
    grid = glmnet_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(glmnet_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_glmnet <- finalize_workflow(glmnet_workflow, best_params)
  glmnet_final_model <- fit(final_glmnet, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(glmnet_final_model, file = paste0("glmnet_model_", station, ".rds"))
}