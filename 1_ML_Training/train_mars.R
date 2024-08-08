# Chargement des bibliothèques
library(tidymodels)
library(earth)
library(tidyverse)

# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# TRAINING MODEL
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
  
  # Définir le modèle MARS
  mars_model <- mars(
    num_terms = tune(),
    prod_degree = tune()
  ) %>%
    set_engine("earth") %>%
    set_mode("regression")
  
  # Créer le workflow
  mars_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(mars_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  # Finalize the parameters
  finalized_num_terms <- finalize(num_terms(), train_data)
  finalized_prod_degree <- finalize(prod_degree(), train_data)
  
  mars_grid <- grid_regular(finalized_num_terms, finalized_prod_degree, levels = 20)
  
  cat("Grid length:", nrow(mars_grid), "\n")
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  mars_fit <- tune_grid(
    mars_workflow,
    resamples = cv_folds,
    grid = mars_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(mars_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_mars <- finalize_workflow(mars_workflow, best_params)
  mars_final_model <- fit(final_mars, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(mars_final_model, file = paste0("mars_model_", station, ".rds"))
}