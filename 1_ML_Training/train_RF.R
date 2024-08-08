# Chargement des bibliothèques
library(tidymodels)
library(ranger)
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
    filter(stations == station) %>%
    select(-stations, -Date)
  
  # Division des données en ensembles d'entraînement et de test
  set.seed(123)
  data_split <- initial_split(station_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Prétraitement des données
  recipe <- recipe(et0 ~ tx + tn + rh + rs + ws, data = train_data) %>%
    step_normalize(all_predictors())
  
  # Définir le modèle Random Forest
  rand_forest_model <- rand_forest(
    trees = tune(),
    min_n = tune(),
    mtry = tune()
  ) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  
  # Créer le workflow
  rand_forest_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rand_forest_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  # Finalize the mtry parameter
  mtry_final <- finalize(mtry(), train_data)
  rand_forest_grid <- grid_regular(trees(), min_n(), mtry(range = c(1, ncol(train_data) - 1)), levels = 5)
  
  cat("Grid length:", nrow(rand_forest_grid), "\n")
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 4)
  
  # Entraîner et évaluer le modèle
  rand_forest_fit <- tune_grid(
    rand_forest_workflow,
    resamples = cv_folds,
    grid = rand_forest_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(rand_forest_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_rand_forest <- finalize_workflow(rand_forest_workflow, best_params)
  rand_forest_final_model <- fit(final_rand_forest, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(rand_forest_final_model, file = paste0("rand_forest_model_", station, ".rds"))
}