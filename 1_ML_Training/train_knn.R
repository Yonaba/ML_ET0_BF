# Chargement des bibliothèques
library(tidymodels)
library(kknn)
library(tidyverse)


# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# Train model
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
  
  # Définir le modèle KNN
  knn_model <- nearest_neighbor(
    neighbors = tune(),
    weight_func = tune()
  ) %>%
    set_engine("kknn") %>%
    set_mode("regression")
  
  # Créer le workflow
  knn_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(knn_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  # Finalize the parameters
  knn_grid <- grid_regular(neighbors(), weight_func(), levels = 10)
  
  cat("Grid length:", nrow(knn_grid), "\n")
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  knn_fit <- tune_grid(
    knn_workflow,
    resamples = cv_folds,
    grid = knn_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(knn_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_knn <- finalize_workflow(knn_workflow, best_params)
  knn_final_model <- fit(final_knn, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(knn_final_model, file = paste0("knn_model_", station, ".rds"))
}

