# Chargement des bibliothèques
library(tidymodels)
library(kernlab)
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
  
  # Définir le modèle SVM avec noyau polynomial
  svm_poly_model <- svm_poly(
    cost = tune(),
    degree = tune()
  ) %>%
    set_engine("kernlab") %>%
    set_mode("regression")
  
  # Créer le workflow
  svm_poly_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(svm_poly_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  #svm_rbf_random_grid <- grid_random(parameters(svm_rbf_model), size = 25)
  svm_poly_grid <- grid_regular(cost(), degree(), levels = 10)
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  svm_poly_fit <- tune_grid(
    svm_poly_workflow,
    resamples = cv_folds,
    grid = svm_poly_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(svm_poly_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_svm_poly <- finalize_workflow(svm_poly_workflow, best_params)
  svm_poly_final_model <- fit(final_svm_poly, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(svm_poly_final_model, file = paste0("svm_poly_model_v2_", station, ".rds"))
  
}