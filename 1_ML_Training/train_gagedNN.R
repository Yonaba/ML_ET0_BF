# Chargement des bibliothèques
library(tidymodels)
library(nnet)
library(tidyverse)

# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# TRANING
stations_names <- unique(stations_data$stations)  # Définir les noms des stations
performance_metrics <- data.frame()  # Initialiser un dataframe pour les métriques de performance

# Boucle sur chaque station
i <- 0
ns <- length(stations_names)

for (station in stations_names) {
  i <- i + 1
  cat("Processing station:", station, " (", i, "/", ns, ")\n")
  
  # Filtrer les données pour la station courante
  station_data <- stations_data %>%
    dplyr::filter(stations == station)
  
  # Division des données en ensembles d'entraînement et de test
  set.seed(123)
  data_split <- initial_split(station_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Prétraitement des données
  recipe <- recipe(et0 ~ tx + tn + rh + rs + ws, data = train_data) %>%
    step_normalize(all_predictors())
  
  # Définir le modèle de réseaux de neurones baggés
  bagged_nn_model <- bag_mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("regression")
  
  # Créer le workflow
  bagged_nn_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(bagged_nn_model)
  
  # Définir la grille de recherche pour les hyperparamètres
  bagged_nn_grid <- grid_regular(
    hidden_units(range = c(1, 10)),
    penalty(range = c(0.01, 0.1)),
    epochs(range = c(50, 200)),
    levels = 5
  )
  
  cat("Grid length:", nrow(bagged_nn_grid), "\n")
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  bagged_nn_fit <- tune_grid(
    bagged_nn_workflow,
    resamples = cv_folds,
    grid = bagged_nn_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sauvegarder les résultats de la validation croisée
  saveRDS(bagged_nn_fit, file = paste0("cv_results_bagged_nn_", station, ".rds"))
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(bagged_nn_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_bagged_nn <- finalize_workflow(bagged_nn_workflow, best_params)
  bagged_nn_final_model <- fit(final_bagged_nn, data = train_data)
  
  # Faire des prédictions sur l'ensemble de test
  final_predictions <- predict(bagged_nn_final_model, test_data) %>%
    bind_cols(test_data)
  
  # Calculer les métriques de performance
  rmse_value <- rmse(final_predictions, truth = et0, estimate = .pred)
  rsq_value <- rsq(final_predictions, truth = et0, estimate = .pred)
  
  # Sauvegarder les métriques de performance
  performance_metrics <- rbind(performance_metrics, data.frame(
    station = station,
    rmse = rmse_value$.estimate,
    rsq = rsq_value$.estimate
  ))
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(bagged_nn_final_model, file = paste0("bagged_nn_model_", station, ".rds"))
}

# Visualiser les performances du modèle à travers les stations
ggplot(performance_metrics, aes(x = station, y = rmse)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "RMSE across Stations", x = "Station", y = "RMSE") +
  theme_minimal()

ggplot(performance_metrics, aes(x = station, y = rsq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "R² across Stations", x = "Station", y = "R²") +
  theme_minimal()