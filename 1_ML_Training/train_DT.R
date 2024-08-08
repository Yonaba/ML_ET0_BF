# Chargement des bibliothèques
library(tidymodels)
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
  
  # Définir le modèle Decision Tree
  decision_tree_model <- decision_tree(
    tree_depth = tune(),
    min_n = tune()
  ) %>%
    set_engine("rpart") %>%
    set_mode("regression")
  
  # Créer le workflow
  decision_tree_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(decision_tree_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  set.seed(123)
  decision_tree_grid <- grid_regular(tree_depth(), min_n(), levels = 10)
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  decision_tree_fit <- tune_grid(
    decision_tree_workflow,
    resamples = cv_folds,
    grid = decision_tree_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(decision_tree_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_decision_tree <- finalize_workflow(decision_tree_workflow, best_params)
  decision_tree_final_model <- fit(final_decision_tree, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(decision_tree_final_model, file = paste0("decision_tree_model_", station, ".rds"))
}