# Chargement des bibliothèques
library(tidymodels)
library(lightgbm)
library(tidyverse)

# DATA IMPORTATION
stations_data <- read.csv(file = "stations_clim_data.csv")

# TRANING
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
  
  # Définir le modèle SVM avec noyau RBF
  gradient_boosted_model <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = 0.01,
    sample_size = 0.8,
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
    set_engine("lightgbm") %>%
    set_mode("regression")
  
  # Créer le workflow
  gradient_boosted_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(gradient_boosted_model)
  
  # Définir la grille de recherche aléatoire pour les hyperparamètres
  #set.seed(123)
  #svm_rbf_random_grid <- grid_random(parameters(svm_rbf_model), size = 25)
  gradient_boosted_grid <- grid_regular(
    trees(),
    tree_depth(),
    finalize(mtry(), train_data),
    min_n(),
    levels = 5
  )
  cat("Parameters set length:",nrow(gradient_boosted_grid))	
  
  # Validation croisée
  cv_folds <- vfold_cv(train_data, v = 5)
  
  # Entraîner et évaluer le modèle
  gradient_boosted_fit <- tune_grid(
    gradient_boosted_workflow,
    resamples = cv_folds,
    grid = gradient_boosted_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  # Sélectionner le meilleur modèle
  best_params <- select_best(gradient_boosted_fit, metric = "rmse")
  
  # Finaliser et ajuster le modèle final
  final_gradient_boosted <- finalize_workflow(gradient_boosted_workflow, best_params)
  final_gradient_final_model <- fit(final_gradient_boosted, data = train_data)
  
  # Sauvegarder le modèle final dans un fichier RDS
  saveRDS(final_gradient_final_model, file = paste0("gradient_boosted_model_", station, ".rds"))  
}