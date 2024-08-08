# Chargement des bibliothèques

library(tidymodels)
library(mgcv)
library(tidyverse)


# DATA IMPORTATION


# TRANING
stations_data <- read.csv(file = "stations_clim_data.csv")%>%
  mutate(Date = as.Date(Date),
         stations = as.factor(stations))


train_gam_model <- function(stations_data) {
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
      filter(stations == station)
    
    # Division des données en ensembles d'entraînement et de test
    set.seed(123)
    data_split <- initial_split(station_data, prop = 0.8)
    train_data <- training(data_split)
    test_data <- testing(data_split)
    
    # Prétraitement des données
    recipe <- recipe(et0 ~ tx + tn + rh + rs + ws, data = train_data) %>%
      step_normalize(all_predictors())
    
    # Préparer les formules de prétraitement et de modèle
    preproc_formula <- et0 ~ tx + tn + rh + rs + ws
    model_formula <- et0 ~ s(tx) + s(tn) + s(rh) + s(rs) + s(ws)
    
    # Définir le modèle GAM avec une formule
    gam_model <- gen_additive_mod(
      select_features = tune(),
      adjust_deg_free = tune()
    ) %>%
      set_engine("mgcv") %>%
      set_mode("regression")
    
    # Créer le workflow
    gam_workflow <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(gam_model, formula = model_formula)
    
    # Définir la grille de recherche pour les hyperparamètres
    gam_grid <- grid_regular(
      select_features(c(TRUE, FALSE)),
      adjust_deg_free(range = c(0.5, 3.0)),
      levels = 5
    )
    
    cat("Grid length:", nrow(gam_grid), "\n")
    
    # Validation croisée
    cv_folds <- vfold_cv(train_data, v = 5)
    
    # Entraîner et évaluer le modèle
    gam_fit <- tune_grid(
      gam_workflow,
      resamples = cv_folds,
      grid = gam_grid,
      metrics = metric_set(rmse),
      control = control_grid(save_pred = TRUE, parallel_over = "everything")
    )
    
    # Sauvegarder les résultats de la validation croisée
    saveRDS(gam_fit, file = paste0("cv_results_gam_", station, ".rds"))
    
    # Sélectionner le meilleur modèle
    best_params <- select_best(gam_fit, metric = "rmse")
    
    # Finaliser et ajuster le modèle final
    final_gam <- finalize_workflow(gam_workflow, best_params)
    gam_final_model <- fit(final_gam, data = train_data)
    
    # Faire des prédictions sur l'ensemble de test
    final_predictions <- predict(gam_final_model, test_data) %>%
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
    saveRDS(gam_final_model, file = paste0("gam_model_", station, ".rds"))
  }
  
  # Retourner les métriques de performance
  return(performance_metrics)
}

# Appliquer la fonction pour entraîner le modèle GAM
performance_gam <- train_gam_model(stations_data)

# Visualiser les performances du modèle à travers les stations
ggplot(performance_gam, aes(x = station, y = rmse)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "RMSE across Stations", x = "Station", y = "RMSE") +
  theme_minimal()

ggplot(performance_gam, aes(x = station, y = rsq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "R² across Stations", x = "Station", y = "R²") +
  theme_minimal()
