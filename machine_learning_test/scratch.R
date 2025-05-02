best_params <- random_forest_tune_results %>%
  select_best(metric = "accuracy")

final_workflow <- workflow_random_forest %>%
  finalize_workflow(best_params)

data_prep <- data_prep |>
  mutate(
    iswin = factor(iswin, levels = c("1", "0"))
  )

final_fitted_model <- final_workflow %>%
  fit(data = data_prep)

saveRDS(final_fitted_model, "models/rand_forest_model_final.rds")
