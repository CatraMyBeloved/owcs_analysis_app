best_params <- c5_tuning_results %>%
  select_best(metric = "accuracy")

final_workflow <- c5_workflow %>%
  finalize_workflow(best_params)

data_prep <- data_prep |>
  mutate(
    iswin = factor(iswin, levels = c("1", "0"))
  )

final_fitted_model <- final_workflow %>%
  fit(data = data_prep)

saveRDS(final_fitted_model, "models/c5_model_final.rds")
