library(tidymodels)
library(doParallel)

# Set up parallel processing
library(future)
plan(multisession, workers = 4)


data <- data_with_heroes_hashed |>
  select(
    -rowid, -round_id
  ) |>
  left_join(match_maps, by = "match_map_id") |>
  left_join(maps, by = "map_id") |>
  select(-match_map_id, -map_win_team_id, -match_id, -map_id, -mode)

labels <- data |> select(iswin)

predictors <- data |> select(-iswin)

data <- data |>
  mutate(iswin = as.factor(iswin))

recipe_pred <- recipe(iswin ~ ., data = data) |>
  step_zv() |>
  step_dummy_hash(all_nominal_predictors())

folds <- vfold_cv(data, v = 5, strata = iswin)

log_reg_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) |>
  set_engine("glmnet") |>
  set_mode("classification")

grid_vals_log_reg <- grid_regular(
  penalty(range = c(-5, 1), trans = log10_trans()),
  mixture(range = c(0, 1)),
  levels = c(10, 5)
)

log_reg_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(log_reg_spec)

log_reg_tune_results <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid_vals_log_reg,
  metrics = metric_set(roc_auc, accuracy)
)

log_reg_metrics <- collect_metrics(log_reg_tune_results)


grid_vals_rf <- grid_regular(
  trees(range = c(100, 1000)),
  finalize(mtry(), predictors),
  min_n(range = c(1, 10)),
  levels = 5
)

random_forest_spec <-
  rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

workflow_random_forest <- workflow() |>
  add_model(random_forest_spec) |>
  add_recipe(recipe_pred)

random_forest_tune_results <- tune_grid(
  workflow_random_forest,
  resamples = folds,
  grid = grid_vals_rf,
  metrics = metric_set(roc_auc, accuracy)
)

random_forest_metrics <- collect_metrics(random_forest_tune_results)
